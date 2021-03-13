/* Lower TLS operations to emulation functions.
   Copyright (C) 2006-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "varasm.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "gimplify.h"

/* Whenever a target does not support thread-local storage (TLS) natively,
   we can emulate it with some run-time support in libgcc.  This will in
   turn rely on "keyed storage" a-la pthread_key_create; essentially all
   thread libraries provide such functionality.

   In order to coordinate with the libgcc runtime, each TLS variable is
   described by a "control variable".  This control variable records the
   required size, alignment, and initial value of the TLS variable for
   instantiation at runtime.  It also stores an integer token to be used
   by the runtime to find the address of the variable within each thread.

   On the compiler side, this means that we need to replace all instances
   of "tls_var" in the code with "*__emutls_get_addr(&control_var)".  We
   also need to eliminate "tls_var" from the symbol table and introduce
   "control_var".

   We used to perform all of the transformations during conversion to rtl,
   and the variable substitutions magically within assemble_variable.
   However, this late fiddling of the symbol table conflicts with LTO and
   whole-program compilation.  Therefore we must now make all the changes
   to the symbol table early in the GIMPLE optimization path, before we
   write things out to LTO intermediate files.  */

/* Value for TLS varpool node where a pointer to control variable and
   access variable are stored.  */
struct tls_var_data
{
  varpool_node *control_var;
  tree access;
};

/* TLS map accesses mapping between a TLS varpool node and a pair
   made by control variable and access variable.  */
static hash_map<varpool_node *, tls_var_data> *tls_map = NULL;

/* The type of the control structure, shared with the emutls.c runtime.  */
static tree emutls_object_type;

#if !defined (NO_DOT_IN_LABEL)
# define EMUTLS_SEPARATOR	"."
#elif !defined (NO_DOLLAR_IN_LABEL)
# define EMUTLS_SEPARATOR	"$"
#else
# define EMUTLS_SEPARATOR	"_"
#endif

/* Create an IDENTIFIER_NODE by prefixing PREFIX to the
   IDENTIFIER_NODE NAME's name.  */

static tree
prefix_name (const char *prefix, tree name)
{
  unsigned plen = strlen (prefix);
  unsigned nlen = strlen (IDENTIFIER_POINTER (name));
  char *toname = (char *) alloca (plen + nlen + 1);

  memcpy (toname, prefix, plen);
  memcpy (toname + plen, IDENTIFIER_POINTER (name), nlen + 1);

  return get_identifier (toname);
}

/* Create an identifier for the struct __emutls_object, given an identifier
   of the DECL_ASSEMBLY_NAME of the original object.  */

static tree
get_emutls_object_name (tree name)
{
  const char *prefix = (targetm.emutls.var_prefix
			? targetm.emutls.var_prefix
			: "__emutls_v" EMUTLS_SEPARATOR);
  return prefix_name (prefix, name);
}

/* Create the fields of the type for the control variables.  Ordinarily
   this must match struct __emutls_object defined in emutls.c.  However
   this is a target hook so that VxWorks can define its own layout.  */

tree
default_emutls_var_fields (tree type, tree *name ATTRIBUTE_UNUSED)
{
  tree word_type_node, field, next_field;

  field = build_decl (UNKNOWN_LOCATION,
		      FIELD_DECL, get_identifier ("__templ"), ptr_type_node);
  DECL_CONTEXT (field) = type;
  next_field = field;

  field = build_decl (UNKNOWN_LOCATION,
		      FIELD_DECL, get_identifier ("__offset"),
		      ptr_type_node);
  DECL_CONTEXT (field) = type;
  DECL_CHAIN (field) = next_field;
  next_field = field;

  word_type_node = lang_hooks.types.type_for_mode (word_mode, 1);
  field = build_decl (UNKNOWN_LOCATION,
		      FIELD_DECL, get_identifier ("__align"),
		      word_type_node);
  DECL_CONTEXT (field) = type;
  DECL_CHAIN (field) = next_field;
  next_field = field;

  field = build_decl (UNKNOWN_LOCATION,
		      FIELD_DECL, get_identifier ("__size"), word_type_node);
  DECL_CONTEXT (field) = type;
  DECL_CHAIN (field) = next_field;

  return field;
}

/* Initialize emulated tls object TO, which refers to TLS variable DECL and
   is initialized by PROXY.  As above, this is the default implementation of
   a target hook overridden by VxWorks.  */

tree
default_emutls_var_init (tree to, tree decl, tree proxy)
{
  vec<constructor_elt, va_gc> *v;
  vec_alloc (v, 4);
  constructor_elt elt;
  tree type = TREE_TYPE (to);
  tree field = TYPE_FIELDS (type);

  elt.index = field;
  elt.value = fold_convert (TREE_TYPE (field), DECL_SIZE_UNIT (decl));
  v->quick_push (elt);

  field = DECL_CHAIN (field);
  elt.index = field;
  elt.value = build_int_cst (TREE_TYPE (field),
			     DECL_ALIGN_UNIT (decl));
  v->quick_push (elt);

  field = DECL_CHAIN (field);
  elt.index = field;
  elt.value = null_pointer_node;
  v->quick_push (elt);

  field = DECL_CHAIN (field);
  elt.index = field;
  elt.value = proxy;
  v->quick_push (elt);

  return build_constructor (type, v);
}

/* Create the structure for struct __emutls_object.  This should match the
   structure at the top of emutls.c, modulo the union there.  */

static tree
get_emutls_object_type (void)
{
  tree type, type_name, field;

  type = emutls_object_type;
  if (type)
    return type;

  emutls_object_type = type = lang_hooks.types.make_type (RECORD_TYPE);
  type_name = NULL;
  field = targetm.emutls.var_fields (type, &type_name);
  if (!type_name)
    type_name = get_identifier ("__emutls_object");
  type_name = build_decl (UNKNOWN_LOCATION,
			  TYPE_DECL, type_name, type);
  TYPE_NAME (type) = type_name;
  TYPE_FIELDS (type) = field;
  layout_type (type);

  return type;
}

/* Create a read-only variable like DECL, with the same DECL_INITIAL.
   This will be used for initializing the emulated tls data area.  */

static tree
get_emutls_init_templ_addr (tree decl)
{
  tree name, to;

  if (targetm.emutls.register_common && !DECL_INITIAL (decl)
      && !DECL_SECTION_NAME (decl))
    return null_pointer_node;

  name = DECL_ASSEMBLER_NAME (decl);
  if (!targetm.emutls.tmpl_prefix || targetm.emutls.tmpl_prefix[0])
    {
      const char *prefix = (targetm.emutls.tmpl_prefix
			    ? targetm.emutls.tmpl_prefix
			    : "__emutls_t" EMUTLS_SEPARATOR);
      name = prefix_name (prefix, name);
    }

  to = build_decl (DECL_SOURCE_LOCATION (decl),
		   VAR_DECL, name, TREE_TYPE (decl));
  SET_DECL_ASSEMBLER_NAME (to, DECL_NAME (to));

  DECL_ARTIFICIAL (to) = 1;
  TREE_USED (to) = TREE_USED (decl);
  TREE_READONLY (to) = 1;
  DECL_IGNORED_P (to) = 1;
  DECL_CONTEXT (to) = DECL_CONTEXT (decl);
  DECL_PRESERVE_P (to) = DECL_PRESERVE_P (decl);

  DECL_WEAK (to) = DECL_WEAK (decl);
  if (DECL_ONE_ONLY (decl) || DECL_WEAK (decl))
    {
      TREE_STATIC (to) = TREE_STATIC (decl);
      TREE_PUBLIC (to) = TREE_PUBLIC (decl);
      DECL_VISIBILITY (to) = DECL_VISIBILITY (decl);
    }
  else
    TREE_STATIC (to) = 1;

  if (DECL_ONE_ONLY (decl))
    make_decl_one_only (to, DECL_ASSEMBLER_NAME (to));

  DECL_VISIBILITY_SPECIFIED (to) = DECL_VISIBILITY_SPECIFIED (decl);
  DECL_INITIAL (to) = DECL_INITIAL (decl);
  DECL_INITIAL (decl) = NULL;

  if (targetm.emutls.tmpl_section)
    set_decl_section_name (to, targetm.emutls.tmpl_section);
  else
    set_decl_section_name (to, DECL_SECTION_NAME (decl));

  /* Create varpool node for the new variable and finalize it if it is
     not external one.  */
  if (DECL_EXTERNAL (to))
    varpool_node::get_create (to);
  else
    varpool_node::add (to);
  return build_fold_addr_expr (to);
}

/* Create and return the control variable for the TLS variable DECL.  */

static tree
new_emutls_decl (tree decl, tree alias_of)
{
  tree name, to;

  name = DECL_ASSEMBLER_NAME (decl);
  to = build_decl (DECL_SOURCE_LOCATION (decl), VAR_DECL,
                   get_emutls_object_name (name),
                   get_emutls_object_type ());

  SET_DECL_ASSEMBLER_NAME (to, DECL_NAME (to));

  DECL_ARTIFICIAL (to) = 1;
  DECL_IGNORED_P (to) = 1;
  TREE_READONLY (to) = 0;
  TREE_STATIC (to) = 1;

  DECL_PRESERVE_P (to) = DECL_PRESERVE_P (decl);
  DECL_CONTEXT (to) = DECL_CONTEXT (decl);
  TREE_USED (to) = TREE_USED (decl);
  TREE_PUBLIC (to) = TREE_PUBLIC (decl);
  DECL_EXTERNAL (to) = DECL_EXTERNAL (decl);
  DECL_COMMON (to) = DECL_COMMON (decl);
  DECL_WEAK (to) = DECL_WEAK (decl);
  DECL_VISIBILITY (to) = DECL_VISIBILITY (decl);
  DECL_VISIBILITY_SPECIFIED (to) = DECL_VISIBILITY_SPECIFIED (decl);
  DECL_DLLIMPORT_P (to) = DECL_DLLIMPORT_P (decl);

  DECL_ATTRIBUTES (to) = targetm.merge_decl_attributes (decl, to);

  if (DECL_ONE_ONLY (decl))
    make_decl_one_only (to, DECL_ASSEMBLER_NAME (to));

  set_decl_tls_model (to, TLS_MODEL_EMULATED);

  /* If we're not allowed to change the proxy object's alignment,
     pretend it has been set by the user.  */
  if (targetm.emutls.var_align_fixed)
    DECL_USER_ALIGN (to) = 1;

  /* If the target wants the control variables grouped, do so.  */
  if (!DECL_COMMON (to) && targetm.emutls.var_section)
    {
      set_decl_section_name (to, targetm.emutls.var_section);
    }

  /* If this variable is defined locally, then we need to initialize the
     control structure with size and alignment information.  Initialization
     of COMMON block variables happens elsewhere via a constructor.  */
  if (!DECL_EXTERNAL (to)
      && (!DECL_COMMON (to) || !targetm.emutls.register_common
          || (DECL_INITIAL (decl)
              && DECL_INITIAL (decl) != error_mark_node)))
    {
      tree tmpl = get_emutls_init_templ_addr (decl);
      DECL_INITIAL (to) = targetm.emutls.var_init (to, decl, tmpl);
      record_references_in_initializer (to, false);
    }

  /* Create varpool node for the new variable and finalize it if it is
     not external one.  */
  if (DECL_EXTERNAL (to))
    varpool_node::get_create (to);
  else if (!alias_of)
    varpool_node::add (to);
  else 
    {
      varpool_node *n;
      varpool_node *t = varpool_node::get_for_asmname
	 (DECL_ASSEMBLER_NAME (DECL_VALUE_EXPR (alias_of)));

      n = varpool_node::create_alias (to, t->decl);
      n->resolve_alias (t);
    }
  return to;
}

/* Generate a call statement to initialize CONTROL_DECL for TLS_DECL.
   This only needs to happen for TLS COMMON variables; non-COMMON
   variables can be initialized statically.  Insert the generated
   call statement at the end of PSTMTS.  */
   
static void
emutls_common_1 (tree tls_decl, tree control_decl, tree *pstmts)
{
  tree x;
  tree word_type_node;

  if (!DECL_COMMON (tls_decl) || !targetm.emutls.register_common
      || (DECL_INITIAL (tls_decl)
	  && DECL_INITIAL (tls_decl) != error_mark_node))
    return;

  word_type_node = lang_hooks.types.type_for_mode (word_mode, 1);

  x = build_call_expr (builtin_decl_explicit (BUILT_IN_EMUTLS_REGISTER_COMMON),
		       4, build_fold_addr_expr (control_decl),
		       fold_convert (word_type_node,
				     DECL_SIZE_UNIT (tls_decl)),
		       build_int_cst (word_type_node,
				      DECL_ALIGN_UNIT (tls_decl)),
		       get_emutls_init_templ_addr (tls_decl));

  append_to_statement_list (x, pstmts);
}

struct lower_emutls_data
{
  struct cgraph_node *cfun_node;
  struct cgraph_node *builtin_node;
  tree builtin_decl;
  basic_block bb;
  location_t loc;
  gimple_seq seq;
};

/* Given a TLS variable DECL, return an SSA_NAME holding its address.
   Append any new computation statements required to D->SEQ.  */

static tree
gen_emutls_addr (tree decl, struct lower_emutls_data *d)
{
  /* Compute the address of the TLS variable with help from runtime.  */
  tls_var_data *data = tls_map->get (varpool_node::get (decl));
  tree addr = data->access;

  if (addr == NULL)
    {
      varpool_node *cvar;
      tree cdecl;
      gcall *x;

      cvar = data->control_var;
      cdecl = cvar->decl;
      TREE_ADDRESSABLE (cdecl) = 1;

      addr = create_tmp_var (build_pointer_type (TREE_TYPE (decl)));
      x = gimple_build_call (d->builtin_decl, 1, build_fold_addr_expr (cdecl));
      gimple_set_location (x, d->loc);

      addr = make_ssa_name (addr, x);
      gimple_call_set_lhs (x, addr);

      gimple_seq_add_stmt (&d->seq, x);

      d->cfun_node->create_edge (d->builtin_node, x, d->bb->count);

      /* We may be adding a new reference to a new variable to the function.
         This means we have to play with the ipa-reference web.  */
      d->cfun_node->create_reference (cvar, IPA_REF_ADDR, x);

      /* Record this ssa_name for possible use later in the basic block.  */
      data->access = addr;
    }

  return addr;
}

/* Callback for lower_emutls_1, return non-NULL if there is any TLS
   VAR_DECL in the subexpressions.  */

static tree
lower_emutls_2 (tree *ptr, int *walk_subtrees, void *)
{
  tree t = *ptr;
  if (TREE_CODE (t) == VAR_DECL)
    return DECL_THREAD_LOCAL_P (t) ? t : NULL_TREE;
  else if (!EXPR_P (t))
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Callback for walk_gimple_op.  D = WI->INFO is a struct lower_emutls_data.
   Given an operand *PTR within D->STMT, if the operand references a TLS
   variable, then lower the reference to a call to the runtime.  Insert
   any new statements required into D->SEQ; the caller is responsible for
   placing those appropriately.  */

static tree
lower_emutls_1 (tree *ptr, int *walk_subtrees, void *cb_data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) cb_data;
  struct lower_emutls_data *d = (struct lower_emutls_data *) wi->info;
  tree t = *ptr;
  bool is_addr = false;
  tree addr;

  *walk_subtrees = 0;

  switch (TREE_CODE (t))
    {
    case ADDR_EXPR:
      /* If this is not a straight-forward "&var", but rather something
	 like "&var.a", then we may need special handling.  */
      if (TREE_CODE (TREE_OPERAND (t, 0)) != VAR_DECL)
	{
	  bool save_changed;

	  /* Gimple invariants are shareable trees, so before changing
	     anything in them if we will need to change anything, unshare
	     them.  */
	  if (is_gimple_min_invariant (t)
	      && walk_tree (&TREE_OPERAND (t, 0), lower_emutls_2, NULL, NULL))
	    *ptr = t = unshare_expr (t);

	  /* If we're allowed more than just is_gimple_val, continue.  */
	  if (!wi->val_only)
	    {
	      *walk_subtrees = 1;
	      return NULL_TREE;
	    }

	  /* See if any substitution would be made.  */
	  save_changed = wi->changed;
	  wi->changed = false;
	  wi->val_only = false;
	  walk_tree (&TREE_OPERAND (t, 0), lower_emutls_1, wi, NULL);
	  wi->val_only = true;

	  /* If so, then extract this entire sub-expression "&p->a" into a
	     new assignment statement, and substitute yet another SSA_NAME.  */
	  if (wi->changed)
	    {
	      gimple *x;

	      addr = create_tmp_var (TREE_TYPE (t));
	      x = gimple_build_assign (addr, t);
	      gimple_set_location (x, d->loc);

	      addr = make_ssa_name (addr, x);
	      gimple_assign_set_lhs (x, addr);

	      gimple_seq_add_stmt (&d->seq, x);

	      *ptr = addr;
	    }
	  else
	    wi->changed = save_changed;

	  return NULL_TREE;
	}

      t = TREE_OPERAND (t, 0);
      is_addr = true;
      /* FALLTHRU */

    case VAR_DECL:
      if (!DECL_THREAD_LOCAL_P (t))
	return NULL_TREE;
      break;

    default:
      /* We're not interested in other decls or types, only subexpressions.  */
      if (EXPR_P (t))
        *walk_subtrees = 1;
      /* FALLTHRU */

    case SSA_NAME:
      /* Special-case the return of SSA_NAME, since it's so common.  */
      return NULL_TREE;
    }

  addr = gen_emutls_addr (t, d);
  if (is_addr)
    {
      /* Replace "&var" with "addr" in the statement.  */
      *ptr = addr;
    }
  else
    {
      /* Replace "var" with "*addr" in the statement.  */
      t = build2 (MEM_REF, TREE_TYPE (t), addr,
	          build_int_cst (TREE_TYPE (addr), 0));
      *ptr = t;
    }

  wi->changed = true;
  return NULL_TREE;
}

/* Lower all of the operands of STMT.  */

static void
lower_emutls_stmt (gimple *stmt, struct lower_emutls_data *d)
{
  struct walk_stmt_info wi;

  d->loc = gimple_location (stmt);

  memset (&wi, 0, sizeof (wi));
  wi.info = d;
  wi.val_only = true;
  walk_gimple_op (stmt, lower_emutls_1, &wi);

  if (wi.changed)
    update_stmt (stmt);
}

/* Lower the I'th operand of PHI.  */

static void
lower_emutls_phi_arg (gphi *phi, unsigned int i,
		      struct lower_emutls_data *d)
{
  struct walk_stmt_info wi;
  struct phi_arg_d *pd = gimple_phi_arg (phi, i);

  /* Early out for a very common case we don't care about.  */
  if (TREE_CODE (pd->def) == SSA_NAME)
    return;

  d->loc = pd->locus;

  memset (&wi, 0, sizeof (wi));
  wi.info = d;
  wi.val_only = true;
  walk_tree (&pd->def, lower_emutls_1, &wi, NULL);

  /* For normal statements, we let update_stmt do its job.  But for phi
     nodes, we have to manipulate the immediate use list by hand.  */
  if (wi.changed)
    {
      gcc_assert (TREE_CODE (pd->def) == SSA_NAME);
      link_imm_use_stmt (&pd->imm_use, pd->def, phi);
    }
}

/* Reset access variable for a given TLS variable data DATA.  */

bool
reset_access (varpool_node * const &, tls_var_data *data, void *)
{
  data->access = NULL;

  return true;
}

/* Clear the access variables, in order to begin a new block.  */

static inline void
clear_access_vars (void)
{
  tls_map->traverse<void *, reset_access> (NULL);
}

/* Lower the entire function NODE.  */

static void
lower_emutls_function_body (struct cgraph_node *node)
{
  struct lower_emutls_data d;
  bool any_edge_inserts = false;

  push_cfun (DECL_STRUCT_FUNCTION (node->decl));

  d.cfun_node = node;
  d.builtin_decl = builtin_decl_explicit (BUILT_IN_EMUTLS_GET_ADDRESS);
  /* This is where we introduce the declaration to the IL and so we have to
     create a node for it.  */
  d.builtin_node = cgraph_node::get_create (d.builtin_decl);

  FOR_EACH_BB_FN (d.bb, cfun)
    {
      unsigned int i, nedge;

      /* Lower each of the PHI nodes of the block, as we may have 
	 propagated &tlsvar into a PHI argument.  These loops are
	 arranged so that we process each edge at once, and each
	 PHI argument for that edge.  */
      if (!gimple_seq_empty_p (phi_nodes (d.bb)))
	{
	  nedge = EDGE_COUNT (d.bb->preds);
	  for (i = 0; i < nedge; ++i)
	    {
	      edge e = EDGE_PRED (d.bb, i);

	      /* We can re-use any SSA_NAME created on this edge.  */
	      clear_access_vars ();
	      d.seq = NULL;

	      for (gphi_iterator gsi = gsi_start_phis (d.bb);
		   !gsi_end_p (gsi);
		   gsi_next (&gsi))
		lower_emutls_phi_arg (gsi.phi (), i, &d);

	      /* Insert all statements generated by all phi nodes for this
		 particular edge all at once.  */
	      if (d.seq)
		{
		  gsi_insert_seq_on_edge (e, d.seq);
		  any_edge_inserts = true;
		}
	    }
	}

      /* We can re-use any SSA_NAME created during this basic block.  */
      clear_access_vars ();

      /* Lower each of the statements of the block.  */
      for (gimple_stmt_iterator gsi = gsi_start_bb (d.bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
          d.seq = NULL;
	  lower_emutls_stmt (gsi_stmt (gsi), &d);

	  /* If any new statements were created, insert them immediately
	     before the first use.  This prevents variable lifetimes from
	     becoming unnecessarily long.  */
	  if (d.seq)
	    gsi_insert_seq_before (&gsi, d.seq, GSI_SAME_STMT);
	}
    }

  if (any_edge_inserts)
    gsi_commit_edge_inserts ();

  pop_cfun ();
}

/* Create emutls variable for VAR, DATA is pointer to static
   ctor body we can add constructors to.
   Callback for varpool_for_variable_and_aliases.  */

static bool
create_emultls_var (varpool_node *var, void *data)
{
  tree cdecl;
  tls_var_data value;

  cdecl = new_emutls_decl (var->decl,
			   var->alias && var->analyzed
			   ? var->get_alias_target ()->decl : NULL);

  varpool_node *cvar = varpool_node::get (cdecl);

  if (!var->alias)
    {
      /* Make sure the COMMON block control variable gets initialized.
	 Note that there's no point in doing this for aliases; we only
	 need to do this once for the main variable.  */
      emutls_common_1 (var->decl, cdecl, (tree *)data);
    }
  if (var->alias && !var->analyzed)
    cvar->alias = true;

  /* Indicate that the value of the TLS variable may be found elsewhere,
     preventing the variable from re-appearing in the GIMPLE.  We cheat
     and use the control variable here (rather than a full call_expr),
     which is special-cased inside the DWARF2 output routines.  */
  SET_DECL_VALUE_EXPR (var->decl, cdecl);
  DECL_HAS_VALUE_EXPR_P (var->decl) = 1;

  value.control_var = cvar;
  tls_map->put (var, value);

  return false;
}

/* Main entry point to the tls lowering pass.  */

static unsigned int
ipa_lower_emutls (void)
{
  varpool_node *var;
  cgraph_node *func;
  bool any_aliases = false;
  tree ctor_body = NULL;
  hash_set <varpool_node *> visited;
  auto_vec <varpool_node *> tls_vars;

  /* Examine all global variables for TLS variables.  */
  FOR_EACH_VARIABLE (var)
    if (DECL_THREAD_LOCAL_P (var->decl)
	&& !visited.add (var))
      {
	gcc_checking_assert (TREE_STATIC (var->decl)
			     || DECL_EXTERNAL (var->decl));
	tls_vars.safe_push (var);
	if (var->alias && var->definition
	    && !visited.add (var->ultimate_alias_target ()))
	  tls_vars.safe_push (var->ultimate_alias_target ());
      }

  /* If we found no TLS variables, then there is no further work to do.  */
  if (tls_vars.is_empty ())
    {
      if (dump_file)
	fprintf (dump_file, "No TLS variables found.\n");
      return 0;
    }

  tls_map = new hash_map <varpool_node *, tls_var_data> ();

  /* Create the control variables for each TLS variable.  */
  for (unsigned i = 0; i < tls_vars.length (); i++)
    {
      var = tls_vars[i];

      if (var->alias && !var->analyzed)
	any_aliases = true;
      else if (!var->alias)
	var->call_for_symbol_and_aliases (create_emultls_var, &ctor_body, true);
    }

  /* If there were any aliases, then frob the alias_pairs vector.  */
  if (any_aliases)
    {
      alias_pair *p;
      unsigned int i;
      FOR_EACH_VEC_SAFE_ELT (alias_pairs, i, p)
	if (DECL_THREAD_LOCAL_P (p->decl))
	  {
	    p->decl = tls_map->get
	      (varpool_node::get (p->decl))->control_var->decl;
	    p->target = get_emutls_object_name (p->target);
	  }
    }

  /* Adjust all uses of TLS variables within the function bodies.  */
  FOR_EACH_DEFINED_FUNCTION (func)
    if (func->lowered)
      lower_emutls_function_body (func);

  /* Generate the constructor for any COMMON control variables created.  */
  if (ctor_body)
    cgraph_build_static_cdtor ('I', ctor_body, DEFAULT_INIT_PRIORITY);

  delete tls_map;

  return 0;
}

namespace {

const pass_data pass_data_ipa_lower_emutls =
{
  SIMPLE_IPA_PASS, /* type */
  "emutls", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_OPT, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_lower_emutls : public simple_ipa_opt_pass
{
public:
  pass_ipa_lower_emutls (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_lower_emutls, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      /* If the target supports TLS natively, we need do nothing here.  */
      return !targetm.have_tls;
    }

  virtual unsigned int execute (function *) { return ipa_lower_emutls (); }

}; // class pass_ipa_lower_emutls

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_lower_emutls (gcc::context *ctxt)
{
  return new pass_ipa_lower_emutls (ctxt);
}
