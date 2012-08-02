/* Mudflap: narrow-pointer bounds-checking by tree rewriting.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2012
   Free Software Foundation, Inc.
   Contributed by Frank Ch. Eigler <fche@redhat.com>
   and Graydon Hoare <graydon@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "flags.h"
#include "function.h"
#include "tree-inline.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-flow.h"
#include "tree-mudflap.h"
#include "tree-pass.h"
#include "hashtab.h"
#include "diagnostic.h"
#include "demangle.h"
#include "langhooks.h"
#include "ggc.h"
#include "cgraph.h"
#include "gimple.h"

extern void add_bb_to_loop (basic_block, struct loop *);

/* Internal function decls */


/* Options.  */
#define flag_mudflap_threads (flag_mudflap == 2)

/* Helpers.  */
static tree mf_build_string (const char *string);
static tree mf_varname_tree (tree);
static tree mf_file_function_line_tree (location_t);

/* Indirection-related instrumentation.  */
static void mf_decl_cache_locals (void);
static void mf_decl_clear_locals (void);
static void mf_xform_statements (void);
static unsigned int execute_mudflap_function_ops (void);

/* Addressable variables instrumentation.  */
static void mf_xform_decls (gimple_seq, tree);
static tree mx_xfn_xform_decls (gimple_stmt_iterator *, bool *,
				struct walk_stmt_info *);
static gimple_seq mx_register_decls (tree, gimple_seq, location_t);
static unsigned int execute_mudflap_function_decls (void);

/* Return true if DECL is artificial stub that shouldn't be instrumented by
   mf.  We should instrument clones of non-artificial functions.  */
static inline bool
mf_artificial (const_tree decl)
{
  return DECL_ARTIFICIAL (DECL_ORIGIN (decl));
}

/* ------------------------------------------------------------------------ */
/* Some generally helpful functions for mudflap instrumentation.  */

/* Build a reference to a literal string.  */
static tree
mf_build_string (const char *string)
{
  size_t len = strlen (string);
  tree result = mf_mark (build_string (len + 1, string));

  TREE_TYPE (result) = build_array_type
    (char_type_node, build_index_type (size_int (len)));
  TREE_CONSTANT (result) = 1;
  TREE_READONLY (result) = 1;
  TREE_STATIC (result) = 1;

  result = build1 (ADDR_EXPR, build_pointer_type (char_type_node), result);

  return mf_mark (result);
}

/* Create a properly typed STRING_CST node that describes the given
   declaration.  It will be used as an argument for __mf_register().
   Try to construct a helpful string, including file/function/variable
   name.  */

static tree
mf_varname_tree (tree decl)
{
  static pretty_printer buf_rec;
  static int initialized = 0;
  pretty_printer *buf = & buf_rec;
  const char *buf_contents;
  tree result;

  gcc_assert (decl);

  if (!initialized)
    {
      pp_construct (buf, /* prefix */ NULL, /* line-width */ 0);
      initialized = 1;
    }
  pp_clear_output_area (buf);

  /* Add FILENAME[:LINENUMBER[:COLUMNNUMBER]].  */
  {
    expanded_location xloc = expand_location (DECL_SOURCE_LOCATION (decl));
    const char *sourcefile;
    unsigned sourceline = xloc.line;
    unsigned sourcecolumn = 0;
    sourcecolumn = xloc.column;
    sourcefile = xloc.file;
    if (sourcefile == NULL && current_function_decl != NULL_TREE)
      sourcefile = DECL_SOURCE_FILE (current_function_decl);
    if (sourcefile == NULL)
      sourcefile = "<unknown file>";

    pp_string (buf, sourcefile);

    if (sourceline != 0)
      {
        pp_string (buf, ":");
        pp_decimal_int (buf, sourceline);

        if (sourcecolumn != 0)
          {
            pp_string (buf, ":");
            pp_decimal_int (buf, sourcecolumn);
          }
      }
  }

  if (current_function_decl != NULL_TREE)
    {
      /* Add (FUNCTION) */
      pp_string (buf, " (");
      {
        const char *funcname = NULL;
        if (DECL_NAME (current_function_decl))
          funcname = lang_hooks.decl_printable_name (current_function_decl, 1);
        if (funcname == NULL)
          funcname = "anonymous fn";

        pp_string (buf, funcname);
      }
      pp_string (buf, ") ");
    }
  else
    pp_string (buf, " ");

  /* Add <variable-declaration>, possibly demangled.  */
  {
    const char *declname = NULL;

    if (DECL_NAME (decl) != NULL)
      {
	if (strcmp ("GNU C++", lang_hooks.name) == 0)
	  {
	    /* The gcc/cp decl_printable_name hook doesn't do as good a job as
	       the libiberty demangler.  */
	    declname = cplus_demangle (IDENTIFIER_POINTER (DECL_NAME (decl)),
				       DMGL_AUTO | DMGL_VERBOSE);
	  }
	if (declname == NULL)
	  declname = lang_hooks.decl_printable_name (decl, 3);
      }
    if (declname == NULL)
      declname = "<unnamed variable>";

    pp_string (buf, declname);
  }

  /* Return the lot as a new STRING_CST.  */
  buf_contents = pp_base_formatted_text (buf);
  result = mf_build_string (buf_contents);
  pp_clear_output_area (buf);

  return result;
}


/* And another friend, for producing a simpler message.  */

static tree
mf_file_function_line_tree (location_t location)
{
  expanded_location xloc = expand_location (location);
  const char *file = NULL, *colon, *line, *op, *name, *cp;
  char linecolbuf[30]; /* Enough for two decimal numbers plus a colon.  */
  char *string;
  tree result;

  /* Add FILENAME[:LINENUMBER[:COLUMNNUMBER]].  */
  file = xloc.file;
  if (file == NULL && current_function_decl != NULL_TREE)
    file = DECL_SOURCE_FILE (current_function_decl);
  if (file == NULL)
    file = "<unknown file>";

  if (xloc.line > 0)
    {
      if (xloc.column > 0)
        sprintf (linecolbuf, "%d:%d", xloc.line, xloc.column);
      else
        sprintf (linecolbuf, "%d", xloc.line);
      colon = ":";
      line = linecolbuf;
    }
  else
    colon = line = "";

  /* Add (FUNCTION).  */
  name = lang_hooks.decl_printable_name (current_function_decl, 1);
  if (name)
    {
      op = " (";
      cp = ")";
    }
  else
    op = name = cp = "";

  string = concat (file, colon, line, op, name, cp, NULL);
  result = mf_build_string (string);
  free (string);

  return result;
}


/* global tree nodes */

/* Global tree objects for global variables and functions exported by
   mudflap runtime library.  mf_init_extern_trees must be called
   before using these.  */

/* uintptr_t (usually "unsigned long") */
static GTY (()) tree mf_uintptr_type;

/* struct __mf_cache { uintptr_t low; uintptr_t high; }; */
static GTY (()) tree mf_cache_struct_type;

/* struct __mf_cache * const */
static GTY (()) tree mf_cache_structptr_type;

/* extern struct __mf_cache __mf_lookup_cache []; */
static GTY (()) tree mf_cache_array_decl;

/* extern unsigned char __mf_lc_shift; */
static GTY (()) tree mf_cache_shift_decl;

/* extern uintptr_t __mf_lc_mask; */
static GTY (()) tree mf_cache_mask_decl;

/* Their function-scope local shadows, used in single-threaded mode only.  */

/* auto const unsigned char __mf_lc_shift_l; */
static GTY (()) tree mf_cache_shift_decl_l;

/* auto const uintptr_t __mf_lc_mask_l; */
static GTY (()) tree mf_cache_mask_decl_l;

/* extern void __mf_check (void *ptr, size_t sz, int type, const char *); */
static GTY (()) tree mf_check_fndecl;

/* extern void __mf_register (void *ptr, size_t sz, int type, const char *); */
static GTY (()) tree mf_register_fndecl;

/* extern void __mf_unregister (void *ptr, size_t sz, int type); */
static GTY (()) tree mf_unregister_fndecl;

/* extern void __mf_init (); */
static GTY (()) tree mf_init_fndecl;

/* extern int __mf_set_options (const char*); */
static GTY (()) tree mf_set_options_fndecl;


/* Helper for mudflap_init: construct a decl with the given category,
   name, and type, mark it an external reference, and pushdecl it.  */
static inline tree
mf_make_builtin (enum tree_code category, const char *name, tree type)
{
  tree decl = mf_mark (build_decl (UNKNOWN_LOCATION,
				   category, get_identifier (name), type));
  TREE_PUBLIC (decl) = 1;
  DECL_EXTERNAL (decl) = 1;
  lang_hooks.decls.pushdecl (decl);
  /* The decl was declared by the compiler.  */
  DECL_ARTIFICIAL (decl) = 1;
  /* And we don't want debug info for it.  */
  DECL_IGNORED_P (decl) = 1;
  return decl;
}

/* Helper for mudflap_init: construct a tree corresponding to the type
     struct __mf_cache { uintptr_t low; uintptr_t high; };
     where uintptr_t is the FIELD_TYPE argument.  */
static inline tree
mf_make_mf_cache_struct_type (tree field_type)
{
  /* There is, abominably, no language-independent way to construct a
     RECORD_TYPE.  So we have to call the basic type construction
     primitives by hand.  */
  tree fieldlo = build_decl (UNKNOWN_LOCATION,
			     FIELD_DECL, get_identifier ("low"), field_type);
  tree fieldhi = build_decl (UNKNOWN_LOCATION,
			     FIELD_DECL, get_identifier ("high"), field_type);

  tree struct_type = make_node (RECORD_TYPE);
  DECL_CONTEXT (fieldlo) = struct_type;
  DECL_CONTEXT (fieldhi) = struct_type;
  DECL_CHAIN (fieldlo) = fieldhi;
  TYPE_FIELDS (struct_type) = fieldlo;
  TYPE_NAME (struct_type) = get_identifier ("__mf_cache");
  layout_type (struct_type);

  return struct_type;
}

/* Initialize the global tree nodes that correspond to mf-runtime.h
   declarations.  */
void
mudflap_init (void)
{
  static bool done = false;
  tree mf_const_string_type;
  tree mf_cache_array_type;
  tree mf_check_register_fntype;
  tree mf_unregister_fntype;
  tree mf_init_fntype;
  tree mf_set_options_fntype;

  if (done)
    return;
  done = true;

  mf_uintptr_type = lang_hooks.types.type_for_mode (ptr_mode,
                                                    /*unsignedp=*/true);
  mf_const_string_type
    = build_pointer_type (build_qualified_type
                          (char_type_node, TYPE_QUAL_CONST));

  mf_cache_struct_type = mf_make_mf_cache_struct_type (mf_uintptr_type);
  mf_cache_structptr_type = build_pointer_type (mf_cache_struct_type);
  mf_cache_array_type = build_array_type (mf_cache_struct_type, 0);
  mf_check_register_fntype =
    build_function_type_list (void_type_node, ptr_type_node, size_type_node,
			      integer_type_node, mf_const_string_type, NULL_TREE);
  mf_unregister_fntype =
    build_function_type_list (void_type_node, ptr_type_node, size_type_node,
			      integer_type_node, NULL_TREE);
  mf_init_fntype =
    build_function_type_list (void_type_node, NULL_TREE);
  mf_set_options_fntype =
    build_function_type_list (integer_type_node, mf_const_string_type, NULL_TREE);

  mf_cache_array_decl = mf_make_builtin (VAR_DECL, "__mf_lookup_cache",
                                         mf_cache_array_type);
  mf_cache_shift_decl = mf_make_builtin (VAR_DECL, "__mf_lc_shift",
                                         unsigned_char_type_node);
  mf_cache_mask_decl = mf_make_builtin (VAR_DECL, "__mf_lc_mask",
                                        mf_uintptr_type);
  /* Don't process these in mudflap_enqueue_decl, should they come by
     there for some reason.  */
  mf_mark (mf_cache_array_decl);
  mf_mark (mf_cache_shift_decl);
  mf_mark (mf_cache_mask_decl);
  mf_check_fndecl = mf_make_builtin (FUNCTION_DECL, "__mf_check",
                                     mf_check_register_fntype);
  mf_register_fndecl = mf_make_builtin (FUNCTION_DECL, "__mf_register",
                                        mf_check_register_fntype);
  mf_unregister_fndecl = mf_make_builtin (FUNCTION_DECL, "__mf_unregister",
                                          mf_unregister_fntype);
  mf_init_fndecl = mf_make_builtin (FUNCTION_DECL, "__mf_init",
                                    mf_init_fntype);
  mf_set_options_fndecl = mf_make_builtin (FUNCTION_DECL, "__mf_set_options",
                                           mf_set_options_fntype);
}


/* ------------------------------------------------------------------------ */
/* This is the second part of the mudflap instrumentation.  It works on
   low-level GIMPLE using the CFG, because we want to run this pass after
   tree optimizations have been performed, but we have to preserve the CFG
   for expansion from trees to RTL.
   Below is the list of transformations performed on statements in the
   current function.

 1)  Memory reference transforms: Perform the mudflap indirection-related
    tree transforms on memory references.

 2) Mark BUILTIN_ALLOCA calls not inlineable.

 */

static unsigned int
execute_mudflap_function_ops (void)
{
  struct gimplify_ctx gctx;

  /* Don't instrument functions such as the synthetic constructor
     built during mudflap_finish_file.  */
  if (mf_marked_p (current_function_decl)
      || mf_artificial (current_function_decl))
    return 0;

  push_gimplify_context (&gctx);

  /* In multithreaded mode, don't cache the lookup cache parameters.  */
  if (! flag_mudflap_threads)
    mf_decl_cache_locals ();

  mf_xform_statements ();

  if (! flag_mudflap_threads)
    mf_decl_clear_locals ();

  pop_gimplify_context (NULL);
  return 0;
}

/* Insert a gimple_seq SEQ on all the outgoing edges out of BB.  Note that
   if BB has more than one edge, STMT will be replicated for each edge.
   Also, abnormal edges will be ignored.  */

static void
insert_edge_copies_seq (gimple_seq seq, basic_block bb)
{
  edge e;
  edge_iterator ei;
  unsigned n_copies = -1;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & EDGE_ABNORMAL))
      n_copies++;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & EDGE_ABNORMAL))
      gsi_insert_seq_on_edge (e, n_copies-- > 0 ? gimple_seq_copy (seq) : seq);
}

/* Create and initialize local shadow variables for the lookup cache
   globals.  Put their decls in the *_l globals for use by
   mf_build_check_statement_for.  */

static void
mf_decl_cache_locals (void)
{
  gimple g;
  gimple_seq seq = NULL;

  /* Build the cache vars.  */
  mf_cache_shift_decl_l
    = mf_mark (create_tmp_reg (TREE_TYPE (mf_cache_shift_decl),
                               "__mf_lookup_shift_l"));

  mf_cache_mask_decl_l
    = mf_mark (create_tmp_reg (TREE_TYPE (mf_cache_mask_decl),
                               "__mf_lookup_mask_l"));

  /* Build initialization nodes for the cache vars.  We just load the
     globals into the cache variables.  */
  g = gimple_build_assign (mf_cache_shift_decl_l, mf_cache_shift_decl);
  gimple_set_location (g, DECL_SOURCE_LOCATION (current_function_decl));
  gimple_seq_add_stmt (&seq, g);

  g = gimple_build_assign (mf_cache_mask_decl_l, mf_cache_mask_decl);
  gimple_set_location (g, DECL_SOURCE_LOCATION (current_function_decl));
  gimple_seq_add_stmt (&seq, g);

  insert_edge_copies_seq (seq, ENTRY_BLOCK_PTR);

  gsi_commit_edge_inserts ();
}


static void
mf_decl_clear_locals (void)
{
  /* Unset local shadows.  */
  mf_cache_shift_decl_l = NULL_TREE;
  mf_cache_mask_decl_l = NULL_TREE;
}

static void
mf_build_check_statement_for (tree base, tree limit,
                              gimple_stmt_iterator *instr_gsi,
                              location_t location, tree dirflag)
{
  gimple_stmt_iterator gsi;
  basic_block cond_bb, then_bb, join_bb;
  edge e;
  tree cond, t, u, v;
  tree mf_base;
  tree mf_elem;
  tree mf_limit;
  gimple g;
  gimple_seq seq, stmts;

  /* We first need to split the current basic block, and start altering
     the CFG.  This allows us to insert the statements we're about to
     construct into the right basic blocks.  */

  cond_bb = gimple_bb (gsi_stmt (*instr_gsi));
  gsi = *instr_gsi;
  gsi_prev (&gsi);
  if (! gsi_end_p (gsi))
    e = split_block (cond_bb, gsi_stmt (gsi));
  else
    e = split_block_after_labels (cond_bb);
  cond_bb = e->src;
  join_bb = e->dest;

  /* A recap at this point: join_bb is the basic block at whose head
     is the gimple statement for which this check expression is being
     built.  cond_bb is the (possibly new, synthetic) basic block the
     end of which will contain the cache-lookup code, and a
     conditional that jumps to the cache-miss code or, much more
     likely, over to join_bb.  */

  /* Create the bb that contains the cache-miss fallback block (mf_check).  */
  then_bb = create_empty_bb (cond_bb);
  make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
  make_single_succ_edge (then_bb, join_bb, EDGE_FALLTHRU);

  /* Mark the pseudo-fallthrough edge from cond_bb to join_bb.  */
  e = find_edge (cond_bb, join_bb);
  e->flags = EDGE_FALSE_VALUE;
  e->count = cond_bb->count;
  e->probability = REG_BR_PROB_BASE;

  /* Update dominance info.  Note that bb_join's data was
     updated by split_block.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);
      set_immediate_dominator (CDI_DOMINATORS, join_bb, cond_bb);
    }

  /* Update loop info.  */
  if (current_loops)
    add_bb_to_loop (then_bb, cond_bb->loop_father);

  /* Build our local variables.  */
  mf_elem = create_tmp_reg (mf_cache_structptr_type, "__mf_elem");
  mf_base = create_tmp_reg (mf_uintptr_type, "__mf_base");
  mf_limit = create_tmp_reg (mf_uintptr_type, "__mf_limit");

  /* Build: __mf_base = (uintptr_t) <base address expression>.  */
  seq = NULL;
  t = fold_convert_loc (location, mf_uintptr_type,
			unshare_expr (base));
  t = force_gimple_operand (t, &stmts, false, NULL_TREE);
  gimple_seq_add_seq (&seq, stmts);
  g = gimple_build_assign (mf_base, t);
  gimple_set_location (g, location);
  gimple_seq_add_stmt (&seq, g);

  /* Build: __mf_limit = (uintptr_t) <limit address expression>.  */
  t = fold_convert_loc (location, mf_uintptr_type,
			unshare_expr (limit));
  t = force_gimple_operand (t, &stmts, false, NULL_TREE);
  gimple_seq_add_seq (&seq, stmts);
  g = gimple_build_assign (mf_limit, t);
  gimple_set_location (g, location);
  gimple_seq_add_stmt (&seq, g);

  /* Build: __mf_elem = &__mf_lookup_cache [(__mf_base >> __mf_shift)
                                            & __mf_mask].  */
  t = build2 (RSHIFT_EXPR, mf_uintptr_type, mf_base,
              flag_mudflap_threads ? mf_cache_shift_decl
	       : mf_cache_shift_decl_l);
  t = build2 (BIT_AND_EXPR, mf_uintptr_type, t,
              flag_mudflap_threads ? mf_cache_mask_decl
	       : mf_cache_mask_decl_l);
  t = build4 (ARRAY_REF,
              TREE_TYPE (TREE_TYPE (mf_cache_array_decl)),
              mf_cache_array_decl, t, NULL_TREE, NULL_TREE);
  t = build1 (ADDR_EXPR, mf_cache_structptr_type, t);
  t = force_gimple_operand (t, &stmts, false, NULL_TREE);
  gimple_seq_add_seq (&seq, stmts);
  g = gimple_build_assign (mf_elem, t);
  gimple_set_location (g, location);
  gimple_seq_add_stmt (&seq, g);

  /* Quick validity check.

     if (__mf_elem->low > __mf_base
         || (__mf_elem_high < __mf_limit))
        {
          __mf_check ();
          ... and only if single-threaded:
          __mf_lookup_shift_1 = f...;
          __mf_lookup_mask_l = ...;
        }

     It is expected that this body of code is rarely executed so we mark
     the edge to the THEN clause of the conditional jump as unlikely.  */

  /* Construct t <-- '__mf_elem->low  > __mf_base'.  */
  t = build3 (COMPONENT_REF, mf_uintptr_type,
              build1 (INDIRECT_REF, mf_cache_struct_type, mf_elem),
              TYPE_FIELDS (mf_cache_struct_type), NULL_TREE);
  t = build2 (GT_EXPR, boolean_type_node, t, mf_base);

  /* Construct '__mf_elem->high < __mf_limit'.

     First build:
        1) u <--  '__mf_elem->high'
        2) v <--  '__mf_limit'.

     Then build 'u <-- (u < v).  */

  u = build3 (COMPONENT_REF, mf_uintptr_type,
              build1 (INDIRECT_REF, mf_cache_struct_type, mf_elem),
              DECL_CHAIN (TYPE_FIELDS (mf_cache_struct_type)), NULL_TREE);

  v = mf_limit;

  u = build2 (LT_EXPR, boolean_type_node, u, v);

  /* Build the composed conditional: t <-- 't || u'.  Then store the
     result of the evaluation of 't' in a temporary variable which we
     can use as the condition for the conditional jump.  */
  t = build2 (TRUTH_OR_EXPR, boolean_type_node, t, u);
  t = force_gimple_operand (t, &stmts, false, NULL_TREE);
  gimple_seq_add_seq (&seq, stmts);
  cond = create_tmp_reg (boolean_type_node, "__mf_unlikely_cond");
  g = gimple_build_assign  (cond, t);
  gimple_set_location (g, location);
  gimple_seq_add_stmt (&seq, g);

  /* Build the conditional jump.  'cond' is just a temporary so we can
     simply build a void COND_EXPR.  We do need labels in both arms though.  */
  g = gimple_build_cond (NE_EXPR, cond, boolean_false_node, NULL_TREE,
			 NULL_TREE);
  gimple_set_location (g, location);
  gimple_seq_add_stmt (&seq, g);

  /* At this point, after so much hard work, we have only constructed
     the conditional jump,

     if (__mf_elem->low > __mf_base
         || (__mf_elem_high < __mf_limit))

     The lowered GIMPLE tree representing this code is in the statement
     list starting at 'head'.

     We can insert this now in the current basic block, i.e. the one that
     the statement we're instrumenting was originally in.  */
  gsi = gsi_last_bb (cond_bb);
  gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);

  /*  Now build up the body of the cache-miss handling:

     __mf_check();
     refresh *_l vars.

     This is the body of the conditional.  */

  seq = NULL;
  /* u is a string, so it is already a gimple value.  */
  u = mf_file_function_line_tree (location);
  /* NB: we pass the overall [base..limit] range to mf_check.  */
  v = fold_build2_loc (location, PLUS_EXPR, mf_uintptr_type,
		   fold_build2_loc (location,
				MINUS_EXPR, mf_uintptr_type, mf_limit, mf_base),
		   build_int_cst (mf_uintptr_type, 1));
  v = force_gimple_operand (v, &stmts, true, NULL_TREE);
  gimple_seq_add_seq (&seq, stmts);
  g = gimple_build_call (mf_check_fndecl, 4, mf_base, v, dirflag, u);
  gimple_seq_add_stmt (&seq, g);

  if (! flag_mudflap_threads)
    {
      if (stmt_ends_bb_p (g))
	{
	  gsi = gsi_start_bb (then_bb);
	  gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);
	  e = split_block (then_bb, g);
	  then_bb = e->dest;
	  seq = NULL;
	}

      g = gimple_build_assign (mf_cache_shift_decl_l, mf_cache_shift_decl);
      gimple_seq_add_stmt (&seq, g);

      g = gimple_build_assign (mf_cache_mask_decl_l, mf_cache_mask_decl);
      gimple_seq_add_stmt (&seq, g);
    }

  /* Insert the check code in the THEN block.  */
  gsi = gsi_start_bb (then_bb);
  gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);

  *instr_gsi = gsi_start_bb (join_bb);
}


/* Check whether the given decl, generally a VAR_DECL or PARM_DECL, is
   eligible for instrumentation.  For the mudflap1 pass, this implies
   that it should be registered with the libmudflap runtime.  For the
   mudflap2 pass this means instrumenting an indirection operation with
   respect to the object.
*/
static int
mf_decl_eligible_p (tree decl)
{
  return ((TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == PARM_DECL)
          /* The decl must have its address taken.  In the case of
             arrays, this flag is also set if the indexes are not
             compile-time known valid constants.  */
	  /* XXX: not sufficient: return-by-value structs! */
          && TREE_ADDRESSABLE (decl)
          /* The type of the variable must be complete.  */
          && COMPLETE_OR_VOID_TYPE_P (TREE_TYPE (decl))
	  /* The decl hasn't been decomposed somehow.  */
	  && !DECL_HAS_VALUE_EXPR_P (decl));
}


static void
mf_xform_derefs_1 (gimple_stmt_iterator *iter, tree *tp,
                   location_t location, tree dirflag)
{
  tree type, base, limit, addr, size, t;

  /* Don't instrument read operations.  */
  if (dirflag == integer_zero_node && flag_mudflap_ignore_reads)
    return;

  /* Don't instrument marked nodes.  */
  if (mf_marked_p (*tp))
    return;

  t = *tp;
  type = TREE_TYPE (t);

  if (type == error_mark_node)
    return;

  size = TYPE_SIZE_UNIT (type);

  switch (TREE_CODE (t))
    {
    case ARRAY_REF:
    case COMPONENT_REF:
      {
        /* This is trickier than it may first appear.  The reason is
           that we are looking at expressions from the "inside out" at
           this point.  We may have a complex nested aggregate/array
           expression (e.g. "a.b[i].c"), maybe with an indirection as
           the leftmost operator ("p->a.b.d"), where instrumentation
           is necessary.  Or we may have an innocent "a.b.c"
           expression that must not be instrumented.  We need to
           recurse all the way down the nesting structure to figure it
           out: looking just at the outer node is not enough.  */
        tree var;
        int component_ref_only = (TREE_CODE (t) == COMPONENT_REF);
	/* If we have a bitfield component reference, we must note the
	   innermost addressable object in ELT, from which we will
	   construct the byte-addressable bounds of the bitfield.  */
	tree elt = NULL_TREE;
	int bitfield_ref_p = (TREE_CODE (t) == COMPONENT_REF
			      && DECL_BIT_FIELD_TYPE (TREE_OPERAND (t, 1)));

        /* Iterate to the top of the ARRAY_REF/COMPONENT_REF
           containment hierarchy to find the outermost VAR_DECL.  */
        var = TREE_OPERAND (t, 0);
        while (1)
          {
	    if (bitfield_ref_p && elt == NULL_TREE
		&& (TREE_CODE (var) == ARRAY_REF
		    || TREE_CODE (var) == COMPONENT_REF))
	      elt = var;

            if (TREE_CODE (var) == ARRAY_REF)
              {
                component_ref_only = 0;
                var = TREE_OPERAND (var, 0);
              }
            else if (TREE_CODE (var) == COMPONENT_REF)
              var = TREE_OPERAND (var, 0);
            else if (INDIRECT_REF_P (var)
		     || TREE_CODE (var) == MEM_REF)
              {
		base = TREE_OPERAND (var, 0);
                break;
              }
            else if (TREE_CODE (var) == VIEW_CONVERT_EXPR)
	      {
		var = TREE_OPERAND (var, 0);
		if (CONSTANT_CLASS_P (var)
		    && TREE_CODE (var) != STRING_CST)
		  return;
	      }
            else
              {
                gcc_assert (TREE_CODE (var) == VAR_DECL
                            || TREE_CODE (var) == PARM_DECL
                            || TREE_CODE (var) == RESULT_DECL
                            || TREE_CODE (var) == STRING_CST);
                /* Don't instrument this access if the underlying
                   variable is not "eligible".  This test matches
                   those arrays that have only known-valid indexes,
                   and thus are not labeled TREE_ADDRESSABLE.  */
                if (! mf_decl_eligible_p (var) || component_ref_only)
                  return;
                else
		  {
		    base = build1 (ADDR_EXPR,
				   build_pointer_type (TREE_TYPE (var)), var);
		    break;
		  }
              }
          }

        /* Handle the case of ordinary non-indirection structure
           accesses.  These have only nested COMPONENT_REF nodes (no
           INDIRECT_REF), but pass through the above filter loop.
           Note that it's possible for such a struct variable to match
           the eligible_p test because someone else might take its
           address sometime.  */

        /* We need special processing for bitfield components, because
           their addresses cannot be taken.  */
        if (bitfield_ref_p)
          {
            tree field = TREE_OPERAND (t, 1);

            if (TREE_CODE (DECL_SIZE_UNIT (field)) == INTEGER_CST)
              size = DECL_SIZE_UNIT (field);

	    if (elt)
	      elt = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (elt)),
			    elt);
            addr = fold_convert_loc (location, ptr_type_node, elt ? elt : base);
            addr = fold_build_pointer_plus_loc (location,
						addr, byte_position (field));
          }
        else
          addr = build1 (ADDR_EXPR, build_pointer_type (type), t);

        limit = fold_build2_loc (location, MINUS_EXPR, mf_uintptr_type,
                             fold_build2_loc (location, PLUS_EXPR, mf_uintptr_type,
					  fold_convert (mf_uintptr_type, addr),
					  size),
                             integer_one_node);
      }
      break;

    case INDIRECT_REF:
      addr = TREE_OPERAND (t, 0);
      base = addr;
      limit = fold_build_pointer_plus_hwi_loc
	(location, fold_build_pointer_plus_loc (location, base, size), -1);
      break;

    case MEM_REF:
      addr = fold_build_pointer_plus_loc (location, TREE_OPERAND (t, 0),
					  TREE_OPERAND (t, 1));
      base = addr;
      limit = fold_build_pointer_plus_hwi_loc (location,
			   fold_build_pointer_plus_loc (location,
							base, size), -1);
      break;

    case TARGET_MEM_REF:
      addr = tree_mem_ref_addr (ptr_type_node, t);
      base = addr;
      limit = fold_build_pointer_plus_hwi_loc (location,
			   fold_build_pointer_plus_loc (location,
							base, size), -1);
      break;

    case ARRAY_RANGE_REF:
      warning (OPT_Wmudflap,
	       "mudflap checking not yet implemented for ARRAY_RANGE_REF");
      return;

    case BIT_FIELD_REF:
      /* ??? merge with COMPONENT_REF code above? */
      {
        tree ofs, rem, bpu;

        /* If we're not dereferencing something, then the access
           must be ok.  */
        if (TREE_CODE (TREE_OPERAND (t, 0)) != INDIRECT_REF)
          return;

        bpu = bitsize_int (BITS_PER_UNIT);
        ofs = fold_convert (bitsizetype, TREE_OPERAND (t, 2));
        rem = size_binop_loc (location, TRUNC_MOD_EXPR, ofs, bpu);
        ofs = size_binop_loc (location, TRUNC_DIV_EXPR, ofs, bpu);

        size = fold_convert (bitsizetype, TREE_OPERAND (t, 1));
        size = size_binop_loc (location, PLUS_EXPR, size, rem);
        size = size_binop_loc (location, CEIL_DIV_EXPR, size, bpu);
        size = fold_convert (sizetype, size);

        addr = TREE_OPERAND (TREE_OPERAND (t, 0), 0);
        addr = fold_convert (ptr_type_node, addr);
        addr = fold_build_pointer_plus_loc (location, addr, ofs);

        base = addr;
        limit = fold_build_pointer_plus_hwi_loc (location,
                             fold_build_pointer_plus_loc (location,
							  base, size), -1);
      }
      break;

    default:
      return;
    }

  mf_build_check_statement_for (base, limit, iter, location, dirflag);
}
/* Transform
   1) Memory references.
*/
static void
mf_xform_statements (void)
{
  basic_block bb, next;
  gimple_stmt_iterator i;
  int saved_last_basic_block = last_basic_block;
  enum gimple_rhs_class grhs_class;

  bb = ENTRY_BLOCK_PTR ->next_bb;
  do
    {
      next = bb->next_bb;
      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
        {
          gimple s = gsi_stmt (i);

          /* Only a few GIMPLE statements can reference memory.  */
          switch (gimple_code (s))
            {
            case GIMPLE_ASSIGN:
	      mf_xform_derefs_1 (&i, gimple_assign_lhs_ptr (s),
		  		 gimple_location (s), integer_one_node);
	      mf_xform_derefs_1 (&i, gimple_assign_rhs1_ptr (s),
		  		 gimple_location (s), integer_zero_node);
	      grhs_class = get_gimple_rhs_class (gimple_assign_rhs_code (s));
	      if (grhs_class == GIMPLE_BINARY_RHS)
		mf_xform_derefs_1 (&i, gimple_assign_rhs2_ptr (s),
				   gimple_location (s), integer_zero_node);
              break;

            case GIMPLE_RETURN:
              if (gimple_return_retval (s) != NULL_TREE)
                {
                  mf_xform_derefs_1 (&i, gimple_return_retval_ptr (s),
				     gimple_location (s),
				     integer_zero_node);
                }
              break;

            default:
              ;
            }
        }
      bb = next;
    }
  while (bb && bb->index <= saved_last_basic_block);
}

/* ------------------------------------------------------------------------ */
/* ADDR_EXPR transforms.  Perform the declaration-related mudflap tree
   transforms on the current function.

   This is the first part of the mudflap instrumentation.  It works on
   high-level GIMPLE because after lowering, all variables are moved out
   of their BIND_EXPR binding context, and we lose liveness information
   for the declarations we wish to instrument.  */

static unsigned int
execute_mudflap_function_decls (void)
{
  struct gimplify_ctx gctx;

  /* Don't instrument functions such as the synthetic constructor
     built during mudflap_finish_file.  */
  if (mf_marked_p (current_function_decl)
      || mf_artificial (current_function_decl))
    return 0;

  push_gimplify_context (&gctx);

  mf_xform_decls (gimple_body (current_function_decl),
                  DECL_ARGUMENTS (current_function_decl));

  pop_gimplify_context (NULL);
  return 0;
}

/* This struct is passed between mf_xform_decls to store state needed
   during the traversal searching for objects that have their
   addresses taken.  */
struct mf_xform_decls_data
{
  tree param_decls;
};


/* Synthesize a CALL_EXPR and a TRY_FINALLY_EXPR, for this chain of
   _DECLs if appropriate.  Arrange to call the __mf_register function
   now, and the __mf_unregister function later for each.  Return the
   gimple sequence after synthesis.  */
gimple_seq
mx_register_decls (tree decl, gimple_seq seq, location_t location)
{
  gimple_seq finally_stmts = NULL;
  gimple_stmt_iterator initially_stmts = gsi_start (seq);

  while (decl != NULL_TREE)
    {
      if (mf_decl_eligible_p (decl)
          /* Not already processed.  */
          && ! mf_marked_p (decl)
          /* Automatic variable.  */
          && ! DECL_EXTERNAL (decl)
          && ! TREE_STATIC (decl))
        {
          tree size = NULL_TREE, variable_name;
          gimple unregister_fncall, register_fncall;
	  tree unregister_fncall_param, register_fncall_param;

	  /* Variable-sized objects should have sizes already been
	     gimplified when we got here. */
	  size = fold_convert (size_type_node,
			       TYPE_SIZE_UNIT (TREE_TYPE (decl)));
	  gcc_assert (is_gimple_val (size));


          unregister_fncall_param =
	    mf_mark (build1 (ADDR_EXPR,
			     build_pointer_type (TREE_TYPE (decl)),
			     decl));
          /* __mf_unregister (&VARIABLE, sizeof (VARIABLE), __MF_TYPE_STACK) */
          unregister_fncall = gimple_build_call (mf_unregister_fndecl, 3,
						 unregister_fncall_param,
						 size,
						 integer_three_node);


          variable_name = mf_varname_tree (decl);
          register_fncall_param =
	    mf_mark (build1 (ADDR_EXPR,
			     build_pointer_type (TREE_TYPE (decl)),
			     decl));
          /* __mf_register (&VARIABLE, sizeof (VARIABLE), __MF_TYPE_STACK,
	                    "name") */
	  register_fncall = gimple_build_call (mf_register_fndecl, 4,
					       register_fncall_param,
					       size,
					       integer_three_node,
					       variable_name);


          /* Accumulate the two calls.  */
	  gimple_set_location (register_fncall, location);
	  gimple_set_location (unregister_fncall, location);

          /* Add the __mf_register call at the current appending point.  */
          if (gsi_end_p (initially_stmts))
	    {
	      if (!mf_artificial (decl))
		warning (OPT_Wmudflap,
			 "mudflap cannot track %qE in stub function",
			 DECL_NAME (decl));
	    }
	  else
	    {
	      gsi_insert_before (&initially_stmts, register_fncall,
				 GSI_SAME_STMT);

	      /* Accumulate the FINALLY piece.  */
	      gimple_seq_add_stmt (&finally_stmts, unregister_fncall);
	    }
          mf_mark (decl);
        }

      decl = DECL_CHAIN (decl);
    }

  /* Actually, (initially_stmts!=NULL) <=> (finally_stmts!=NULL) */
  if (finally_stmts != NULL)
    {
      gimple stmt = gimple_build_try (seq, finally_stmts, GIMPLE_TRY_FINALLY);
      gimple_seq new_seq = NULL;

      gimple_seq_add_stmt (&new_seq, stmt);
      return new_seq;
    }
   else
    return seq;
}


/* Process every variable mentioned in BIND_EXPRs.  */
static tree
mx_xfn_xform_decls (gimple_stmt_iterator *gsi,
		    bool *handled_operands_p ATTRIBUTE_UNUSED,
		    struct walk_stmt_info *wi)
{
  struct mf_xform_decls_data *d = (struct mf_xform_decls_data *) wi->info;
  gimple stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_BIND:
      {
        /* Process function parameters now (but only once).  */
	if (d->param_decls)
	  {
	    gimple_bind_set_body (stmt,
				  mx_register_decls (d->param_decls,
						     gimple_bind_body (stmt),
						     gimple_location (stmt)));
	    d->param_decls = NULL_TREE;
	  }

	gimple_bind_set_body (stmt,
			      mx_register_decls (gimple_bind_vars (stmt),
						 gimple_bind_body (stmt),
						 gimple_location (stmt)));
      }
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Perform the object lifetime tracking mudflap transform on the given function
   tree.  The tree is mutated in place, with possibly copied subtree nodes.

   For every auto variable declared, if its address is ever taken
   within the function, then supply its lifetime to the mudflap
   runtime with the __mf_register and __mf_unregister calls.
*/

static void
mf_xform_decls (gimple_seq fnbody, tree fnparams)
{
  struct mf_xform_decls_data d;
  struct walk_stmt_info wi;
  struct pointer_set_t *pset = pointer_set_create ();

  d.param_decls = fnparams;
  memset (&wi, 0, sizeof (wi));
  wi.info = (void*) &d;
  wi.pset = pset;
  walk_gimple_seq (fnbody, mx_xfn_xform_decls, NULL, &wi);
  pointer_set_destroy (pset);
}


/* ------------------------------------------------------------------------ */
/* Externally visible mudflap functions.  */


/* Mark and return the given tree node to prevent further mudflap
   transforms.  */
static GTY ((param_is (union tree_node))) htab_t marked_trees = NULL;

tree
mf_mark (tree t)
{
  void **slot;

  if (marked_trees == NULL)
    marked_trees = htab_create_ggc (31, htab_hash_pointer, htab_eq_pointer,
				    NULL);

  slot = htab_find_slot (marked_trees, t, INSERT);
  *slot = t;
  return t;
}

int
mf_marked_p (tree t)
{
  void *entry;

  if (marked_trees == NULL)
    return 0;

  entry = htab_find (marked_trees, t);
  return (entry != NULL);
}

/* Remember given node as a static of some kind: global data,
   function-scope static, or an anonymous constant.  Its assembler
   label is given.  */

/* A list of globals whose incomplete declarations we encountered.
   Instead of emitting the __mf_register call for them here, it's
   delayed until program finish time.  If they're still incomplete by
   then, warnings are emitted.  */

static GTY (()) VEC(tree,gc) *deferred_static_decls;

/* A list of statements for calling __mf_register() at startup time.  */
static GTY (()) tree enqueued_call_stmt_chain;

static void
mudflap_register_call (tree obj, tree object_size, tree varname)
{
  tree arg, call_stmt;

  arg = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (obj)), obj);
  arg = fold_convert (ptr_type_node, arg);

  call_stmt = build_call_expr (mf_register_fndecl, 4,
			       arg,
			       fold_convert (size_type_node, object_size),
			       /* __MF_TYPE_STATIC */
			       build_int_cst (integer_type_node, 4),
			       varname);

  append_to_statement_list (call_stmt, &enqueued_call_stmt_chain);
}

void
mudflap_enqueue_decl (tree obj)
{
  if (mf_marked_p (obj))
    return;

  /* We don't need to process variable decls that are internally
     generated extern.  If we did, we'd end up with warnings for them
     during mudflap_finish_file ().  That would confuse the user,
     since the text would refer to variables that don't show up in the
     user's source code.  */
  if (DECL_P (obj) && DECL_EXTERNAL (obj) && mf_artificial (obj))
    return;

  VEC_safe_push (tree, gc, deferred_static_decls, obj);
}


void
mudflap_enqueue_constant (tree obj)
{
  tree object_size, varname;

  if (mf_marked_p (obj))
    return;

  if (TREE_CODE (obj) == STRING_CST)
    object_size = size_int (TREE_STRING_LENGTH (obj));
  else
    object_size = size_in_bytes (TREE_TYPE (obj));

  if (TREE_CODE (obj) == STRING_CST)
    varname = mf_build_string ("string literal");
  else
    varname = mf_build_string ("constant");

  mudflap_register_call (obj, object_size, varname);
}


/* Emit any file-wide instrumentation.  */
void
mudflap_finish_file (void)
{
  tree ctor_statements = NULL_TREE;

  /* No need to continue when there were errors.  */
  if (seen_error ())
    return;

  /* Insert a call to __mf_init.  */
  {
    tree call2_stmt = build_call_expr (mf_init_fndecl, 0);
    append_to_statement_list (call2_stmt, &ctor_statements);
  }

  /* If appropriate, call __mf_set_options to pass along read-ignore mode.  */
  if (flag_mudflap_ignore_reads)
    {
      tree arg = mf_build_string ("-ignore-reads");
      tree call_stmt = build_call_expr (mf_set_options_fndecl, 1, arg);
      append_to_statement_list (call_stmt, &ctor_statements);
    }

  /* Process all enqueued object decls.  */
  if (deferred_static_decls)
    {
      size_t i;
      tree obj;
      FOR_EACH_VEC_ELT (tree, deferred_static_decls, i, obj)
        {
          gcc_assert (DECL_P (obj));

          if (mf_marked_p (obj))
            continue;

          /* Omit registration for static unaddressed objects.  NB:
             Perform registration for non-static objects regardless of
             TREE_USED or TREE_ADDRESSABLE, because they may be used
             from other compilation units.  */
          if (! TREE_PUBLIC (obj) && ! TREE_ADDRESSABLE (obj))
            continue;

          if (! COMPLETE_TYPE_P (TREE_TYPE (obj)))
            {
              warning (OPT_Wmudflap,
		       "mudflap cannot track unknown size extern %qE",
                       DECL_NAME (obj));
              continue;
            }

          mudflap_register_call (obj,
                                 size_in_bytes (TREE_TYPE (obj)),
                                 mf_varname_tree (obj));
        }

      VEC_truncate (tree, deferred_static_decls, 0);
    }

  /* Append all the enqueued registration calls.  */
  if (enqueued_call_stmt_chain)
    {
      append_to_statement_list (enqueued_call_stmt_chain, &ctor_statements);
      enqueued_call_stmt_chain = NULL_TREE;
    }

  cgraph_build_static_cdtor ('I', ctor_statements,
                             MAX_RESERVED_INIT_PRIORITY-1);
}


static bool
gate_mudflap (void)
{
  return flag_mudflap != 0;
}

struct gimple_opt_pass pass_mudflap_1 =
{
 {
  GIMPLE_PASS,
  "mudflap1",                           /* name */
  gate_mudflap,                         /* gate */
  execute_mudflap_function_decls,       /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  PROP_gimple_any,                      /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_mudflap_2 =
{
 {
  GIMPLE_PASS,
  "mudflap2",                           /* name */
  gate_mudflap,                         /* gate */
  execute_mudflap_function_ops,         /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  PROP_ssa | PROP_cfg | PROP_gimple_leh,/* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_verify_flow | TODO_verify_stmts
  | TODO_update_ssa                     /* todo_flags_finish */
 }
};

#include "gt-tree-mudflap.h"
