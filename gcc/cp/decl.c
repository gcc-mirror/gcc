/* Process declarations and variables for C++ compiler.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002, 2003, 2004, 2005  Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Process declarations and symbol lookup for C++ front end.
   Also constructs types; the standard scalar types at initialization,
   and structure, union, array and enum types when they are declared.  */

/* ??? not all decl nodes are given the most useful possible
   line numbers.  For example, the CONST_DECLs for enum values.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "expr.h"
#include "flags.h"
#include "cp-tree.h"
#include "tree-inline.h"
#include "decl.h"
#include "lex.h"
#include "output.h"
#include "except.h"
#include "toplev.h"
#include "hashtab.h"
#include "tm_p.h"
#include "target.h"
#include "c-common.h"
#include "c-pragma.h"
#include "diagnostic.h"
#include "debug.h"
#include "timevar.h"

static tree grokparms (tree, tree *);
static const char *redeclaration_error_message (tree, tree);

static int decl_jump_unsafe (tree);
static void require_complete_types_for_parms (tree);
static int ambi_op_p (enum tree_code);
static int unary_op_p (enum tree_code);
static void push_local_name (tree);
static tree grok_reference_init (tree, tree, tree, tree *);
static tree grokfndecl (tree, tree, tree, tree, tree, int,
			enum overload_flags, tree,
			tree, int, int, int, int, int, int, tree);
static tree grokvardecl (tree, tree, RID_BIT_TYPE *, int, int, tree);
static void record_unknown_type (tree, const char *);
static tree builtin_function_1 (const char *, tree, tree, int,
                                enum built_in_class, const char *,
				tree);
static tree build_library_fn_1 (tree, enum tree_code, tree);
static int member_function_or_else (tree, tree, enum overload_flags);
static void bad_specifiers (tree, const char *, int, int, int, int,
			    int);
static void check_for_uninitialized_const_var (tree);
static hashval_t typename_hash (const void *);
static int typename_compare (const void *, const void *);
static tree local_variable_p_walkfn (tree *, int *, void *);
static tree record_builtin_java_type (const char *, int);
static const char *tag_name (enum tag_types code);
static int walk_namespaces_r (tree, walk_namespaces_fn, void *);
static int walk_globals_r (tree, void*);
static int walk_vtables_r (tree, void*);
static tree make_label_decl (tree, int);
static void use_label (tree);
static void check_previous_goto_1 (tree, struct cp_binding_level *, tree,
				   const location_t *);
static void check_previous_goto (struct named_label_use_list *);
static void check_switch_goto (struct cp_binding_level *);
static void check_previous_gotos (tree);
static void pop_label (tree, tree);
static void pop_labels (tree);
static void maybe_deduce_size_from_array_init (tree, tree);
static void layout_var_decl (tree);
static void maybe_commonize_var (tree);
static tree check_initializer (tree, tree, int, tree *);
static void make_rtl_for_nonlocal_decl (tree, tree, const char *);
static void save_function_data (tree);
static void check_function_type (tree, tree);
static void begin_constructor_body (void);
static void finish_constructor_body (void);
static void begin_destructor_body (void);
static void finish_destructor_body (void);
static tree create_array_type_for_decl (tree, tree, tree);
static tree get_atexit_node (void);
static tree get_dso_handle_node (void);
static tree start_cleanup_fn (void);
static void end_cleanup_fn (void);
static tree cp_make_fname_decl (tree, int);
static void initialize_predefined_identifiers (void);
static tree check_special_function_return_type 
	(special_function_kind, tree, tree);
static tree push_cp_library_fn (enum tree_code, tree);
static tree build_cp_library_fn (tree, enum tree_code, tree);
static void store_parm_decls (tree);
static int cp_missing_noreturn_ok_p (tree);
static void initialize_local_var (tree, tree);
static void expand_static_init (tree, tree);
static tree next_initializable_field (tree);
static tree reshape_init (tree, tree *);
static bool reshape_init_array (tree, tree, tree *, tree);
static tree build_typename_type (tree, tree, tree);

/* Erroneous argument lists can use this *IFF* they do not modify it.  */
tree error_mark_list;

/* The following symbols are subsumed in the cp_global_trees array, and
   listed here individually for documentation purposes.

   C++ extensions
	tree wchar_decl_node;

	tree vtable_entry_type;
	tree delta_type_node;
	tree __t_desc_type_node;
        tree ti_desc_type_node;
	tree bltn_desc_type_node, ptr_desc_type_node;
	tree ary_desc_type_node, func_desc_type_node, enum_desc_type_node;
	tree class_desc_type_node, si_class_desc_type_node, vmi_class_desc_type_node;
	tree ptm_desc_type_node;
	tree base_desc_type_node;

	tree class_type_node;
	tree unknown_type_node;

   Array type `vtable_entry_type[]'

	tree vtbl_type_node;
	tree vtbl_ptr_type_node;

   Namespaces,

	tree std_node;
	tree abi_node;

   A FUNCTION_DECL which can call `abort'.  Not necessarily the
   one that the user will declare, but sufficient to be called
   by routines that want to abort the program.

	tree abort_fndecl;

   The FUNCTION_DECL for the default `::operator delete'.

	tree global_delete_fndecl;

   Used by RTTI
	tree type_info_type_node, tinfo_decl_id, tinfo_decl_type;
	tree tinfo_var_id;

*/

tree cp_global_trees[CPTI_MAX];

/* Indicates that there is a type value in some namespace, although
   that is not necessarily in scope at the moment.  */

tree global_type_node;

/* The node that holds the "name" of the global scope.  */
tree global_scope_name;

/* Used only for jumps to as-yet undefined labels, since jumps to
   defined labels can have their validity checked immediately.  */

struct named_label_use_list GTY(())
{
  struct cp_binding_level *binding_level;
  tree names_in_scope;
  tree label_decl;
  location_t o_goto_locus;
  struct named_label_use_list *next;
};

#define named_label_uses cp_function_chain->x_named_label_uses

#define local_names cp_function_chain->x_local_names

/* A list of objects which have constructors or destructors
   which reside in the global scope.  The decl is stored in
   the TREE_VALUE slot and the initializer is stored
   in the TREE_PURPOSE slot.  */
tree static_aggregates;

/* -- end of C++ */

/* A node for the integer constants 2, and 3.  */

tree integer_two_node, integer_three_node;

/* A list of all LABEL_DECLs in the function that have names.  Here so
   we can clear out their names' definitions at the end of the
   function, and so we can check the validity of jumps to these labels.  */

struct named_label_list GTY(())
{
  struct cp_binding_level *binding_level;
  tree names_in_scope;
  tree old_value;
  tree label_decl;
  tree bad_decls;
  struct named_label_list *next;
  unsigned int in_try_scope : 1;
  unsigned int in_catch_scope : 1;
};

#define named_labels cp_function_chain->x_named_labels

/* The number of function bodies which we are currently processing.
   (Zero if we are at namespace scope, one inside the body of a
   function, two inside the body of a function in a local class, etc.)  */
int function_depth;

/* States indicating how grokdeclarator() should handle declspecs marked
   with __attribute__((deprecated)).  An object declared as
   __attribute__((deprecated)) suppresses warnings of uses of other
   deprecated items.  */
   
enum deprecated_states {
  DEPRECATED_NORMAL,
  DEPRECATED_SUPPRESS
};

static enum deprecated_states deprecated_state = DEPRECATED_NORMAL;

/* Set by add_implicitly_declared_members() to keep those members from
   being flagged as deprecated or reported as using deprecated
   types.  */
int adding_implicit_members = 0;

/* True if a declaration with an `extern' linkage specifier is being
   processed.  */
bool have_extern_spec;


/* A TREE_LIST of VAR_DECLs.  The TREE_PURPOSE is a RECORD_TYPE or
   UNION_TYPE; the TREE_VALUE is a VAR_DECL with that type.  At the
   time the VAR_DECL was declared, the type was incomplete.  */

static GTY(()) tree incomplete_vars;

/* Returns the kind of template specialization we are currently
   processing, given that it's declaration contained N_CLASS_SCOPES
   explicit scope qualifications.  */

tmpl_spec_kind
current_tmpl_spec_kind (int n_class_scopes)
{
  int n_template_parm_scopes = 0;
  int seen_specialization_p = 0;
  int innermost_specialization_p = 0;
  struct cp_binding_level *b;

  /* Scan through the template parameter scopes.  */
  for (b = current_binding_level; 
       b->kind == sk_template_parms; 
       b = b->level_chain)
    {
      /* If we see a specialization scope inside a parameter scope,
	 then something is wrong.  That corresponds to a declaration
	 like:

	    template <class T> template <> ...

	 which is always invalid since [temp.expl.spec] forbids the
	 specialization of a class member template if the enclosing
	 class templates are not explicitly specialized as well.  */
      if (b->explicit_spec_p)
	{
	  if (n_template_parm_scopes == 0)
	    innermost_specialization_p = 1;
	  else
	    seen_specialization_p = 1;
	}
      else if (seen_specialization_p == 1)
	return tsk_invalid_member_spec;

      ++n_template_parm_scopes;
    }

  /* Handle explicit instantiations.  */
  if (processing_explicit_instantiation)
    {
      if (n_template_parm_scopes != 0)
	/* We've seen a template parameter list during an explicit
	   instantiation.  For example:

	     template <class T> template void f(int);

	   This is erroneous.  */
	return tsk_invalid_expl_inst;
      else
	return tsk_expl_inst;
    }

  if (n_template_parm_scopes < n_class_scopes)
    /* We've not seen enough template headers to match all the
       specialized classes present.  For example:

         template <class T> void R<T>::S<T>::f(int);

       This is invalid; there needs to be one set of template
       parameters for each class.  */
    return tsk_insufficient_parms;
  else if (n_template_parm_scopes == n_class_scopes)
    /* We're processing a non-template declaration (even though it may
       be a member of a template class.)  For example:

         template <class T> void S<T>::f(int);

       The `class T' maches the `S<T>', leaving no template headers
       corresponding to the `f'.  */
    return tsk_none;
  else if (n_template_parm_scopes > n_class_scopes + 1)
    /* We've got too many template headers.  For example:

         template <> template <class T> void f (T);

       There need to be more enclosing classes.  */
    return tsk_excessive_parms;
  else
    /* This must be a template.  It's of the form:

         template <class T> template <class U> void S<T>::f(U);

       This is a specialization if the innermost level was a
       specialization; otherwise it's just a definition of the
       template.  */
    return innermost_specialization_p ? tsk_expl_spec : tsk_template;
}

/* Exit the current scope.  */

void
finish_scope (void)
{
  poplevel (0, 0, 0);
}

/* When a label goes out of scope, check to see if that label was used
   in a valid manner, and issue any appropriate warnings or errors.  */

static void
pop_label (tree label, tree old_value)
{
  if (!processing_template_decl)
    {
      if (DECL_INITIAL (label) == NULL_TREE)
	{
	  location_t location;

	  cp_error_at ("label `%D' used but not defined", label);
 	  location.file = input_filename;
	  location.line = 0;
	  /* Avoid crashing later.  */
	  define_label (location, DECL_NAME (label));
	}
      else if (warn_unused_label && !TREE_USED (label))
	cp_warning_at ("label `%D' defined but not used", label);
    }

  SET_IDENTIFIER_LABEL_VALUE (DECL_NAME (label), old_value);
}

/* At the end of a function, all labels declared within the function
   go out of scope.  BLOCK is the top-level block for the
   function.  */

static void
pop_labels (tree block)
{
  struct named_label_list *link;

  /* Clear out the definitions of all label names, since their scopes
     end here.  */
  for (link = named_labels; link; link = link->next)
    {
      pop_label (link->label_decl, link->old_value);
      /* Put the labels into the "variables" of the top-level block,
	 so debugger can see them.  */
      TREE_CHAIN (link->label_decl) = BLOCK_VARS (block);
      BLOCK_VARS (block) = link->label_decl;
    }

  named_labels = NULL;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP == 1, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (int keep, int reverse, int functionbody)
{
  tree link;
  /* The chain of decls was accumulated in reverse order.
     Put it into forward order, just for cleanliness.  */
  tree decls;
  int tmp = functionbody;
  int real_functionbody;
  tree subblocks;
  tree block = NULL_TREE;
  tree decl;
  int leaving_for_scope;
  scope_kind kind;

  timevar_push (TV_NAME_LOOKUP);

  my_friendly_assert (current_binding_level->kind != sk_class, 19990916);

  real_functionbody = (current_binding_level->kind == sk_cleanup
		       ? ((functionbody = 0), tmp) : functionbody);
  subblocks = functionbody >= 0 ? current_binding_level->blocks : 0;

  my_friendly_assert (!current_binding_level->class_shadowed,
		      19990414);

  /* We used to use KEEP == 2 to indicate that the new block should go
     at the beginning of the list of blocks at this binding level,
     rather than the end.  This hack is no longer used.  */
  my_friendly_assert (keep == 0 || keep == 1, 0);

  if (current_binding_level->keep)
    keep = 1;

  /* Any uses of undefined labels, and any defined labels, now operate
     under constraints of next binding contour.  */
  if (cfun && !functionbody)
    {
      struct cp_binding_level *level_chain;
      level_chain = current_binding_level->level_chain;
      if (level_chain)
	{
	  struct named_label_use_list *uses;
	  struct named_label_list *labels;
	  for (labels = named_labels; labels; labels = labels->next)
	    if (labels->binding_level == current_binding_level)
	      {
		tree decl;
		if (current_binding_level->kind == sk_try)
		  labels->in_try_scope = 1;
		if (current_binding_level->kind == sk_catch)
		  labels->in_catch_scope = 1;
		for (decl = labels->names_in_scope; decl;
		     decl = TREE_CHAIN (decl))
		  if (decl_jump_unsafe (decl))
		    labels->bad_decls = tree_cons (NULL_TREE, decl,
						   labels->bad_decls);
		labels->binding_level = level_chain;
		labels->names_in_scope = level_chain->names;
	      }

	  for (uses = named_label_uses; uses; uses = uses->next)
	    if (uses->binding_level == current_binding_level)
	      {
		uses->binding_level = level_chain;
		uses->names_in_scope = level_chain->names;
	      }
	}
    }

  /* Get the decls in the order they were written.
     Usually current_binding_level->names is in reverse order.
     But parameter decls were previously put in forward order.  */

  if (reverse)
    current_binding_level->names
      = decls = nreverse (current_binding_level->names);
  else
    decls = current_binding_level->names;

  /* Output any nested inline functions within this block
     if they weren't already output.  */
  for (decl = decls; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == FUNCTION_DECL
	&& ! TREE_ASM_WRITTEN (decl)
	&& DECL_INITIAL (decl) != NULL_TREE
	&& TREE_ADDRESSABLE (decl)
	&& decl_function_context (decl) == current_function_decl)
      {
	/* If this decl was copied from a file-scope decl
	   on account of a block-scope extern decl,
	   propagate TREE_ADDRESSABLE to the file-scope decl.  */
	if (DECL_ABSTRACT_ORIGIN (decl) != NULL_TREE)
	  TREE_ADDRESSABLE (DECL_ABSTRACT_ORIGIN (decl)) = 1;
	else
	  {
	    push_function_context ();
	    output_inline_function (decl);
	    pop_function_context ();
	  }
      }

  /* When not in function-at-a-time mode, expand_end_bindings will
     warn about unused variables.  But, in function-at-a-time mode
     expand_end_bindings is not passed the list of variables in the
     current scope, and therefore no warning is emitted.  So, we
     explicitly warn here.  */
  if (!processing_template_decl)
    warn_about_unused_variables (getdecls ());

  /* If there were any declarations or structure tags in that level,
     or if this level is a function body,
     create a BLOCK to record them for the life of this function.  */
  block = NULL_TREE;
  if (keep == 1 || functionbody)
    block = make_node (BLOCK);
  if (block != NULL_TREE)
    {
      BLOCK_VARS (block) = decls;
      BLOCK_SUBBLOCKS (block) = subblocks;
    }

  /* In each subblock, record that this is its superior.  */
  if (keep >= 0)
    for (link = subblocks; link; link = TREE_CHAIN (link))
      BLOCK_SUPERCONTEXT (link) = block;

  /* We still support the old for-scope rules, whereby the variables
     in a for-init statement were in scope after the for-statement
     ended.  We only use the new rules if flag_new_for_scope is
     nonzero.  */
  leaving_for_scope
    = current_binding_level->kind == sk_for && flag_new_for_scope == 1;

  /* Remove declarations for all the DECLs in this level.  */
  for (link = decls; link; link = TREE_CHAIN (link))
    {
      if (leaving_for_scope && TREE_CODE (link) == VAR_DECL
          && DECL_NAME (link))
	{
	  cxx_binding *outer_binding
	    = IDENTIFIER_BINDING (DECL_NAME (link))->previous;
	  tree ns_binding;

	  if (!outer_binding)
	    ns_binding = IDENTIFIER_NAMESPACE_VALUE (DECL_NAME (link));
	  else
	    ns_binding = NULL_TREE;

	  if (outer_binding
	      && outer_binding->scope == current_binding_level->level_chain)
	    /* We have something like:

	         int i;
	         for (int i; ;);

	       and we are leaving the `for' scope.  There's no reason to
	       keep the binding of the inner `i' in this case.  */
	    pop_binding (DECL_NAME (link), link);
	  else if ((outer_binding
		    && (TREE_CODE (outer_binding->value) == TYPE_DECL))
		   || (ns_binding && TREE_CODE (ns_binding) == TYPE_DECL))
	    /* Here, we have something like:

		 typedef int I;

		 void f () {
		   for (int I; ;);
		 }

	       We must pop the for-scope binding so we know what's a
	       type and what isn't.  */
	    pop_binding (DECL_NAME (link), link);
	  else
	    {
	      /* Mark this VAR_DECL as dead so that we can tell we left it
		 there only for backward compatibility.  */
	      DECL_DEAD_FOR_LOCAL (link) = 1;

	      /* Keep track of what should have happened when we
		 popped the binding.  */
	      if (outer_binding && outer_binding->value)
		DECL_SHADOWED_FOR_VAR (link) = outer_binding->value;

	      /* Add it to the list of dead variables in the next
		 outermost binding to that we can remove these when we
		 leave that binding.  */
	      current_binding_level->level_chain->dead_vars_from_for
		= tree_cons (NULL_TREE, link,
			     current_binding_level->level_chain->
			     dead_vars_from_for);

	      /* Although we don't pop the cxx_binding, we do clear
		 its SCOPE since the scope is going away now.  */
	      IDENTIFIER_BINDING (DECL_NAME (link))->scope = NULL;
	    }
	}
      else
	{
	  /* Remove the binding.  */
	  decl = link;
	  if (TREE_CODE (decl) == TREE_LIST)
	    decl = TREE_VALUE (decl);
	  if (DECL_P (decl))
	    pop_binding (DECL_NAME (decl), decl);
	  else if (TREE_CODE (decl) == OVERLOAD)
	    pop_binding (DECL_NAME (OVL_FUNCTION (decl)), decl);
	  else
	    abort ();
	}
    }

  /* Remove declarations for any `for' variables from inner scopes
     that we kept around.  */
  for (link = current_binding_level->dead_vars_from_for;
       link; link = TREE_CHAIN (link))
    pop_binding (DECL_NAME (TREE_VALUE (link)), TREE_VALUE (link));

  /* Restore the IDENTIFIER_TYPE_VALUEs.  */
  for (link = current_binding_level->type_shadowed;
       link; link = TREE_CHAIN (link))
    SET_IDENTIFIER_TYPE_VALUE (TREE_PURPOSE (link), TREE_VALUE (link));

  /* Restore the IDENTIFIER_LABEL_VALUEs for local labels.  */
  for (link = current_binding_level->shadowed_labels;
       link;
       link = TREE_CHAIN (link))
    pop_label (TREE_VALUE (link), TREE_PURPOSE (link));

  /* There may be OVERLOADs (wrapped in TREE_LISTs) on the BLOCK_VARs
     list if a `using' declaration put them there.  The debugging
     back-ends won't understand OVERLOAD, so we remove them here.
     Because the BLOCK_VARS are (temporarily) shared with
     CURRENT_BINDING_LEVEL->NAMES we must do this fixup after we have
     popped all the bindings.  */
  if (block)
    {
      tree* d;

      for (d = &BLOCK_VARS (block); *d; )
	{
	  if (TREE_CODE (*d) == TREE_LIST)
	    *d = TREE_CHAIN (*d);
	  else
	    d = &TREE_CHAIN (*d);
	}
    }

  /* If the level being exited is the top level of a function,
     check over all the labels.  */
  if (functionbody)
    {
      /* Since this is the top level block of a function, the vars are
	 the function's parameters.  Don't leave them in the BLOCK
	 because they are found in the FUNCTION_DECL instead.  */
      BLOCK_VARS (block) = 0;
      pop_labels (block);
    }

  kind = current_binding_level->kind;

  leave_scope ();
  if (functionbody)
    DECL_INITIAL (current_function_decl) = block;
  else if (block)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, block);

  /* If we did not make a block for the level just exited,
     any blocks made for inner levels
     (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks
     of something else.  */
  else if (subblocks)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblocks);

  /* Each and every BLOCK node created here in `poplevel' is important
     (e.g. for proper debugging information) so if we created one
     earlier, mark it as "used".  */
  if (block)
    TREE_USED (block) = 1;

  /* Take care of compiler's internal binding structures.  */
  if (kind == sk_cleanup)
    {
      tree scope_stmts;

      scope_stmts
	= add_scope_stmt (/*begin_p=*/0, /*partial_p=*/1);
      if (block)
	{
	  SCOPE_STMT_BLOCK (TREE_PURPOSE (scope_stmts)) = block;
	  SCOPE_STMT_BLOCK (TREE_VALUE (scope_stmts)) = block;
	}

      block = poplevel (keep, reverse, functionbody);
    }

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, block);
}

/* Delete the node BLOCK from the current binding level.
   This is used for the block inside a stmt expr ({...})
   so that the block can be reinserted where appropriate.  */

void
delete_block (tree block)
{
  tree t;
  if (current_binding_level->blocks == block)
    current_binding_level->blocks = TREE_CHAIN (block);
  for (t = current_binding_level->blocks; t;)
    {
      if (TREE_CHAIN (t) == block)
	TREE_CHAIN (t) = TREE_CHAIN (block);
      else
	t = TREE_CHAIN (t);
    }
  TREE_CHAIN (block) = NULL_TREE;
  /* Clear TREE_USED which is always set by poplevel.
     The flag is set again if insert_block is called.  */
  TREE_USED (block) = 0;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (tree block)
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (tree block ATTRIBUTE_UNUSED )
{
  /* The RTL expansion machinery requires us to provide this callback,
     but it is not applicable in function-at-a-time mode.  */
}

/* Returns nonzero if T is a virtual function table.  */

int
vtable_decl_p (tree t, void* data ATTRIBUTE_UNUSED )
{
  return (TREE_CODE (t) == VAR_DECL && DECL_VIRTUAL_P (t));
}

/* Returns nonzero if T is a TYPE_DECL for a type with virtual
   functions.  */

int
vtype_decl_p (tree t, void *data ATTRIBUTE_UNUSED )
{
  return (TREE_CODE (t) == TYPE_DECL
	  && TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE
	  && TYPE_POLYMORPHIC_P (TREE_TYPE (t)));
}

struct walk_globals_data {
  walk_globals_pred p;
  walk_globals_fn f;
  void *data;
};

/* Walk the vtable declarations in NAMESPACE.  Whenever one is found
   for which P returns nonzero, call F with its address.  If any call
   to F returns a nonzero value, return a nonzero value.  */

static int
walk_vtables_r (tree namespace, void* data)
{
  struct walk_globals_data* wgd = (struct walk_globals_data *) data;
  walk_globals_fn f = wgd->f;
  void *d = wgd->data;
  tree decl = NAMESPACE_LEVEL (namespace)->vtables;
  int result = 0;

  for (; decl ; decl = TREE_CHAIN (decl))
    result |= (*f) (&decl, d);

  return result;
}

/* Walk the vtable declarations.  Whenever one is found for which P
   returns nonzero, call F with its address.  If any call to F
   returns a nonzero value, return a nonzero value.  */
bool
walk_vtables (walk_globals_pred p, walk_globals_fn f, void *data)
{    
  struct walk_globals_data wgd;
  wgd.p = p;    
  wgd.f = f;
  wgd.data = data;

  return walk_namespaces (walk_vtables_r, &wgd);
}

/* Walk all the namespaces contained NAMESPACE, including NAMESPACE
   itself, calling F for each.  The DATA is passed to F as well.  */

static int
walk_namespaces_r (tree namespace, walk_namespaces_fn f, void* data)
{
  int result = 0;
  tree current = NAMESPACE_LEVEL (namespace)->namespaces;     

  result |= (*f) (namespace, data);

  for (; current; current = TREE_CHAIN (current))
    result |= walk_namespaces_r (current, f, data);

  return result;
}

/* Walk all the namespaces, calling F for each.  The DATA is passed to
   F as well.  */

int
walk_namespaces (walk_namespaces_fn f, void* data)
{
  return walk_namespaces_r (global_namespace, f, data);
}

/* Walk the global declarations in NAMESPACE.  Whenever one is found
   for which P returns nonzero, call F with its address.  If any call
   to F returns a nonzero value, return a nonzero value.  */

static int
walk_globals_r (tree namespace, void* data)
{
  struct walk_globals_data* wgd = (struct walk_globals_data *) data;
  walk_globals_pred p = wgd->p;
  walk_globals_fn f = wgd->f;
  void *d = wgd->data;
  tree *t;
  int result = 0;

  t = &NAMESPACE_LEVEL (namespace)->names;

  while (*t)
    {
      tree glbl = *t;

      if ((*p) (glbl, d))
	result |= (*f) (t, d);

      /* If F changed *T, then *T still points at the next item to
	 examine.  */
      if (*t == glbl)
	t = &TREE_CHAIN (*t);
    }

  return result;
}

/* Walk the global declarations.  Whenever one is found for which P
   returns true, call F with its address.  If any call to F
   returns true, return true.  */

bool
walk_globals (walk_globals_pred p, walk_globals_fn f, void *data)
{
  struct walk_globals_data wgd;
  wgd.p = p;
  wgd.f = f;
  wgd.data = data;

  return walk_namespaces (walk_globals_r, &wgd);
}

/* Call wrapup_globals_declarations for the globals in NAMESPACE.  If
   DATA is non-NULL, this is the last time we will call
   wrapup_global_declarations for this NAMESPACE.  */

int
wrapup_globals_for_namespace (tree namespace, void* data)
{
  struct cp_binding_level *level = NAMESPACE_LEVEL (namespace);
  varray_type statics = level->static_decls;
  tree *vec = &VARRAY_TREE (statics, 0);
  int len = VARRAY_ACTIVE_SIZE (statics);
  int last_time = (data != 0);

  if (last_time)
    {
      check_global_declarations (vec, len);
      return 0;
    }

  /* Write out any globals that need to be output.  */
  return wrapup_global_declarations (vec, len);
}


/* In C++, you don't have to write `struct S' to refer to `S'; you
   can just use `S'.  We accomplish this by creating a TYPE_DECL as
   if the user had written `typedef struct S S'.  Create and return
   the TYPE_DECL for TYPE.  */

tree
create_implicit_typedef (tree name, tree type)
{
  tree decl;

  decl = build_decl (TYPE_DECL, name, type);
  DECL_ARTIFICIAL (decl) = 1;
  /* There are other implicit type declarations, like the one *within*
     a class that allows you to write `S::S'.  We must distinguish
     amongst these.  */
  SET_DECL_IMPLICIT_TYPEDEF_P (decl);
  TYPE_NAME (type) = decl;

  return decl;
}

/* Remember a local name for name-mangling purposes.  */

static void
push_local_name (tree decl)
{
  size_t i, nelts;
  tree t, name;

  timevar_push (TV_NAME_LOOKUP);
  if (!local_names)
    VARRAY_TREE_INIT (local_names, 8, "local_names");

  name = DECL_NAME (decl);

  nelts = VARRAY_ACTIVE_SIZE (local_names);
  for (i = 0; i < nelts; i++)
    {
      t = VARRAY_TREE (local_names, i);
      if (DECL_NAME (t) == name)
	{
	  if (!DECL_LANG_SPECIFIC (decl))
	    retrofit_lang_decl (decl);
	  DECL_LANG_SPECIFIC (decl)->decl_flags.u2sel = 1;
	  if (DECL_LANG_SPECIFIC (t))
	    DECL_DISCRIMINATOR (decl) = DECL_DISCRIMINATOR (t) + 1;
	  else
	    DECL_DISCRIMINATOR (decl) = 1;

	  VARRAY_TREE (local_names, i) = decl;
	  timevar_pop (TV_NAME_LOOKUP);
	  return;
	}
    }

  VARRAY_PUSH_TREE (local_names, decl);
  timevar_pop (TV_NAME_LOOKUP);
}

/* Subroutine of duplicate_decls: return truthvalue of whether
   or not types of these decls match.

   For C++, we must compare the parameter list so that `int' can match
   `int&' in a parameter position, but `int&' is not confused with
   `const int&'.  */

int
decls_match (tree newdecl, tree olddecl)
{
  int types_match;

  if (newdecl == olddecl)
    return 1;

  if (TREE_CODE (newdecl) != TREE_CODE (olddecl))
    /* If the two DECLs are not even the same kind of thing, we're not
       interested in their types.  */
    return 0;

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      tree f1 = TREE_TYPE (newdecl);
      tree f2 = TREE_TYPE (olddecl);
      tree p1 = TYPE_ARG_TYPES (f1);
      tree p2 = TYPE_ARG_TYPES (f2);

      if (CP_DECL_CONTEXT (newdecl) != CP_DECL_CONTEXT (olddecl)
	  && ! (DECL_EXTERN_C_P (newdecl)
		&& DECL_EXTERN_C_P (olddecl)))
	return 0;

      if (TREE_CODE (f1) != TREE_CODE (f2))
        return 0;

      if (same_type_p (TREE_TYPE (f1), TREE_TYPE (f2)))
	{
	  if (p2 == NULL_TREE && DECL_EXTERN_C_P (olddecl)
	      && (DECL_BUILT_IN (olddecl)
#ifndef NO_IMPLICIT_EXTERN_C
	          || (DECL_IN_SYSTEM_HEADER (newdecl) && !DECL_CLASS_SCOPE_P (newdecl))
	          || (DECL_IN_SYSTEM_HEADER (olddecl) && !DECL_CLASS_SCOPE_P (olddecl))
#endif
	      ))
	    {
	      types_match = self_promoting_args_p (p1);
	      if (p1 == void_list_node)
		TREE_TYPE (newdecl) = TREE_TYPE (olddecl);
	    }
#ifndef NO_IMPLICIT_EXTERN_C
	  else if (p1 == NULL_TREE
		   && (DECL_EXTERN_C_P (olddecl)
	               && DECL_IN_SYSTEM_HEADER (olddecl)
	               && !DECL_CLASS_SCOPE_P (olddecl))
		   && (DECL_EXTERN_C_P (newdecl)
	               && DECL_IN_SYSTEM_HEADER (newdecl)
	               && !DECL_CLASS_SCOPE_P (newdecl)))
	    {
	      types_match = self_promoting_args_p (p2);
	      TREE_TYPE (newdecl) = TREE_TYPE (olddecl);
	    }
#endif
	  else
	    types_match = compparms (p1, p2);
	}
      else
	types_match = 0;
    }
  else if (TREE_CODE (newdecl) == TEMPLATE_DECL)
    {
      if (TREE_CODE (DECL_TEMPLATE_RESULT (newdecl))
	  != TREE_CODE (DECL_TEMPLATE_RESULT (olddecl)))
	return 0;

      if (!comp_template_parms (DECL_TEMPLATE_PARMS (newdecl),
				DECL_TEMPLATE_PARMS (olddecl)))
	return 0;

      if (TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == TYPE_DECL)
	types_match = same_type_p (TREE_TYPE (DECL_TEMPLATE_RESULT (olddecl)),
				   TREE_TYPE (DECL_TEMPLATE_RESULT (newdecl)));
      else
	types_match = decls_match (DECL_TEMPLATE_RESULT (olddecl),
				   DECL_TEMPLATE_RESULT (newdecl));
    }
  else
    {
      /* Need to check scope for variable declaration (VAR_DECL).
	 For typedef (TYPE_DECL), scope is ignored.  */
      if (TREE_CODE (newdecl) == VAR_DECL
	  && CP_DECL_CONTEXT (newdecl) != CP_DECL_CONTEXT (olddecl))
	return 0;

      if (TREE_TYPE (newdecl) == error_mark_node)
	types_match = TREE_TYPE (olddecl) == error_mark_node;
      else if (TREE_TYPE (olddecl) == NULL_TREE)
	types_match = TREE_TYPE (newdecl) == NULL_TREE;
      else if (TREE_TYPE (newdecl) == NULL_TREE)
	types_match = 0;
      else
	types_match = comptypes (TREE_TYPE (newdecl),
				 TREE_TYPE (olddecl),
				 COMPARE_REDECLARATION);
    }

  return types_match;
}

/* If NEWDECL is `static' and an `extern' was seen previously,
   warn about it.  OLDDECL is the previous declaration.

   Note that this does not apply to the C++ case of declaring
   a variable `extern const' and then later `const'.

   Don't complain about built-in functions, since they are beyond
   the user's control.  */

void
warn_extern_redeclared_static (tree newdecl, tree olddecl)
{
  static const char *const explicit_extern_static_warning
    = "`%D' was declared `extern' and later `static'";
  static const char *const implicit_extern_static_warning
    = "`%D' was declared implicitly `extern' and later `static'";

  tree name;

  if (TREE_CODE (newdecl) == TYPE_DECL
      || TREE_CODE (newdecl) == TEMPLATE_DECL
      || TREE_CODE (newdecl) == CONST_DECL
      || TREE_CODE (newdecl) == NAMESPACE_DECL)
    return;

  /* Don't get confused by static member functions; that's a different
     use of `static'.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL
      && DECL_STATIC_FUNCTION_P (newdecl))
    return;

  /* If the old declaration was `static', or the new one isn't, then
     then everything is OK.  */
  if (DECL_THIS_STATIC (olddecl) || !DECL_THIS_STATIC (newdecl))
    return;

  /* It's OK to declare a builtin function as `static'.  */
  if (TREE_CODE (olddecl) == FUNCTION_DECL
      && DECL_ARTIFICIAL (olddecl))
    return;

  name = DECL_ASSEMBLER_NAME (newdecl);
  pedwarn (IDENTIFIER_IMPLICIT_DECL (name)
	      ? implicit_extern_static_warning
	      : explicit_extern_static_warning, newdecl);
  cp_pedwarn_at ("previous declaration of `%D'", olddecl);
}

/* If NEWDECL is a redeclaration of OLDDECL, merge the declarations.
   If the redeclaration is invalid, a diagnostic is issued, and the
   error_mark_node is returned.  Otherwise, OLDDECL is returned.

   If NEWDECL is not a redeclaration of OLDDECL, NULL_TREE is
   returned.  */

tree
duplicate_decls (tree newdecl, tree olddecl)
{
  unsigned olddecl_uid = DECL_UID (olddecl);
  int olddecl_friend = 0, types_match = 0;
  int new_defines_function = 0;

  if (newdecl == olddecl)
    return olddecl;

  types_match = decls_match (newdecl, olddecl);

  /* If either the type of the new decl or the type of the old decl is an
     error_mark_node, then that implies that we have already issued an
     error (earlier) for some bogus type specification, and in that case,
     it is rather pointless to harass the user with yet more error message
     about the same declaration, so just pretend the types match here.  */
  if (TREE_TYPE (newdecl) == error_mark_node
      || TREE_TYPE (olddecl) == error_mark_node)
    types_match = 1;

  if (DECL_P (olddecl)
      && TREE_CODE (newdecl) == FUNCTION_DECL
      && TREE_CODE (olddecl) == FUNCTION_DECL
      && (DECL_UNINLINABLE (newdecl) || DECL_UNINLINABLE (olddecl)))
    {
      if (DECL_DECLARED_INLINE_P (newdecl)
	  && DECL_UNINLINABLE (newdecl)
	  && lookup_attribute ("noinline", DECL_ATTRIBUTES (newdecl)))
	/* Already warned elsewhere.  */;
      else if (DECL_DECLARED_INLINE_P (olddecl)
	       && DECL_UNINLINABLE (olddecl)
	       && lookup_attribute ("noinline", DECL_ATTRIBUTES (olddecl)))
	/* Already warned.  */;
      else if (DECL_DECLARED_INLINE_P (newdecl)
	       && DECL_UNINLINABLE (olddecl)
	       && lookup_attribute ("noinline", DECL_ATTRIBUTES (olddecl)))
	{
	  warning ("%Jfunction '%D' redeclared as inline", newdecl, newdecl);
	  warning ("%Jprevious declaration of '%D' with attribute noinline",
                   olddecl, olddecl);
	}
      else if (DECL_DECLARED_INLINE_P (olddecl)
	       && DECL_UNINLINABLE (newdecl)
	       && lookup_attribute ("noinline", DECL_ATTRIBUTES (newdecl)))
	{
	  warning ("%Jfunction '%D' redeclared with attribute noinline",
		   newdecl, newdecl);
	  warning ("%Jprevious declaration of '%D' was inline",
		   olddecl, olddecl);
	}
    }

  /* Check for redeclaration and other discrepancies.  */
  if (TREE_CODE (olddecl) == FUNCTION_DECL
      && DECL_ARTIFICIAL (olddecl))
    {
      if (TREE_CODE (newdecl) != FUNCTION_DECL)
	{
          /* Avoid warnings redeclaring anticipated built-ins.  */
          if (DECL_ANTICIPATED (olddecl))
            return NULL_TREE;

	  /* If you declare a built-in or predefined function name as static,
	     the old definition is overridden, but optionally warn this was a
	     bad choice of name.  */
	  if (! TREE_PUBLIC (newdecl))
	    {
	      if (warn_shadow)
		warning ("shadowing %s function `%#D'",
			    DECL_BUILT_IN (olddecl) ? "built-in" : "library",
			    olddecl);
	      /* Discard the old built-in function.  */
	      return NULL_TREE;
	    }
	  /* If the built-in is not ansi, then programs can override
	     it even globally without an error.  */
	  else if (! DECL_BUILT_IN (olddecl))
	    warning ("library function `%#D' redeclared as non-function `%#D'",
			olddecl, newdecl);
	  else
	    {
	      error ("declaration of `%#D'", newdecl);
	      error ("conflicts with built-in declaration `%#D'",
			olddecl);
	    }
	  return NULL_TREE;
	}
      else if (!types_match)
	{
          /* Avoid warnings redeclaring anticipated built-ins.  */
          if (DECL_ANTICIPATED (olddecl))
            ;  /* Do nothing yet.  */
	  else if ((DECL_EXTERN_C_P (newdecl)
		    && DECL_EXTERN_C_P (olddecl))
		   || compparms (TYPE_ARG_TYPES (TREE_TYPE (newdecl)),
				 TYPE_ARG_TYPES (TREE_TYPE (olddecl))))
	    {
	      /* A near match; override the builtin.  */

	      if (TREE_PUBLIC (newdecl))
		{
		  warning ("new declaration `%#D'", newdecl);
		  warning ("ambiguates built-in declaration `%#D'",
			      olddecl);
		}
	      else if (warn_shadow)
		warning ("shadowing %s function `%#D'",
			    DECL_BUILT_IN (olddecl) ? "built-in" : "library",
			    olddecl);
	    }
	  else
	    /* Discard the old built-in function.  */
	    return NULL_TREE;

	  /* Replace the old RTL to avoid problems with inlining.  */
	  SET_DECL_RTL (olddecl, DECL_RTL (newdecl));
	}
      /* Even if the types match, prefer the new declarations type
	 for anticipated built-ins, for exception lists, etc...  */
      else if (DECL_ANTICIPATED (olddecl))
	{
	  tree type = TREE_TYPE (newdecl);
	  tree attribs = (*targetm.merge_type_attributes)
	    (TREE_TYPE (olddecl), type);

	  type = cp_build_type_attribute_variant (type, attribs);
	  TREE_TYPE (newdecl) = TREE_TYPE (olddecl) = type;
	}

      /* Whether or not the builtin can throw exceptions has no
	 bearing on this declarator.  */
      TREE_NOTHROW (olddecl) = 0;

      if (DECL_THIS_STATIC (newdecl) && !DECL_THIS_STATIC (olddecl))
	{
	  /* If a builtin function is redeclared as `static', merge
	     the declarations, but make the original one static.  */
	  DECL_THIS_STATIC (olddecl) = 1;
	  TREE_PUBLIC (olddecl) = 0;

	  /* Make the old declaration consistent with the new one so
	     that all remnants of the builtin-ness of this function
	     will be banished.  */
	  SET_DECL_LANGUAGE (olddecl, DECL_LANGUAGE (newdecl));
	  SET_DECL_RTL (olddecl, DECL_RTL (newdecl));
	}
    }
  else if (TREE_CODE (olddecl) != TREE_CODE (newdecl))
    {
      if ((TREE_CODE (olddecl) == TYPE_DECL && DECL_ARTIFICIAL (olddecl)
	   && TREE_CODE (newdecl) != TYPE_DECL
	   && ! (TREE_CODE (newdecl) == TEMPLATE_DECL
		 && TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == TYPE_DECL))
	  || (TREE_CODE (newdecl) == TYPE_DECL && DECL_ARTIFICIAL (newdecl)
	      && TREE_CODE (olddecl) != TYPE_DECL
	      && ! (TREE_CODE (olddecl) == TEMPLATE_DECL
		    && (TREE_CODE (DECL_TEMPLATE_RESULT (olddecl))
			== TYPE_DECL))))
	{
	  /* We do nothing special here, because C++ does such nasty
	     things with TYPE_DECLs.  Instead, just let the TYPE_DECL
	     get shadowed, and know that if we need to find a TYPE_DECL
	     for a given name, we can look in the IDENTIFIER_TYPE_VALUE
	     slot of the identifier.  */
	  return NULL_TREE;
	}

      if ((TREE_CODE (newdecl) == FUNCTION_DECL
	   && DECL_FUNCTION_TEMPLATE_P (olddecl))
	  || (TREE_CODE (olddecl) == FUNCTION_DECL
	      && DECL_FUNCTION_TEMPLATE_P (newdecl)))
	return NULL_TREE;

      error ("`%#D' redeclared as different kind of symbol", newdecl);
      if (TREE_CODE (olddecl) == TREE_LIST)
	olddecl = TREE_VALUE (olddecl);
      cp_error_at ("previous declaration of `%#D'", olddecl);

      /* New decl is completely inconsistent with the old one =>
	 tell caller to replace the old one.  */

      return NULL_TREE;
    }
  else if (!types_match)
    {
      if (CP_DECL_CONTEXT (newdecl) != CP_DECL_CONTEXT (olddecl))
	/* These are certainly not duplicate declarations; they're
	   from different scopes.  */
	return NULL_TREE;

      if (TREE_CODE (newdecl) == TEMPLATE_DECL)
	{
	  /* The name of a class template may not be declared to refer to
	     any other template, class, function, object, namespace, value,
	     or type in the same scope.  */
	  if (TREE_CODE (DECL_TEMPLATE_RESULT (olddecl)) == TYPE_DECL
	      || TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == TYPE_DECL)
	    {
	      error ("declaration of template `%#D'", newdecl);
	      cp_error_at ("conflicts with previous declaration `%#D'",
			   olddecl);
	    }
	  else if (TREE_CODE (DECL_TEMPLATE_RESULT (olddecl)) == FUNCTION_DECL
		   && TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == FUNCTION_DECL
		   && compparms (TYPE_ARG_TYPES (TREE_TYPE (DECL_TEMPLATE_RESULT (olddecl))),
				 TYPE_ARG_TYPES (TREE_TYPE (DECL_TEMPLATE_RESULT (newdecl))))
		   && comp_template_parms (DECL_TEMPLATE_PARMS (newdecl),
					   DECL_TEMPLATE_PARMS (olddecl))
		   /* Template functions can be disambiguated by
		      return type.  */
		   && same_type_p (TREE_TYPE (TREE_TYPE (newdecl)),
				   TREE_TYPE (TREE_TYPE (olddecl))))
	    {
	      error ("new declaration `%#D'", newdecl);
	      cp_error_at ("ambiguates old declaration `%#D'", olddecl);
	    }
	  return NULL_TREE;
	}
      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  if (DECL_EXTERN_C_P (newdecl) && DECL_EXTERN_C_P (olddecl))
	    {
	      error ("declaration of C function `%#D' conflicts with",
			newdecl);
	      cp_error_at ("previous declaration `%#D' here", olddecl);
	    }
	  else if (compparms (TYPE_ARG_TYPES (TREE_TYPE (newdecl)),
			      TYPE_ARG_TYPES (TREE_TYPE (olddecl))))
	    {
	      error ("new declaration `%#D'", newdecl);
	      cp_error_at ("ambiguates old declaration `%#D'", olddecl);
	    }
	  else
	    return NULL_TREE;
	}

      /* Already complained about this, so don't do so again.  */
      else if (current_class_type == NULL_TREE
	  || IDENTIFIER_ERROR_LOCUS (DECL_ASSEMBLER_NAME (newdecl)) != current_class_type)
	{
	  error ("conflicting declaration '%#D'", newdecl);
	  cp_error_at ("'%D' has a previous declaration as `%#D'",
                       olddecl, olddecl);
          return NULL_TREE;
	}
    }
  else if (TREE_CODE (newdecl) == FUNCTION_DECL
	    && ((DECL_TEMPLATE_SPECIALIZATION (olddecl)
		 && (!DECL_TEMPLATE_INFO (newdecl)
		     || (DECL_TI_TEMPLATE (newdecl)
			 != DECL_TI_TEMPLATE (olddecl))))
		|| (DECL_TEMPLATE_SPECIALIZATION (newdecl)
		    && (!DECL_TEMPLATE_INFO (olddecl)
			|| (DECL_TI_TEMPLATE (olddecl)
			    != DECL_TI_TEMPLATE (newdecl))))))
    /* It's OK to have a template specialization and a non-template
       with the same type, or to have specializations of two
       different templates with the same type.  Note that if one is a
       specialization, and the other is an instantiation of the same
       template, that we do not exit at this point.  That situation
       can occur if we instantiate a template class, and then
       specialize one of its methods.  This situation is valid, but
       the declarations must be merged in the usual way.  */
    return NULL_TREE;
  else if (TREE_CODE (newdecl) == FUNCTION_DECL
	   && ((DECL_TEMPLATE_INSTANTIATION (olddecl)
		&& !DECL_USE_TEMPLATE (newdecl))
	       || (DECL_TEMPLATE_INSTANTIATION (newdecl)
		   && !DECL_USE_TEMPLATE (olddecl))))
    /* One of the declarations is a template instantiation, and the
       other is not a template at all.  That's OK.  */
    return NULL_TREE;
  else if (TREE_CODE (newdecl) == NAMESPACE_DECL)
    {
      /* In [namespace.alias] we have:
	 
           In a declarative region, a namespace-alias-definition can be
	   used to redefine a namespace-alias declared in that declarative
	   region to refer only to the namespace to which it already
	   refers.
	   
	 Therefore, if we encounter a second alias directive for the same
	 alias, we can just ignore the second directive.  */
      if (DECL_NAMESPACE_ALIAS (newdecl)
	  && (DECL_NAMESPACE_ALIAS (newdecl) 
	      == DECL_NAMESPACE_ALIAS (olddecl)))
	return olddecl;
      /* [namespace.alias]

         A namespace-name or namespace-alias shall not be declared as
	 the name of any other entity in the same declarative region.
	 A namespace-name defined at global scope shall not be
	 declared as the name of any other entity in any glogal scope
	 of the program.  */
      error ("declaration of `namespace %D' conflicts with", newdecl);
      cp_error_at ("previous declaration of `namespace %D' here", olddecl);
      return error_mark_node;
    }
  else
    {
      const char *errmsg = redeclaration_error_message (newdecl, olddecl);
      if (errmsg)
	{
	  error (errmsg, newdecl);
	  if (DECL_NAME (olddecl) != NULL_TREE)
	    cp_error_at ((DECL_INITIAL (olddecl)
			  && namespace_bindings_p ())
			 ? "`%#D' previously defined here"
			 : "`%#D' previously declared here", olddecl);
	  return error_mark_node;
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_INITIAL (olddecl) != NULL_TREE
	       && TYPE_ARG_TYPES (TREE_TYPE (olddecl)) == NULL_TREE
	       && TYPE_ARG_TYPES (TREE_TYPE (newdecl)) != NULL_TREE)
	{
	  /* Prototype decl follows defn w/o prototype.  */
	  cp_warning_at ("prototype for `%#D'", newdecl);
	  warning ("%Jfollows non-prototype definition here", olddecl);
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_LANGUAGE (newdecl) != DECL_LANGUAGE (olddecl))
	{
	  /* extern "C" int foo ();
	     int foo () { bar (); }
	     is OK.  */
	  if (current_lang_depth () == 0)
	    SET_DECL_LANGUAGE (newdecl, DECL_LANGUAGE (olddecl));
	  else
	    {
	      cp_error_at ("previous declaration of `%#D' with %L linkage",
			   olddecl, DECL_LANGUAGE (olddecl));
	      error ("conflicts with new declaration with %L linkage",
			DECL_LANGUAGE (newdecl));
	    }
	}

      if (DECL_LANG_SPECIFIC (olddecl) && DECL_USE_TEMPLATE (olddecl))
	;
      else if (TREE_CODE (olddecl) == FUNCTION_DECL)
	{
	  tree t1 = TYPE_ARG_TYPES (TREE_TYPE (olddecl));
	  tree t2 = TYPE_ARG_TYPES (TREE_TYPE (newdecl));
	  int i = 1;

	  if (TREE_CODE (TREE_TYPE (newdecl)) == METHOD_TYPE)
	    t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2);

	  for (; t1 && t1 != void_list_node;
	       t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2), i++)
	    if (TREE_PURPOSE (t1) && TREE_PURPOSE (t2))
	      {
		if (1 == simple_cst_equal (TREE_PURPOSE (t1),
					   TREE_PURPOSE (t2)))
		  {
		    pedwarn ("default argument given for parameter %d of `%#D'",
			     i, newdecl);
		    cp_pedwarn_at ("after previous specification in `%#D'",
			           olddecl);
		  }
		else
		  {
		    error ("default argument given for parameter %d of `%#D'",
			      i, newdecl);
		    cp_error_at ("after previous specification in `%#D'",
				 olddecl);
		  }
	      }

	  if (DECL_DECLARED_INLINE_P (newdecl) 
	      && ! DECL_DECLARED_INLINE_P (olddecl)
	      && TREE_ADDRESSABLE (olddecl) && warn_inline)
	    {
	      warning ("`%#D' was used before it was declared inline", newdecl);
	      warning ("%Jprevious non-inline declaration here", olddecl);
	    }
	}
    }

  /* Do not merge an implicit typedef with an explicit one.  In:

       class A;
       ...
       typedef class A A __attribute__ ((foo));

     the attribute should apply only to the typedef.  */
  if (TREE_CODE (olddecl) == TYPE_DECL
      && (DECL_IMPLICIT_TYPEDEF_P (olddecl)
	  || DECL_IMPLICIT_TYPEDEF_P (newdecl)))
    return NULL_TREE;

  /* If new decl is `static' and an `extern' was seen previously,
     warn about it.  */
  warn_extern_redeclared_static (newdecl, olddecl);

  /* We have committed to returning 1 at this point.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* Now that functions must hold information normally held
	 by field decls, there is extra work to do so that
	 declaration information does not get destroyed during
	 definition.  */
      if (DECL_VINDEX (olddecl))
	DECL_VINDEX (newdecl) = DECL_VINDEX (olddecl);
      if (DECL_CONTEXT (olddecl))
	DECL_CONTEXT (newdecl) = DECL_CONTEXT (olddecl);
      DECL_STATIC_CONSTRUCTOR (newdecl) |= DECL_STATIC_CONSTRUCTOR (olddecl);
      DECL_STATIC_DESTRUCTOR (newdecl) |= DECL_STATIC_DESTRUCTOR (olddecl);
      DECL_PURE_VIRTUAL_P (newdecl) |= DECL_PURE_VIRTUAL_P (olddecl);
      DECL_VIRTUAL_P (newdecl) |= DECL_VIRTUAL_P (olddecl);
      DECL_NEEDS_FINAL_OVERRIDER_P (newdecl) |= DECL_NEEDS_FINAL_OVERRIDER_P (olddecl);
      DECL_THIS_STATIC (newdecl) |= DECL_THIS_STATIC (olddecl);
      if (DECL_OVERLOADED_OPERATOR_P (olddecl) != ERROR_MARK)
	SET_OVERLOADED_OPERATOR_CODE
	  (newdecl, DECL_OVERLOADED_OPERATOR_P (olddecl));
      new_defines_function = DECL_INITIAL (newdecl) != NULL_TREE;

      /* Optionally warn about more than one declaration for the same
         name, but don't warn about a function declaration followed by a
         definition.  */
      if (warn_redundant_decls && ! DECL_ARTIFICIAL (olddecl)
	  && !(new_defines_function && DECL_INITIAL (olddecl) == NULL_TREE)
	  /* Don't warn about extern decl followed by definition.  */
	  && !(DECL_EXTERNAL (olddecl) && ! DECL_EXTERNAL (newdecl))
	  /* Don't warn about friends, let add_friend take care of it.  */
	  && ! (DECL_FRIEND_P (newdecl) || DECL_FRIEND_P (olddecl)))
	{
	  warning ("redundant redeclaration of `%D' in same scope", newdecl);
	  cp_warning_at ("previous declaration of `%D'", olddecl);
	}
    }

  /* Deal with C++: must preserve virtual function table size.  */
  if (TREE_CODE (olddecl) == TYPE_DECL)
    {
      tree newtype = TREE_TYPE (newdecl);
      tree oldtype = TREE_TYPE (olddecl);

      if (newtype != error_mark_node && oldtype != error_mark_node
	  && TYPE_LANG_SPECIFIC (newtype) && TYPE_LANG_SPECIFIC (oldtype))
	CLASSTYPE_FRIEND_CLASSES (newtype)
	  = CLASSTYPE_FRIEND_CLASSES (oldtype);

      DECL_ORIGINAL_TYPE (newdecl) = DECL_ORIGINAL_TYPE (olddecl);
    }

  /* Copy all the DECL_... slots specified in the new decl
     except for any that we copy here from the old type.  */
  DECL_ATTRIBUTES (newdecl)
    = (*targetm.merge_decl_attributes) (olddecl, newdecl);

  if (TREE_CODE (newdecl) == TEMPLATE_DECL)
    {
      TREE_TYPE (olddecl) = TREE_TYPE (DECL_TEMPLATE_RESULT (olddecl));
      DECL_TEMPLATE_SPECIALIZATIONS (olddecl)
	= chainon (DECL_TEMPLATE_SPECIALIZATIONS (olddecl),
		   DECL_TEMPLATE_SPECIALIZATIONS (newdecl));

      /* If the new declaration is a definition, update the file and
	 line information on the declaration.  */
      if (DECL_INITIAL (DECL_TEMPLATE_RESULT (olddecl)) == NULL_TREE
	  && DECL_INITIAL (DECL_TEMPLATE_RESULT (newdecl)) != NULL_TREE)
	{
	  DECL_SOURCE_LOCATION (olddecl) 
	    = DECL_SOURCE_LOCATION (DECL_TEMPLATE_RESULT (olddecl))
	    = DECL_SOURCE_LOCATION (newdecl);
	  if (DECL_FUNCTION_TEMPLATE_P (newdecl))
	    DECL_ARGUMENTS (DECL_TEMPLATE_RESULT (olddecl))
	      = DECL_ARGUMENTS (DECL_TEMPLATE_RESULT (newdecl));
	}

      if (DECL_FUNCTION_TEMPLATE_P (newdecl))
	{
	  DECL_INLINE (DECL_TEMPLATE_RESULT (olddecl)) 
	    |= DECL_INLINE (DECL_TEMPLATE_RESULT (newdecl));
	  DECL_DECLARED_INLINE_P (DECL_TEMPLATE_RESULT (olddecl))
	    |= DECL_DECLARED_INLINE_P (DECL_TEMPLATE_RESULT (newdecl));
	}

      return olddecl;
    }

  if (types_match)
    {
      /* Automatically handles default parameters.  */
      tree oldtype = TREE_TYPE (olddecl);
      tree newtype;

      /* Merge the data types specified in the two decls.  */
      newtype = merge_types (TREE_TYPE (newdecl), TREE_TYPE (olddecl));

      /* If merge_types produces a non-typedef type, just use the old type.  */
      if (TREE_CODE (newdecl) == TYPE_DECL
	  && newtype == DECL_ORIGINAL_TYPE (newdecl))
	newtype = oldtype;

      if (TREE_CODE (newdecl) == VAR_DECL)
	{
	  DECL_THIS_EXTERN (newdecl) |= DECL_THIS_EXTERN (olddecl);
	  DECL_INITIALIZED_P (newdecl) |= DECL_INITIALIZED_P (olddecl);
	  DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (newdecl)
	    |= DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (olddecl);
	}

      /* Do this after calling `merge_types' so that default
	 parameters don't confuse us.  */
      else if (TREE_CODE (newdecl) == FUNCTION_DECL
	  && (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (newdecl))
	      != TYPE_RAISES_EXCEPTIONS (TREE_TYPE (olddecl))))
	{
	  TREE_TYPE (newdecl) = build_exception_variant (newtype,
							 TYPE_RAISES_EXCEPTIONS (TREE_TYPE (newdecl)));
	  TREE_TYPE (olddecl) = build_exception_variant (newtype,
							 TYPE_RAISES_EXCEPTIONS (oldtype));

	  if ((pedantic || ! DECL_IN_SYSTEM_HEADER (olddecl))
	      && DECL_SOURCE_LINE (olddecl) != 0
	      && flag_exceptions
	      && !comp_except_specs (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (newdecl)),
	                             TYPE_RAISES_EXCEPTIONS (TREE_TYPE (olddecl)), 1))
	    {
	      error ("declaration of `%F' throws different exceptions",
			newdecl);
	      cp_error_at ("than previous declaration `%F'", olddecl);
	    }
	}
      TREE_TYPE (newdecl) = TREE_TYPE (olddecl) = newtype;

      /* Lay the type out, unless already done.  */
      if (! same_type_p (newtype, oldtype)
	  && TREE_TYPE (newdecl) != error_mark_node
	  && !(processing_template_decl && uses_template_parms (newdecl)))
	layout_type (TREE_TYPE (newdecl));

      if ((TREE_CODE (newdecl) == VAR_DECL
	   || TREE_CODE (newdecl) == PARM_DECL
	   || TREE_CODE (newdecl) == RESULT_DECL
	   || TREE_CODE (newdecl) == FIELD_DECL
	   || TREE_CODE (newdecl) == TYPE_DECL)
	  && !(processing_template_decl && uses_template_parms (newdecl)))
	layout_decl (newdecl, 0);

      /* Merge the type qualifiers.  */
      if (TREE_READONLY (newdecl))
	TREE_READONLY (olddecl) = 1;
      if (TREE_THIS_VOLATILE (newdecl))
	TREE_THIS_VOLATILE (olddecl) = 1;

      /* Merge the initialization information.  */
      if (DECL_INITIAL (newdecl) == NULL_TREE
	  && DECL_INITIAL (olddecl) != NULL_TREE)
	{
	  DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
	  DECL_SOURCE_LOCATION (newdecl) = DECL_SOURCE_LOCATION (olddecl);
	  if (CAN_HAVE_FULL_LANG_DECL_P (newdecl)
	      && DECL_LANG_SPECIFIC (newdecl)
	      && DECL_LANG_SPECIFIC (olddecl))
	    {
	      DECL_SAVED_TREE (newdecl) = DECL_SAVED_TREE (olddecl);
	      DECL_SAVED_INSNS (newdecl) = DECL_SAVED_INSNS (olddecl);
	    }
	}

      /* Merge the section attribute.
         We want to issue an error if the sections conflict but that must be
	 done later in decl_attributes since we are called before attributes
	 are assigned.  */
      if (DECL_SECTION_NAME (newdecl) == NULL_TREE)
	DECL_SECTION_NAME (newdecl) = DECL_SECTION_NAME (olddecl);

      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (newdecl)
	    |= DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (olddecl);
	  DECL_NO_LIMIT_STACK (newdecl) |= DECL_NO_LIMIT_STACK (olddecl);
	  TREE_THIS_VOLATILE (newdecl) |= TREE_THIS_VOLATILE (olddecl);
	  TREE_READONLY (newdecl) |= TREE_READONLY (olddecl);
	  TREE_NOTHROW (newdecl) |= TREE_NOTHROW (olddecl);
	  DECL_IS_MALLOC (newdecl) |= DECL_IS_MALLOC (olddecl);
	  DECL_IS_PURE (newdecl) |= DECL_IS_PURE (olddecl);
	  /* Keep the old RTL.  */
	  COPY_DECL_RTL (olddecl, newdecl);
	}
      else if (TREE_CODE (newdecl) == VAR_DECL 
	       && (DECL_SIZE (olddecl) || !DECL_SIZE (newdecl)))
	{
	  /* Keep the old RTL.  We cannot keep the old RTL if the old
	     declaration was for an incomplete object and the new
	     declaration is not since many attributes of the RTL will
	     change.  */
	  COPY_DECL_RTL (olddecl, newdecl);
	}
    }
  /* If cannot merge, then use the new type and qualifiers,
     and don't preserve the old rtl.  */
  else
    {
      /* Clean out any memory we had of the old declaration.  */
      tree oldstatic = value_member (olddecl, static_aggregates);
      if (oldstatic)
	TREE_VALUE (oldstatic) = error_mark_node;

      TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
      TREE_READONLY (olddecl) = TREE_READONLY (newdecl);
      TREE_THIS_VOLATILE (olddecl) = TREE_THIS_VOLATILE (newdecl);
      TREE_SIDE_EFFECTS (olddecl) = TREE_SIDE_EFFECTS (newdecl);
    }

  /* Merge the storage class information.  */
  merge_weak (newdecl, olddecl);

  DECL_ONE_ONLY (newdecl) |= DECL_ONE_ONLY (olddecl);
  DECL_DEFER_OUTPUT (newdecl) |= DECL_DEFER_OUTPUT (olddecl);
  TREE_PUBLIC (newdecl) = TREE_PUBLIC (olddecl);
  TREE_STATIC (olddecl) = TREE_STATIC (newdecl) |= TREE_STATIC (olddecl);
  if (! DECL_EXTERNAL (olddecl))
    DECL_EXTERNAL (newdecl) = 0;

  if (DECL_LANG_SPECIFIC (newdecl) && DECL_LANG_SPECIFIC (olddecl))
    {
      DECL_INTERFACE_KNOWN (newdecl) |= DECL_INTERFACE_KNOWN (olddecl);
      DECL_NOT_REALLY_EXTERN (newdecl) |= DECL_NOT_REALLY_EXTERN (olddecl);
      DECL_COMDAT (newdecl) |= DECL_COMDAT (olddecl);
      DECL_TEMPLATE_INSTANTIATED (newdecl)
	|= DECL_TEMPLATE_INSTANTIATED (olddecl);
      /* Don't really know how much of the language-specific
	 values we should copy from old to new.  */
      DECL_IN_AGGR_P (newdecl) = DECL_IN_AGGR_P (olddecl);
      DECL_LANG_SPECIFIC (newdecl)->decl_flags.u2 = 
	DECL_LANG_SPECIFIC (olddecl)->decl_flags.u2;
      DECL_NONCONVERTING_P (newdecl) = DECL_NONCONVERTING_P (olddecl);
      DECL_TEMPLATE_INFO (newdecl) = DECL_TEMPLATE_INFO (olddecl);
      DECL_INITIALIZED_IN_CLASS_P (newdecl)
        |= DECL_INITIALIZED_IN_CLASS_P (olddecl);
      olddecl_friend = DECL_FRIEND_P (olddecl);

      /* Only functions have DECL_BEFRIENDING_CLASSES.  */
      if (TREE_CODE (newdecl) == FUNCTION_DECL
	  || DECL_FUNCTION_TEMPLATE_P (newdecl))
	{
	  DECL_BEFRIENDING_CLASSES (newdecl)
	    = chainon (DECL_BEFRIENDING_CLASSES (newdecl),
		       DECL_BEFRIENDING_CLASSES (olddecl));
	  /* DECL_THUNKS is only valid for virtual functions,
	     otherwise it is a DECL_FRIEND_CONTEXT.  */
	  if (DECL_VIRTUAL_P (newdecl))
	    DECL_THUNKS (newdecl) = DECL_THUNKS (olddecl);
	}
    }

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      if (DECL_TEMPLATE_INSTANTIATION (olddecl)
	  && !DECL_TEMPLATE_INSTANTIATION (newdecl))
	{
	  /* If newdecl is not a specialization, then it is not a
	     template-related function at all.  And that means that we
	     should have exited above, returning 0.  */
	  my_friendly_assert (DECL_TEMPLATE_SPECIALIZATION (newdecl),
			      0);

	  if (TREE_USED (olddecl))
	    /* From [temp.expl.spec]:

	       If a template, a member template or the member of a class
	       template is explicitly specialized then that
	       specialization shall be declared before the first use of
	       that specialization that would cause an implicit
	       instantiation to take place, in every translation unit in
	       which such a use occurs.  */
	    error ("explicit specialization of %D after first use",
		      olddecl);

	  SET_DECL_TEMPLATE_SPECIALIZATION (olddecl);

	  /* [temp.expl.spec/14] We don't inline explicit specialization
	     just because the primary template says so.  */
	}
      else
	{
	  if (DECL_PENDING_INLINE_INFO (newdecl) == 0)
	    DECL_PENDING_INLINE_INFO (newdecl) = DECL_PENDING_INLINE_INFO (olddecl);

	  DECL_DECLARED_INLINE_P (newdecl) |= DECL_DECLARED_INLINE_P (olddecl);

	  /* If either decl says `inline', this fn is inline, unless 
	     its definition was passed already.  */
	  if (DECL_INLINE (newdecl) && DECL_INITIAL (olddecl) == NULL_TREE)
	    DECL_INLINE (olddecl) = 1;
	  DECL_INLINE (newdecl) = DECL_INLINE (olddecl);

	  DECL_UNINLINABLE (newdecl) = DECL_UNINLINABLE (olddecl)
	    = (DECL_UNINLINABLE (newdecl) || DECL_UNINLINABLE (olddecl));
	}

      /* Preserve abstractness on cloned [cd]tors.  */
      DECL_ABSTRACT (newdecl) = DECL_ABSTRACT (olddecl);

      if (! types_match)
	{
	  SET_DECL_LANGUAGE (olddecl, DECL_LANGUAGE (newdecl));
	  COPY_DECL_ASSEMBLER_NAME (newdecl, olddecl);
	  SET_DECL_RTL (olddecl, DECL_RTL (newdecl));
	}
      if (! types_match || new_defines_function)
	{
	  /* These need to be copied so that the names are available.
	     Note that if the types do match, we'll preserve inline
	     info and other bits, but if not, we won't.  */
	  DECL_ARGUMENTS (olddecl) = DECL_ARGUMENTS (newdecl);
	  DECL_RESULT (olddecl) = DECL_RESULT (newdecl);
	}
      if (new_defines_function)
	/* If defining a function declared with other language
	   linkage, use the previously declared language linkage.  */
	SET_DECL_LANGUAGE (newdecl, DECL_LANGUAGE (olddecl));
      else if (types_match)
	{
	  /* If redeclaring a builtin function, and not a definition,
	     it stays built in.  */
	  if (DECL_BUILT_IN (olddecl))
	    {
	      DECL_BUILT_IN_CLASS (newdecl) = DECL_BUILT_IN_CLASS (olddecl);
	      DECL_FUNCTION_CODE (newdecl) = DECL_FUNCTION_CODE (olddecl);
	      /* If we're keeping the built-in definition, keep the rtl,
		 regardless of declaration matches.  */
	      SET_DECL_RTL (newdecl, DECL_RTL (olddecl));
	    }

	  DECL_RESULT (newdecl) = DECL_RESULT (olddecl);
	  /* Don't clear out the arguments if we're redefining a function.  */
	  if (DECL_ARGUMENTS (olddecl))
	    DECL_ARGUMENTS (newdecl) = DECL_ARGUMENTS (olddecl);
	}
    }
  else if (TREE_CODE (newdecl) == NAMESPACE_DECL)
    NAMESPACE_LEVEL (newdecl) = NAMESPACE_LEVEL (olddecl);

  /* Now preserve various other info from the definition.  */
  TREE_ADDRESSABLE (newdecl) = TREE_ADDRESSABLE (olddecl);
  TREE_ASM_WRITTEN (newdecl) = TREE_ASM_WRITTEN (olddecl);
  DECL_COMMON (newdecl) = DECL_COMMON (olddecl);
  COPY_DECL_ASSEMBLER_NAME (olddecl, newdecl);

  /* If either declaration has a nondefault visibility, use it.  */
  if (DECL_VISIBILITY (olddecl) != VISIBILITY_DEFAULT)
    {
      if (DECL_VISIBILITY (newdecl) != VISIBILITY_DEFAULT
	  && DECL_VISIBILITY (newdecl) != DECL_VISIBILITY (olddecl))
	{
	  warning ("%J'%D': visibility attribute ignored because it",
		   newdecl, newdecl);
	  warning ("%Jconflicts with previous declaration here", olddecl);
	}
      DECL_VISIBILITY (newdecl) = DECL_VISIBILITY (olddecl);
    }

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      int function_size;

      function_size = sizeof (struct tree_decl);

      memcpy ((char *) olddecl + sizeof (struct tree_common),
	      (char *) newdecl + sizeof (struct tree_common),
	      function_size - sizeof (struct tree_common));

      if (DECL_TEMPLATE_INSTANTIATION (newdecl))
	/* If newdecl is a template instantiation, it is possible that
	   the following sequence of events has occurred:

	   o A friend function was declared in a class template.  The
	   class template was instantiated.

	   o The instantiation of the friend declaration was
	   recorded on the instantiation list, and is newdecl.

	   o Later, however, instantiate_class_template called pushdecl
	   on the newdecl to perform name injection.  But, pushdecl in
	   turn called duplicate_decls when it discovered that another
	   declaration of a global function with the same name already
	   existed.

	   o Here, in duplicate_decls, we decided to clobber newdecl.

	   If we're going to do that, we'd better make sure that
	   olddecl, and not newdecl, is on the list of
	   instantiations so that if we try to do the instantiation
	   again we won't get the clobbered declaration.  */
	reregister_specialization (newdecl, 
				   DECL_TI_TEMPLATE (newdecl), 
				   olddecl);
    }
  else
    {
      memcpy ((char *) olddecl + sizeof (struct tree_common),
	      (char *) newdecl + sizeof (struct tree_common),
	      sizeof (struct tree_decl) - sizeof (struct tree_common)
	      + TREE_CODE_LENGTH (TREE_CODE (newdecl)) * sizeof (char *));
    }

  DECL_UID (olddecl) = olddecl_uid;
  if (olddecl_friend)
    DECL_FRIEND_P (olddecl) = 1;

  /* NEWDECL contains the merged attribute lists.
     Update OLDDECL to be the same.  */
  DECL_ATTRIBUTES (olddecl) = DECL_ATTRIBUTES (newdecl);

  /* If OLDDECL had its DECL_RTL instantiated, re-invoke make_decl_rtl
    so that encode_section_info has a chance to look at the new decl
    flags and attributes.  */
  if (DECL_RTL_SET_P (olddecl) 
      && (TREE_CODE (olddecl) == FUNCTION_DECL
	  || (TREE_CODE (olddecl) == VAR_DECL
	      && TREE_STATIC (olddecl))))
    make_decl_rtl (olddecl, NULL);

  return olddecl;
}

/* Generate an implicit declaration for identifier FUNCTIONID
   as a function of type int ().  Print a warning if appropriate.  */

tree
implicitly_declare (tree functionid)
{
  tree decl;

  /* We used to reuse an old implicit decl here,
     but this loses with inline functions because it can clobber
     the saved decl chains.  */
  decl = build_lang_decl (FUNCTION_DECL, functionid, default_function_type);

  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  /* ISO standard says implicit declarations are in the innermost block.
     So we record the decl in the standard fashion.  */
  pushdecl (decl);
  rest_of_decl_compilation (decl, NULL, 0, 0);

  if (warn_implicit
      /* Only one warning per identifier.  */
      && IDENTIFIER_IMPLICIT_DECL (functionid) == NULL_TREE)
    {
      pedwarn ("implicit declaration of function `%#D'", decl);
    }

  SET_IDENTIFIER_IMPLICIT_DECL (functionid, decl);

  return decl;
}

/* Return zero if the declaration NEWDECL is valid
   when the declaration OLDDECL (assumed to be for the same name)
   has already been seen.
   Otherwise return an error message format string with a %s
   where the identifier should go.  */

static const char *
redeclaration_error_message (tree newdecl, tree olddecl)
{
  if (TREE_CODE (newdecl) == TYPE_DECL)
    {
      /* Because C++ can put things into name space for free,
	 constructs like "typedef struct foo { ... } foo"
	 would look like an erroneous redeclaration.  */
      if (same_type_p (TREE_TYPE (newdecl), TREE_TYPE (olddecl)))
	return 0;
      else
	return "redefinition of `%#D'";
    }
  else if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* If this is a pure function, its olddecl will actually be
	 the original initialization to `0' (which we force to call
	 abort()).  Don't complain about redefinition in this case.  */
      if (DECL_LANG_SPECIFIC (olddecl) && DECL_PURE_VIRTUAL_P (olddecl))
	return 0;

      /* If both functions come from different namespaces, this is not
	 a redeclaration - this is a conflict with a used function.  */
      if (DECL_NAMESPACE_SCOPE_P (olddecl)
	  && DECL_CONTEXT (olddecl) != DECL_CONTEXT (newdecl)
	  && ! decls_match (olddecl, newdecl))
	return "`%D' conflicts with used function";

      /* We'll complain about linkage mismatches in
         warn_extern_redeclared_static.  */

      /* Defining the same name twice is no good.  */
      if (DECL_INITIAL (olddecl) != NULL_TREE
	  && DECL_INITIAL (newdecl) != NULL_TREE)
	{
	  if (DECL_NAME (olddecl) == NULL_TREE)
	    return "`%#D' not declared in class";
	  else
	    return "redefinition of `%#D'";
	}
      return 0;
    }
  else if (TREE_CODE (newdecl) == TEMPLATE_DECL)
    {
      tree nt, ot;

      if (TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == TYPE_DECL)
	{
	  if (COMPLETE_TYPE_P (TREE_TYPE (newdecl))
	      && COMPLETE_TYPE_P (TREE_TYPE (olddecl)))
	    return "redefinition of `%#D'";
	  return NULL;
	}

      if (TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) != FUNCTION_DECL
	  || (DECL_TEMPLATE_RESULT (newdecl) 
	      == DECL_TEMPLATE_RESULT (olddecl)))
	return NULL;

      nt = DECL_TEMPLATE_RESULT (newdecl);
      if (DECL_TEMPLATE_INFO (nt))
	nt = DECL_TEMPLATE_RESULT (template_for_substitution (nt));
      ot = DECL_TEMPLATE_RESULT (olddecl);
      if (DECL_TEMPLATE_INFO (ot))
	ot = DECL_TEMPLATE_RESULT (template_for_substitution (ot));
      if (DECL_INITIAL (nt) && DECL_INITIAL (ot))
	return "redefinition of `%#D'";

      return NULL;
    }
  else if (toplevel_bindings_p () || DECL_NAMESPACE_SCOPE_P (newdecl))
    {
      /* Objects declared at top level:  */
      /* If at least one is a reference, it's ok.  */
      if (DECL_EXTERNAL (newdecl) || DECL_EXTERNAL (olddecl))
	return 0;
      /* Reject two definitions.  */
      return "redefinition of `%#D'";
    }
  else
    {
      /* Objects declared with block scope:  */
      /* Reject two definitions, and reject a definition
	 together with an external reference.  */
      if (!(DECL_EXTERNAL (newdecl) && DECL_EXTERNAL (olddecl)))
	return "redeclaration of `%#D'";
      return 0;
    }
}

/* Create a new label, named ID.  */

static tree
make_label_decl (tree id, int local_p)
{
  tree decl;

  decl = build_decl (LABEL_DECL, id, void_type_node);

  DECL_CONTEXT (decl) = current_function_decl;
  DECL_MODE (decl) = VOIDmode;
  C_DECLARED_LABEL_FLAG (decl) = local_p;

  /* Say where one reference is to the label, for the sake of the
     error if it is not defined.  */
  DECL_SOURCE_LOCATION (decl) = input_location;

  /* Record the fact that this identifier is bound to this label.  */
  SET_IDENTIFIER_LABEL_VALUE (id, decl);

  return decl;
}

/* Record this label on the list of used labels so that we can check
   at the end of the function to see whether or not the label was
   actually defined, and so we can check when the label is defined whether
   this use is valid.  */

static void
use_label (tree decl)
{
  if (named_label_uses == NULL
      || named_label_uses->names_in_scope != current_binding_level->names
      || named_label_uses->label_decl != decl)
    {
      struct named_label_use_list *new_ent;
      new_ent = ggc_alloc (sizeof (struct named_label_use_list));
      new_ent->label_decl = decl;
      new_ent->names_in_scope = current_binding_level->names;
      new_ent->binding_level = current_binding_level;
      new_ent->o_goto_locus = input_location;
      new_ent->next = named_label_uses;
      named_label_uses = new_ent;
    }
}

/* Look for a label named ID in the current function.  If one cannot
   be found, create one.  (We keep track of used, but undefined,
   labels, and complain about them at the end of a function.)  */

tree
lookup_label (tree id)
{
  tree decl;
  struct named_label_list *ent;

  timevar_push (TV_NAME_LOOKUP);
  /* You can't use labels at global scope.  */
  if (current_function_decl == NULL_TREE)
    {
      error ("label `%s' referenced outside of any function",
	     IDENTIFIER_POINTER (id));
      POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
    }

  /* See if we've already got this label.  */
  decl = IDENTIFIER_LABEL_VALUE (id);
  if (decl != NULL_TREE && DECL_CONTEXT (decl) == current_function_decl)
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, decl);

  /* Record this label on the list of labels used in this function.
     We do this before calling make_label_decl so that we get the
     IDENTIFIER_LABEL_VALUE before the new label is declared.  */
  ent = ggc_alloc_cleared (sizeof (struct named_label_list));
  ent->old_value = IDENTIFIER_LABEL_VALUE (id);
  ent->next = named_labels;
  named_labels = ent;

  /* We need a new label.  */
  decl = make_label_decl (id, /*local_p=*/0);

  /* Now fill in the information we didn't have before.  */
  ent->label_decl = decl;

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, decl);
}

/* Declare a local label named ID.  */

tree
declare_local_label (tree id)
{
  tree decl;

  /* Add a new entry to the SHADOWED_LABELS list so that when we leave
     this scope we can restore the old value of
     IDENTIFIER_TYPE_VALUE.  */
  current_binding_level->shadowed_labels
    = tree_cons (IDENTIFIER_LABEL_VALUE (id), NULL_TREE,
		 current_binding_level->shadowed_labels);
  /* Look for the label.  */
  decl = make_label_decl (id, /*local_p=*/1);
  /* Now fill in the information we didn't have before.  */
  TREE_VALUE (current_binding_level->shadowed_labels) = decl;

  return decl;
}

/* Returns nonzero if it is ill-formed to jump past the declaration of
   DECL.  Returns 2 if it's also a real problem.  */

static int
decl_jump_unsafe (tree decl)
{
  if (TREE_CODE (decl) != VAR_DECL || TREE_STATIC (decl))
    return 0;

  if (DECL_INITIAL (decl) == NULL_TREE
      && pod_type_p (TREE_TYPE (decl)))
    return 0;

  /* This is really only important if we're crossing an initialization.
     The POD stuff is just pedantry; why should it matter if the class
     contains a field of pointer to member type?  */
  if (DECL_INITIAL (decl)
      || (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl))))
    return 2;
  return 1;
}

/* Check that a single previously seen jump to a newly defined label
   is OK.  DECL is the LABEL_DECL or 0; LEVEL is the binding_level for
   the jump context; NAMES are the names in scope in LEVEL at the jump
   context; FILE and LINE are the source position of the jump or 0.  */

static void
check_previous_goto_1 (tree decl,
                       struct cp_binding_level* level,
                       tree names, const location_t *locus)
{
  int identified = 0;
  int saw_eh = 0;
  struct cp_binding_level *b = current_binding_level;
  for (; b; b = b->level_chain)
    {
      tree new_decls = b->names;
      tree old_decls = (b == level ? names : NULL_TREE);
      for (; new_decls != old_decls;
	   new_decls = TREE_CHAIN (new_decls))
	{
	  int problem = decl_jump_unsafe (new_decls);
	  if (! problem)
	    continue;

	  if (! identified)
	    {
	      if (decl)
		pedwarn ("jump to label `%D'", decl);
	      else
		pedwarn ("jump to case label");

	      if (locus)
		pedwarn ("%H  from here", locus);
	      identified = 1;
	    }

	  if (problem > 1)
	    cp_error_at ("  crosses initialization of `%#D'",
			 new_decls);
	  else
	    cp_pedwarn_at ("  enters scope of non-POD `%#D'",
			   new_decls);
	}

      if (b == level)
	break;
      if ((b->kind == sk_try || b->kind == sk_catch) && ! saw_eh)
	{
	  if (! identified)
	    {
	      if (decl)
		pedwarn ("jump to label `%D'", decl);
	      else
		pedwarn ("jump to case label");

	      if (locus)
		pedwarn ("%H  from here", locus);
	      identified = 1;
	    }
	  if (b->kind == sk_try)
	    error ("  enters try block");
	  else
	    error ("  enters catch block");
	  saw_eh = 1;
	}
    }
}

static void
check_previous_goto (struct named_label_use_list* use)
{
  check_previous_goto_1 (use->label_decl, use->binding_level,
			 use->names_in_scope, &use->o_goto_locus);
}

static void
check_switch_goto (struct cp_binding_level* level)
{
  check_previous_goto_1 (NULL_TREE, level, level->names, NULL);
}

/* Check that any previously seen jumps to a newly defined label DECL
   are OK.  Called by define_label.  */

static void
check_previous_gotos (tree decl)
{
  struct named_label_use_list **usep;

  if (! TREE_USED (decl))
    return;

  for (usep = &named_label_uses; *usep; )
    {
      struct named_label_use_list *use = *usep;
      if (use->label_decl == decl)
	{
	  check_previous_goto (use);
	  *usep = use->next;
	}
      else
	usep = &(use->next);
    }
}

/* Check that a new jump to a label DECL is OK.  Called by
   finish_goto_stmt.  */

void
check_goto (tree decl)
{
  int identified = 0;
  tree bad;
  struct named_label_list *lab;

  /* We can't know where a computed goto is jumping.  So we assume
     that it's OK.  */
  if (! DECL_P (decl))
    return;

  /* If the label hasn't been defined yet, defer checking.  */
  if (! DECL_INITIAL (decl))
    {
      use_label (decl);
      return;
    }

  for (lab = named_labels; lab; lab = lab->next)
    if (decl == lab->label_decl)
      break;

  /* If the label is not on named_labels it's a gcc local label, so
     it must be in an outer scope, so jumping to it is always OK.  */
  if (lab == 0)
    return;

  if ((lab->in_try_scope || lab->in_catch_scope || lab->bad_decls)
      && !identified)
    {
      cp_pedwarn_at ("jump to label `%D'", decl);
      pedwarn ("  from here");
      identified = 1;
    }

  for (bad = lab->bad_decls; bad; bad = TREE_CHAIN (bad))
    {
      tree b = TREE_VALUE (bad);
      int u = decl_jump_unsafe (b);

      if (u > 1 && DECL_ARTIFICIAL (b))
	/* Can't skip init of __exception_info.  */
	error ("%J  enters catch block", b);
      else if (u > 1)
	cp_error_at ("  skips initialization of `%#D'", b);
      else
	cp_pedwarn_at ("  enters scope of non-POD `%#D'", b);
    }

  if (lab->in_try_scope)
    error ("  enters try block");
  else if (lab->in_catch_scope)
    error ("  enters catch block");
}

/* Define a label, specifying the location in the source file.
   Return the LABEL_DECL node for the label.  */

tree
define_label (location_t location, tree name)
{
  tree decl = lookup_label (name);
  struct named_label_list *ent;
  struct cp_binding_level *p;

  timevar_push (TV_NAME_LOOKUP);
  for (ent = named_labels; ent; ent = ent->next)
    if (ent->label_decl == decl)
      break;

  /* After labels, make any new cleanups in the function go into their
     own new (temporary) binding contour.  */
  for (p = current_binding_level; 
       p->kind != sk_function_parms; 
       p = p->level_chain)
    p->more_cleanups_ok = 0;

  if (name == get_identifier ("wchar_t"))
    pedwarn ("label named wchar_t");

  if (DECL_INITIAL (decl) != NULL_TREE)
    error ("duplicate label `%D'", decl);
  else
    {
      /* Mark label as having been defined.  */
      DECL_INITIAL (decl) = error_mark_node;
      /* Say where in the source.  */
      DECL_SOURCE_LOCATION (decl) = location;
      if (ent)
	{
	  ent->names_in_scope = current_binding_level->names;
	  ent->binding_level = current_binding_level;
	}
      check_previous_gotos (decl);
    }

  timevar_pop (TV_NAME_LOOKUP);
  return decl;
}

struct cp_switch
{
  struct cp_binding_level *level;
  struct cp_switch *next;
  /* The SWITCH_STMT being built.  */
  tree switch_stmt;
  /* A splay-tree mapping the low element of a case range to the high
     element, or NULL_TREE if there is no high element.  Used to
     determine whether or not a new case label duplicates an old case
     label.  We need a tree, rather than simply a hash table, because
     of the GNU case range extension.  */
  splay_tree cases;
};

/* A stack of the currently active switch statements.  The innermost
   switch statement is on the top of the stack.  There is no need to
   mark the stack for garbage collection because it is only active
   during the processing of the body of a function, and we never
   collect at that point.  */

static struct cp_switch *switch_stack;

/* Called right after a switch-statement condition is parsed.
   SWITCH_STMT is the switch statement being parsed.  */

void
push_switch (tree switch_stmt)
{
  struct cp_switch *p = xmalloc (sizeof (struct cp_switch));
  p->level = current_binding_level;
  p->next = switch_stack;
  p->switch_stmt = switch_stmt;
  p->cases = splay_tree_new (case_compare, NULL, NULL);
  switch_stack = p;
}

void
pop_switch (void)
{
  struct cp_switch *cs;

  cs = switch_stack;
  splay_tree_delete (cs->cases);
  switch_stack = switch_stack->next;
  free (cs);
}

/* Note that we've seen a definition of a case label, and complain if this
   is a bad place for one.  */

tree
finish_case_label (tree low_value, tree high_value)
{
  tree cond, r;
  struct cp_binding_level *p;

  if (processing_template_decl)
    {
      tree label;

      /* For templates, just add the case label; we'll do semantic
	 analysis at instantiation-time.  */
      label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
      return add_stmt (build_case_label (low_value, high_value, label));
    }

  /* Find the condition on which this switch statement depends.  */
  cond = SWITCH_COND (switch_stack->switch_stmt);
  if (cond && TREE_CODE (cond) == TREE_LIST)
    cond = TREE_VALUE (cond);

  r = c_add_case_label (switch_stack->cases, cond, low_value, high_value);

  check_switch_goto (switch_stack->level);

  /* After labels, make any new cleanups in the function go into their
     own new (temporary) binding contour.  */
  for (p = current_binding_level; 
       p->kind != sk_function_parms; 
       p = p->level_chain)
    p->more_cleanups_ok = 0;

  return r;
}

/* Hash a TYPENAME_TYPE.  K is really of type `tree'.  */

static hashval_t
typename_hash (const void* k)
{
  hashval_t hash;
  tree t = (tree) k;

  hash = (htab_hash_pointer (TYPE_CONTEXT (t))
	  ^ htab_hash_pointer (DECL_NAME (TYPE_NAME (t))));

  return hash;
}

/* Compare two TYPENAME_TYPEs.  K1 and K2 are really of type `tree'.  */

static int
typename_compare (const void * k1, const void * k2)
{
  tree t1;
  tree t2;
  tree d1;
  tree d2;

  t1 = (tree) k1;
  t2 = (tree) k2;
  d1 = TYPE_NAME (t1);
  d2 = TYPE_NAME (t2);

  return (DECL_NAME (d1) == DECL_NAME (d2)
	  && TYPE_CONTEXT (t1) == TYPE_CONTEXT (t2)
	  && ((TREE_TYPE (t1) != NULL_TREE)
	      == (TREE_TYPE (t2) != NULL_TREE))
	  && same_type_p (TREE_TYPE (t1), TREE_TYPE (t2))
	  && TYPENAME_TYPE_FULLNAME (t1) == TYPENAME_TYPE_FULLNAME (t2));
}

/* Build a TYPENAME_TYPE.  If the type is `typename T::t', CONTEXT is
   the type of `T', NAME is the IDENTIFIER_NODE for `t'.  If BASE_TYPE
   is non-NULL, this type is being created by the implicit typename
   extension, and BASE_TYPE is a type named `t' in some base class of
   `T' which depends on template parameters.

   Returns the new TYPENAME_TYPE.  */

static GTY ((param_is (union tree_node))) htab_t typename_htab;

static tree
build_typename_type (tree context, tree name, tree fullname)
{
  tree t;
  tree d;
  void **e;

  if (typename_htab == NULL)
    {
      typename_htab = htab_create_ggc (61, &typename_hash, 
				       &typename_compare, NULL);
    }

  /* Build the TYPENAME_TYPE.  */
  t = make_aggr_type (TYPENAME_TYPE);
  TYPE_CONTEXT (t) = FROB_CONTEXT (context);
  TYPENAME_TYPE_FULLNAME (t) = fullname;

  /* Build the corresponding TYPE_DECL.  */
  d = build_decl (TYPE_DECL, name, t);
  TYPE_NAME (TREE_TYPE (d)) = d;
  TYPE_STUB_DECL (TREE_TYPE (d)) = d;
  DECL_CONTEXT (d) = FROB_CONTEXT (context);
  DECL_ARTIFICIAL (d) = 1;

  /* See if we already have this type.  */
  e = htab_find_slot (typename_htab, t, INSERT);
  if (*e)
    t = (tree) *e;
  else
    *e = t;

  return t;
}

/* Resolve `typename CONTEXT::NAME'.  Returns an appropriate type,
   unless an error occurs, in which case error_mark_node is returned.
   If we locate a non-artificial TYPE_DECL and TF_KEEP_TYPE_DECL is
   set, we return that, rather than the _TYPE it corresponds to, in
   other cases we look through the type decl.  If TF_ERROR is set,
   complain about errors, otherwise be quiet.  */

tree
make_typename_type (tree context, tree name, tsubst_flags_t complain)
{
  tree fullname;

  if (name == error_mark_node
      || context == NULL_TREE
      || context == error_mark_node)
    return error_mark_node;

  if (TYPE_P (name))
    {
      if (!(TYPE_LANG_SPECIFIC (name)
	    && (CLASSTYPE_IS_TEMPLATE (name)
		|| CLASSTYPE_USE_TEMPLATE (name))))
	name = TYPE_IDENTIFIER (name);
      else
	/* Create a TEMPLATE_ID_EXPR for the type.  */
	name = build_nt (TEMPLATE_ID_EXPR,
			 CLASSTYPE_TI_TEMPLATE (name),
			 CLASSTYPE_TI_ARGS (name));
    }
  else if (TREE_CODE (name) == TYPE_DECL)
    name = DECL_NAME (name);

  fullname = name;

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      name = TREE_OPERAND (name, 0);
      if (TREE_CODE (name) == TEMPLATE_DECL)
	name = TREE_OPERAND (fullname, 0) = DECL_NAME (name);
    }
  if (TREE_CODE (name) == TEMPLATE_DECL)
    {
      error ("`%D' used without template parameters", name);
      return error_mark_node;
    }
  my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 20030802);
  
  if (TREE_CODE (context) == NAMESPACE_DECL)
    {
      /* We can get here from typename_sub0 in the explicit_template_type
	 expansion.  Just fail.  */
      if (complain & tf_error)
	error ("no class template named `%#T' in `%#T'",
		  name, context);
      return error_mark_node;
    }

  if (!dependent_type_p (context)
      || currently_open_class (context))
    {
      if (TREE_CODE (fullname) == TEMPLATE_ID_EXPR)
	{
	  tree tmpl = NULL_TREE;
	  if (IS_AGGR_TYPE (context))
	    tmpl = lookup_field (context, name, 0, false);
	  if (!tmpl || !DECL_CLASS_TEMPLATE_P (tmpl))
	    {
	      if (complain & tf_error)
		error ("no class template named `%#T' in `%#T'",
			  name, context);
	      return error_mark_node;
	    }

	  if (complain & tf_error)
	    perform_or_defer_access_check (TYPE_BINFO (context), tmpl);

	  return lookup_template_class (tmpl,
					TREE_OPERAND (fullname, 1),
					NULL_TREE, context,
					/*entering_scope=*/0,
	                                tf_error | tf_warning | tf_user);
	}
      else
	{
          tree t;

	  if (!IS_AGGR_TYPE (context))
	    {
	      if (complain & tf_error)
		error ("no type named `%#T' in `%#T'", name, context);
	      return error_mark_node;
	    }

	  t = lookup_field (context, name, 0, true);
	  if (t)
	    {
	      if (TREE_CODE (t) != TYPE_DECL)
		{
		  if (complain & tf_error)
		    error ("no type named `%#T' in `%#T'", name, context);
		  return error_mark_node;
		}

	      if (complain & tf_error)
		perform_or_defer_access_check (TYPE_BINFO (context), t);

	      if (DECL_ARTIFICIAL (t) || !(complain & tf_keep_type_decl))
		t = TREE_TYPE (t);
	      
	      return t;
	    }
	}
    }

  /* If the CONTEXT is not a template type, then either the field is
     there now or its never going to be.  */
  if (!dependent_type_p (context))
    {
      if (complain & tf_error)
	error ("no type named `%#T' in `%#T'", name, context);
      return error_mark_node;
    }

  return build_typename_type (context, name, fullname);
}

/* Resolve `CONTEXT::template NAME'.  Returns an appropriate type,
   unless an error occurs, in which case error_mark_node is returned.
   If we locate a TYPE_DECL, we return that, rather than the _TYPE it
   corresponds to.  If COMPLAIN zero, don't complain about any errors
   that occur.  */

tree
make_unbound_class_template (tree context, tree name, tsubst_flags_t complain)
{
  tree t;
  tree d;

  if (TYPE_P (name))
    name = TYPE_IDENTIFIER (name);
  else if (DECL_P (name))
    name = DECL_NAME (name);
  if (TREE_CODE (name) != IDENTIFIER_NODE)
    abort ();

  if (!dependent_type_p (context)
      || currently_open_class (context))
    {
      tree tmpl = NULL_TREE;

      if (IS_AGGR_TYPE (context))
	tmpl = lookup_field (context, name, 0, false);

      if (!tmpl || !DECL_CLASS_TEMPLATE_P (tmpl))
	{
	  if (complain & tf_error)
	    error ("no class template named `%#T' in `%#T'", name, context);
	  return error_mark_node;
	}
      
      if (complain & tf_error)
	perform_or_defer_access_check (TYPE_BINFO (context), tmpl);

      return tmpl;
    }

  /* Build the UNBOUND_CLASS_TEMPLATE.  */
  t = make_aggr_type (UNBOUND_CLASS_TEMPLATE);
  TYPE_CONTEXT (t) = FROB_CONTEXT (context);
  TREE_TYPE (t) = NULL_TREE;

  /* Build the corresponding TEMPLATE_DECL.  */
  d = build_decl (TEMPLATE_DECL, name, t);
  TYPE_NAME (TREE_TYPE (d)) = d;
  TYPE_STUB_DECL (TREE_TYPE (d)) = d;
  DECL_CONTEXT (d) = FROB_CONTEXT (context);
  DECL_ARTIFICIAL (d) = 1;

  return t;
}



/* A chain of TYPE_DECLs for the builtin types.  */

static GTY(()) tree builtin_type_decls;

/* Return a chain of TYPE_DECLs for the builtin types.  */

tree
cxx_builtin_type_decls (void)
{
  return builtin_type_decls;
}

/* Push the declarations of builtin types into the namespace.
   RID_INDEX is the index of the builtin type in the array
   RID_POINTERS.  NAME is the name used when looking up the builtin
   type.  TYPE is the _TYPE node for the builtin type.  */

void
record_builtin_type (enum rid rid_index, 
                     const char* name, 
                     tree type)
{
  tree rname = NULL_TREE, tname = NULL_TREE;
  tree tdecl = NULL_TREE;

  if ((int) rid_index < (int) RID_MAX)
    rname = ridpointers[(int) rid_index];
  if (name)
    tname = get_identifier (name);

  /* The calls to SET_IDENTIFIER_GLOBAL_VALUE below should be
     eliminated.  Built-in types should not be looked up name; their
     names are keywords that the parser can recognize.  However, there
     is code in c-common.c that uses identifier_global_value to look
     up built-in types by name.  */
  if (tname)
    {
      tdecl = build_decl (TYPE_DECL, tname, type);
      DECL_ARTIFICIAL (tdecl) = 1;
      SET_IDENTIFIER_GLOBAL_VALUE (tname, tdecl);
    }
  if (rname)
    {
      if (!tdecl)
	{
	  tdecl = build_decl (TYPE_DECL, rname, type);
	  DECL_ARTIFICIAL (tdecl) = 1;
	}
      SET_IDENTIFIER_GLOBAL_VALUE (rname, tdecl);
    }

  if (!TYPE_NAME (type))
    TYPE_NAME (type) = tdecl;

  if (tdecl)
    {
      TREE_CHAIN (tdecl) = builtin_type_decls;
      builtin_type_decls = tdecl;
    }
}

/* Record one of the standard Java types.
 * Declare it as having the given NAME.
 * If SIZE > 0, it is the size of one of the integral types;
 * otherwise it is the negative of the size of one of the other types.  */

static tree
record_builtin_java_type (const char* name, int size)
{
  tree type, decl;
  if (size > 0)
    type = make_signed_type (size);
  else if (size > -32)
    { /* "__java_char" or ""__java_boolean".  */
      type = make_unsigned_type (-size);
      /*if (size == -1)	TREE_SET_CODE (type, BOOLEAN_TYPE);*/
    }
  else
    { /* "__java_float" or ""__java_double".  */
      type = make_node (REAL_TYPE);
      TYPE_PRECISION (type) = - size;
      layout_type (type);
    }
  record_builtin_type (RID_MAX, name, type);
  decl = TYPE_NAME (type);

  /* Suppress generate debug symbol entries for these types,
     since for normal C++ they are just clutter.
     However, push_lang_context undoes this if extern "Java" is seen.  */
  DECL_IGNORED_P (decl) = 1;

  TYPE_FOR_JAVA (type) = 1;
  return type;
}

/* Push a type into the namespace so that the back-ends ignore it.  */

static void
record_unknown_type (tree type, const char* name)
{
  tree decl = pushdecl (build_decl (TYPE_DECL, get_identifier (name), type));
  /* Make sure the "unknown type" typedecl gets ignored for debug info.  */
  DECL_IGNORED_P (decl) = 1;
  TYPE_DECL_SUPPRESS_DEBUG (decl) = 1;
  TYPE_SIZE (type) = TYPE_SIZE (void_type_node);
  TYPE_ALIGN (type) = 1;
  TYPE_USER_ALIGN (type) = 0;
  TYPE_MODE (type) = TYPE_MODE (void_type_node);
}

/* An string for which we should create an IDENTIFIER_NODE at
   startup.  */

typedef struct predefined_identifier
{
  /* The name of the identifier.  */
  const char *const name;
  /* The place where the IDENTIFIER_NODE should be stored.  */
  tree *const node;
  /* Nonzero if this is the name of a constructor or destructor.  */
  const int ctor_or_dtor_p;
} predefined_identifier;

/* Create all the predefined identifiers.  */

static void
initialize_predefined_identifiers (void)
{
  const predefined_identifier *pid;

  /* A table of identifiers to create at startup.  */
  static const predefined_identifier predefined_identifiers[] = {
    { "C++", &lang_name_cplusplus, 0 },
    { "C", &lang_name_c, 0 },
    { "Java", &lang_name_java, 0 },
    { CTOR_NAME, &ctor_identifier, 1 },
    { "__base_ctor", &base_ctor_identifier, 1 },
    { "__comp_ctor", &complete_ctor_identifier, 1 },
    { DTOR_NAME, &dtor_identifier, 1 },
    { "__comp_dtor", &complete_dtor_identifier, 1 },
    { "__base_dtor", &base_dtor_identifier, 1 },
    { "__deleting_dtor", &deleting_dtor_identifier, 1 },
    { IN_CHARGE_NAME, &in_charge_identifier, 0 },
    { "nelts", &nelts_identifier, 0 },
    { THIS_NAME, &this_identifier, 0 },
    { VTABLE_DELTA_NAME, &delta_identifier, 0 },
    { VTABLE_PFN_NAME, &pfn_identifier, 0 },
    { "_vptr", &vptr_identifier, 0 },
    { "__vtt_parm", &vtt_parm_identifier, 0 },
    { "::", &global_scope_name, 0 },
    { "std", &std_identifier, 0 },
    { NULL, NULL, 0 }
  };

  for (pid = predefined_identifiers; pid->name; ++pid)
    {
      *pid->node = get_identifier (pid->name);
      if (pid->ctor_or_dtor_p)
	IDENTIFIER_CTOR_OR_DTOR_P (*pid->node) = 1;
    }
}

/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *)0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

void
cxx_init_decl_processing (void)
{
  tree void_ftype;
  tree void_ftype_ptr;

  /* Create all the identifiers we need.  */
  initialize_predefined_identifiers ();

  /* Fill in back-end hooks.  */
  lang_missing_noreturn_ok_p = &cp_missing_noreturn_ok_p;

  /* Create the global variables.  */
  push_to_top_level ();

  current_function_decl = NULL_TREE;
  current_binding_level = NULL;
  /* Enter the global namespace.  */
  my_friendly_assert (global_namespace == NULL_TREE, 375);
  global_namespace = build_lang_decl (NAMESPACE_DECL, global_scope_name,
                                      void_type_node);
  begin_scope (sk_namespace, global_namespace);

  current_lang_name = NULL_TREE;

  /* Adjust various flags based on command-line settings.  */
  if (!flag_permissive)
    flag_pedantic_errors = 1;
  if (!flag_no_inline)
    {
      flag_inline_trees = 1;
      flag_no_inline = 1;
    }
  if (flag_inline_functions)
    {
      flag_inline_trees = 2;
      flag_inline_functions = 0;
    }

  /* Force minimum function alignment if using the least significant
     bit of function pointers to store the virtual bit.  */
  if (TARGET_PTRMEMFUNC_VBIT_LOCATION == ptrmemfunc_vbit_in_pfn
      && force_align_functions_log < 1)
    force_align_functions_log = 1;

  /* Initially, C.  */
  current_lang_name = lang_name_c;

  build_common_tree_nodes (flag_signed_char);

  error_mark_list = build_tree_list (error_mark_node, error_mark_node);
  TREE_TYPE (error_mark_list) = error_mark_node;

  /* Create the `std' namespace.  */
  push_namespace (std_identifier);
  std_node = current_namespace;
  pop_namespace ();

  c_common_nodes_and_builtins ();

  java_byte_type_node = record_builtin_java_type ("__java_byte", 8);
  java_short_type_node = record_builtin_java_type ("__java_short", 16);
  java_int_type_node = record_builtin_java_type ("__java_int", 32);
  java_long_type_node = record_builtin_java_type ("__java_long", 64);
  java_float_type_node = record_builtin_java_type ("__java_float", -32);
  java_double_type_node = record_builtin_java_type ("__java_double", -64);
  java_char_type_node = record_builtin_java_type ("__java_char", -16);
  java_boolean_type_node = record_builtin_java_type ("__java_boolean", -1);

  integer_two_node = build_int_2 (2, 0);
  TREE_TYPE (integer_two_node) = integer_type_node;
  integer_three_node = build_int_2 (3, 0);
  TREE_TYPE (integer_three_node) = integer_type_node;

  record_builtin_type (RID_BOOL, "bool", boolean_type_node);
  truthvalue_type_node = boolean_type_node;
  truthvalue_false_node = boolean_false_node;
  truthvalue_true_node = boolean_true_node;

  empty_except_spec = build_tree_list (NULL_TREE, NULL_TREE);

#if 0
  record_builtin_type (RID_MAX, NULL, string_type_node);
#endif

  delta_type_node = ptrdiff_type_node;
  vtable_index_type = ptrdiff_type_node;

  vtt_parm_type = build_pointer_type (const_ptr_type_node);
  void_ftype = build_function_type (void_type_node, void_list_node);
  void_ftype_ptr = build_function_type (void_type_node,
					tree_cons (NULL_TREE,
						   ptr_type_node, 
						   void_list_node));
  void_ftype_ptr
    = build_exception_variant (void_ftype_ptr, empty_except_spec);

  /* C++ extensions */

  unknown_type_node = make_node (UNKNOWN_TYPE);
  record_unknown_type (unknown_type_node, "unknown type");

  /* Indirecting an UNKNOWN_TYPE node yields an UNKNOWN_TYPE node.  */
  TREE_TYPE (unknown_type_node) = unknown_type_node;

  /* Looking up TYPE_POINTER_TO and TYPE_REFERENCE_TO yield the same
     result.  */
  TYPE_POINTER_TO (unknown_type_node) = unknown_type_node;
  TYPE_REFERENCE_TO (unknown_type_node) = unknown_type_node;

  {
    /* Make sure we get a unique function type, so we can give
       its pointer type a name.  (This wins for gdb.) */
    tree vfunc_type = make_node (FUNCTION_TYPE);
    TREE_TYPE (vfunc_type) = integer_type_node;
    TYPE_ARG_TYPES (vfunc_type) = NULL_TREE;
    layout_type (vfunc_type);

    vtable_entry_type = build_pointer_type (vfunc_type);
  }
  record_builtin_type (RID_MAX, VTBL_PTR_TYPE, vtable_entry_type);

  vtbl_type_node
    = build_cplus_array_type (vtable_entry_type, NULL_TREE);
  layout_type (vtbl_type_node);
  vtbl_type_node = build_qualified_type (vtbl_type_node, TYPE_QUAL_CONST);
  record_builtin_type (RID_MAX, NULL, vtbl_type_node);
  vtbl_ptr_type_node = build_pointer_type (vtable_entry_type);
  layout_type (vtbl_ptr_type_node);
  record_builtin_type (RID_MAX, NULL, vtbl_ptr_type_node);

  push_namespace (get_identifier ("__cxxabiv1"));
  abi_node = current_namespace;
  pop_namespace ();

  global_type_node = make_node (LANG_TYPE);
  record_unknown_type (global_type_node, "global type");

  /* Now, C++.  */
  current_lang_name = lang_name_cplusplus;

  {
    tree bad_alloc_id;
    tree bad_alloc_type_node;
    tree bad_alloc_decl;
    tree newtype, deltype;
    tree ptr_ftype_sizetype;

    push_namespace (std_identifier);
    bad_alloc_id = get_identifier ("bad_alloc");
    bad_alloc_type_node = make_aggr_type (RECORD_TYPE);
    TYPE_CONTEXT (bad_alloc_type_node) = current_namespace;
    bad_alloc_decl 
      = create_implicit_typedef (bad_alloc_id, bad_alloc_type_node);
    DECL_CONTEXT (bad_alloc_decl) = current_namespace;
    TYPE_STUB_DECL (bad_alloc_type_node) = bad_alloc_decl;
    pop_namespace ();
 
    ptr_ftype_sizetype 
      = build_function_type (ptr_type_node,
			     tree_cons (NULL_TREE,
					size_type_node,
					void_list_node));
    newtype = build_exception_variant
      (ptr_ftype_sizetype, add_exception_specifier
       (NULL_TREE, bad_alloc_type_node, -1));
    deltype = build_exception_variant (void_ftype_ptr, empty_except_spec);
    push_cp_library_fn (NEW_EXPR, newtype);
    push_cp_library_fn (VEC_NEW_EXPR, newtype);
    global_delete_fndecl = push_cp_library_fn (DELETE_EXPR, deltype);
    push_cp_library_fn (VEC_DELETE_EXPR, deltype);
  }

  abort_fndecl
    = build_library_fn_ptr ("__cxa_pure_virtual", void_ftype);

  /* Perform other language dependent initializations.  */
  init_class_processing ();
  init_search_processing ();
  init_rtti_processing ();

  if (flag_exceptions)
    init_exception_processing ();

  if (! supports_one_only ())
    flag_weak = 0;

  make_fname_decl = cp_make_fname_decl;
  start_fname_decls ();

  /* Show we use EH for cleanups.  */
  using_eh_for_cleanups ();

  /* Maintain consistency.  Perhaps we should just complain if they
     say -fwritable-strings?  */
  if (flag_writable_strings)
    flag_const_strings = 0;
}

/* Generate an initializer for a function naming variable from
   NAME. NAME may be NULL, to indicate a dependent name.  TYPE_P is
   filled in with the type of the init.  */

tree
cp_fname_init (const char* name, tree *type_p)
{
  tree domain = NULL_TREE;
  tree type;
  tree init = NULL_TREE;
  size_t length = 0;

  if (name)
    {
      length = strlen (name);
      domain = build_index_type (size_int (length));
      init = build_string (length + 1, name);
    }
  
  type = build_qualified_type (char_type_node, TYPE_QUAL_CONST);
  type = build_cplus_array_type (type, domain);

  *type_p = type;
  
  if (init)
    TREE_TYPE (init) = type;
  else
    init = error_mark_node;
  
  return init;
}

/* Create the VAR_DECL for __FUNCTION__ etc. ID is the name to give the
   decl, NAME is the initialization string and TYPE_DEP indicates whether
   NAME depended on the type of the function. We make use of that to detect
   __PRETTY_FUNCTION__ inside a template fn. This is being done
   lazily at the point of first use, so we musn't push the decl now.  */

static tree
cp_make_fname_decl (tree id, int type_dep)
{
  const char *const name = (type_dep && processing_template_decl
			    ? NULL : fname_as_string (type_dep));
  tree type;
  tree init = cp_fname_init (name, &type);
  tree decl = build_decl (VAR_DECL, id, type);

  /* As we're using pushdecl_with_scope, we must set the context.  */
  DECL_CONTEXT (decl) = current_function_decl;
  DECL_PRETTY_FUNCTION_P (decl) = type_dep;
      
  TREE_STATIC (decl) = 1;
  TREE_READONLY (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_INITIAL (decl) = init;
  
  TREE_USED (decl) = 1;

  if (current_function_decl)
    {
      struct cp_binding_level *b = current_binding_level;
      while (b->level_chain->kind != sk_function_parms)
	b = b->level_chain;
      pushdecl_with_scope (decl, b);
      cp_finish_decl (decl, init, NULL_TREE, LOOKUP_ONLYCONVERTING);
    }
  else
    pushdecl_top_level_and_finish (decl, init);
      
  return decl;
}

/* Make a definition for a builtin function named NAME in the current
   namespace, whose data type is TYPE and whose context is CONTEXT.
   TYPE should be a function type with argument types.

   CLASS and CODE tell later passes how to compile calls to this function.
   See tree.h for possible values.

   If LIBNAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.
   If ATTRS is nonzero, use that for the function's attribute
   list.  */

static tree
builtin_function_1 (const char* name,
                    tree type,
                    tree context,
                    int code,
                    enum built_in_class class,
                    const char* libname,
                    tree attrs)
{
  tree decl = build_library_fn_1 (get_identifier (name), ERROR_MARK, type);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = code;
  DECL_CONTEXT (decl) = context;

  pushdecl (decl);

  /* Since `pushdecl' relies on DECL_ASSEMBLER_NAME instead of DECL_NAME,
     we cannot change DECL_ASSEMBLER_NAME until we have installed this
     function in the namespace.  */
  if (libname)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (libname));
  make_decl_rtl (decl, NULL);

  /* Warn if a function in the namespace for users
     is used without an occasion to consider it declared.  */
  if (name[0] != '_' || name[1] != '_')
    DECL_ANTICIPATED (decl) = 1;

  /* Possibly apply some default attributes to this built-in function.  */
  if (attrs)
    decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
  else
    decl_attributes (&decl, NULL_TREE, 0);

  return decl;
}

/* Entry point for the benefit of c_common_nodes_and_builtins.

   Make a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.  This
   function places the anticipated declaration in the global namespace
   and additionally in the std namespace if appropriate.

   CLASS and CODE tell later passes how to compile calls to this function.
   See tree.h for possible values.

   If LIBNAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.

   If ATTRS is nonzero, use that for the function's attribute
   list.  */

tree
builtin_function (const char* name,
                  tree type,
                  int code,
                  enum built_in_class class,
                  const char* libname,
                  tree attrs)
{
  /* All builtins that don't begin with an '_' should additionally
     go in the 'std' namespace.  */
  if (name[0] != '_')
    {
      push_namespace (std_identifier);
      builtin_function_1 (name, type, std_node, code, class, libname, attrs);
      pop_namespace ();
    }

  return builtin_function_1 (name, type, NULL_TREE, code,
			     class, libname, attrs);
}

/* Generate a FUNCTION_DECL with the typical flags for a runtime library
   function.  Not called directly.  */

static tree
build_library_fn_1 (tree name, enum tree_code operator_code, tree type)
{
  tree fn = build_lang_decl (FUNCTION_DECL, name, type);
  DECL_EXTERNAL (fn) = 1;
  TREE_PUBLIC (fn) = 1;
  DECL_ARTIFICIAL (fn) = 1;
  TREE_NOTHROW (fn) = 1;
  SET_OVERLOADED_OPERATOR_CODE (fn, operator_code);
  SET_DECL_LANGUAGE (fn, lang_c);
  return fn;
}

/* Returns the _DECL for a library function with C linkage.
   We assume that such functions never throw; if this is incorrect,
   callers should unset TREE_NOTHROW.  */

tree
build_library_fn (tree name, tree type)
{
  return build_library_fn_1 (name, ERROR_MARK, type);
}

/* Returns the _DECL for a library function with C++ linkage.  */

static tree
build_cp_library_fn (tree name, enum tree_code operator_code, tree type)
{
  tree fn = build_library_fn_1 (name, operator_code, type);
  TREE_NOTHROW (fn) = TYPE_NOTHROW_P (type);
  DECL_CONTEXT (fn) = FROB_CONTEXT (current_namespace);
  SET_DECL_LANGUAGE (fn, lang_cplusplus);
  set_mangled_name_for_decl (fn);
  return fn;
}

/* Like build_library_fn, but takes a C string instead of an
   IDENTIFIER_NODE.  */

tree
build_library_fn_ptr (const char* name, tree type)
{
  return build_library_fn (get_identifier (name), type);
}

/* Like build_cp_library_fn, but takes a C string instead of an
   IDENTIFIER_NODE.  */

tree
build_cp_library_fn_ptr (const char* name, tree type)
{
  return build_cp_library_fn (get_identifier (name), ERROR_MARK, type);
}

/* Like build_library_fn, but also pushes the function so that we will
   be able to find it via IDENTIFIER_GLOBAL_VALUE.  */

tree
push_library_fn (tree name, tree type)
{
  tree fn = build_library_fn (name, type);
  pushdecl_top_level (fn);
  return fn;
}

/* Like build_cp_library_fn, but also pushes the function so that it
   will be found by normal lookup.  */

static tree
push_cp_library_fn (enum tree_code operator_code, tree type)
{
  tree fn = build_cp_library_fn (ansi_opname (operator_code),
				 operator_code,
				 type);
  pushdecl (fn);
  return fn;
}

/* Like push_library_fn, but takes a TREE_LIST of parm types rather than
   a FUNCTION_TYPE.  */

tree
push_void_library_fn (tree name, tree parmtypes)
{
  tree type = build_function_type (void_type_node, parmtypes);
  return push_library_fn (name, type);
}

/* Like push_library_fn, but also note that this function throws
   and does not return.  Used for __throw_foo and the like.  */

tree
push_throw_library_fn (tree name, tree type)
{
  tree fn = push_library_fn (name, type);
  TREE_THIS_VOLATILE (fn) = 1;
  TREE_NOTHROW (fn) = 0;
  return fn;
}

/* When we call finish_struct for an anonymous union, we create
   default copy constructors and such.  But, an anonymous union
   shouldn't have such things; this function undoes the damage to the
   anonymous union type T.

   (The reason that we create the synthesized methods is that we don't
   distinguish `union { int i; }' from `typedef union { int i; } U'.
   The first is an anonymous union; the second is just an ordinary
   union type.)  */

void
fixup_anonymous_aggr (tree t)
{
  tree *q;

  /* Wipe out memory of synthesized methods.  */
  TYPE_HAS_CONSTRUCTOR (t) = 0;
  TYPE_HAS_DEFAULT_CONSTRUCTOR (t) = 0;
  TYPE_HAS_INIT_REF (t) = 0;
  TYPE_HAS_CONST_INIT_REF (t) = 0;
  TYPE_HAS_ASSIGN_REF (t) = 0;
  TYPE_HAS_CONST_ASSIGN_REF (t) = 0;

  /* Splice the implicitly generated functions out of the TYPE_METHODS
     list.  */
  q = &TYPE_METHODS (t);
  while (*q)
    {
      if (DECL_ARTIFICIAL (*q))
	*q = TREE_CHAIN (*q);
      else
	q = &TREE_CHAIN (*q);
    }

  /* ISO C++ 9.5.3.  Anonymous unions may not have function members.  */
  if (TYPE_METHODS (t))
    error ("%Jan anonymous union cannot have function members",
	   TYPE_MAIN_DECL (t));

  /* Anonymous aggregates cannot have fields with ctors, dtors or complex
     assignment operators (because they cannot have these methods themselves).
     For anonymous unions this is already checked because they are not allowed
     in any union, otherwise we have to check it.  */
  if (TREE_CODE (t) != UNION_TYPE)
    {
      tree field, type;

      for (field = TYPE_FIELDS (t); field; field = TREE_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  {
	    type = TREE_TYPE (field);
	    if (CLASS_TYPE_P (type))
	      {
	        if (TYPE_NEEDS_CONSTRUCTING (type))
		  cp_error_at ("member %#D' with constructor not allowed in anonymous aggregate",
			       field);
		if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
		  cp_error_at ("member %#D' with destructor not allowed in anonymous aggregate",
			       field);
		if (TYPE_HAS_COMPLEX_ASSIGN_REF (type))
		  cp_error_at ("member %#D' with copy assignment operator not allowed in anonymous aggregate",
			       field);
	      }
	  }
    }
}

/* Make sure that a declaration with no declarator is well-formed, i.e.
   just declares a tagged type or anonymous union.

   Returns the type declared; or NULL_TREE if none.  */

tree
check_tag_decl (tree declspecs)
{
  int found_type = 0;
  int saw_friend = 0;
  int saw_typedef = 0;
  tree ob_modifier = NULL_TREE;
  tree link;
  /* If a class, struct, or enum type is declared by the DECLSPECS
     (i.e, if a class-specifier, enum-specifier, or non-typename
     elaborated-type-specifier appears in the DECLSPECS),
     DECLARED_TYPE is set to the corresponding type.  */
  tree declared_type = NULL_TREE;
  bool error_p = false;

  for (link = declspecs; link; link = TREE_CHAIN (link))
    {
      tree value = TREE_VALUE (link);

      if (TYPE_P (value) || TREE_CODE (value) == TYPE_DECL
	  || (TREE_CODE (value) == IDENTIFIER_NODE
	      && is_typename_at_global_scope (value)))
	{
	  ++found_type;

	  if (found_type == 2 && TREE_CODE (value) == IDENTIFIER_NODE)
	    {
	      if (! in_system_header)
		pedwarn ("redeclaration of C++ built-in type `%T'", value);
	      return NULL_TREE;
	    }

	  if (TYPE_P (value)
	      && ((TREE_CODE (value) != TYPENAME_TYPE && IS_AGGR_TYPE (value))
		  || TREE_CODE (value) == ENUMERAL_TYPE))
	    {
	      my_friendly_assert (TYPE_MAIN_DECL (value) != NULL_TREE, 261);
	      declared_type = value;
	    }
	}
      else if (value == ridpointers[(int) RID_TYPEDEF])
        saw_typedef = 1;
      else if (value == ridpointers[(int) RID_FRIEND])
	{
	  if (current_class_type == NULL_TREE
	      || current_scope () != current_class_type)
	    ob_modifier = value;
	  else
	    saw_friend = 1;
	}
      else if (value == ridpointers[(int) RID_STATIC]
	       || value == ridpointers[(int) RID_EXTERN]
	       || value == ridpointers[(int) RID_AUTO]
	       || value == ridpointers[(int) RID_REGISTER]
	       || value == ridpointers[(int) RID_INLINE]
	       || value == ridpointers[(int) RID_VIRTUAL]
	       || value == ridpointers[(int) RID_CONST]
	       || value == ridpointers[(int) RID_VOLATILE]
	       || value == ridpointers[(int) RID_EXPLICIT]
	       || value == ridpointers[(int) RID_THREAD])
	ob_modifier = value;
      else if (value == error_mark_node)
	error_p = true;
    }

  if (found_type > 1)
    error ("multiple types in one declaration");

  if (declared_type == NULL_TREE && ! saw_friend && !error_p)
    pedwarn ("declaration does not declare anything");
  /* Check for an anonymous union.  */
  else if (declared_type && IS_AGGR_TYPE_CODE (TREE_CODE (declared_type))
	   && TYPE_ANONYMOUS_P (declared_type))
    {
      /* 7/3 In a simple-declaration, the optional init-declarator-list
         can be omitted only when declaring a class (clause 9) or
         enumeration (7.2), that is, when the decl-specifier-seq contains
         either a class-specifier, an elaborated-type-specifier with
         a class-key (9.1), or an enum-specifier.  In these cases and
         whenever a class-specifier or enum-specifier is present in the
         decl-specifier-seq, the identifiers in these specifiers are among
         the names being declared by the declaration (as class-name,
         enum-names, or enumerators, depending on the syntax).  In such
         cases, and except for the declaration of an unnamed bit-field (9.6),
         the decl-specifier-seq shall introduce one or more names into the
         program, or shall redeclare a name introduced by a previous
         declaration.  [Example:
             enum { };            // ill-formed
             typedef class { };   // ill-formed
         --end example]  */
      if (saw_typedef)
        {
          error ("missing type-name in typedef-declaration");
          return NULL_TREE;
        }
      /* Anonymous unions are objects, so they can have specifiers.  */;
      SET_ANON_AGGR_TYPE_P (declared_type);

      if (TREE_CODE (declared_type) != UNION_TYPE && pedantic 
	  && !in_system_header)
	pedwarn ("ISO C++ prohibits anonymous structs");
    }

  else if (ob_modifier)
    {
      if (ob_modifier == ridpointers[(int) RID_INLINE]
	  || ob_modifier == ridpointers[(int) RID_VIRTUAL])
	error ("`%D' can only be specified for functions", ob_modifier);
      else if (ob_modifier == ridpointers[(int) RID_FRIEND])
	error ("`%D' can only be specified inside a class", ob_modifier);
      else if (ob_modifier == ridpointers[(int) RID_EXPLICIT])
	error ("`%D' can only be specified for constructors",
		  ob_modifier);
      else
	error ("`%D' can only be specified for objects and functions",
		  ob_modifier);
    }

  return declared_type;
}

/* Called when a declaration is seen that contains no names to declare.
   If its type is a reference to a structure, union or enum inherited
   from a containing scope, shadow that tag name for the current scope
   with a forward reference.
   If its type defines a new named structure or union
   or defines an enum, it is valid but we need not do anything here.
   Otherwise, it is an error.

   C++: may have to grok the declspecs to learn about static,
   complain for anonymous unions.  

   Returns the TYPE declared -- or NULL_TREE if none.  */

tree
shadow_tag (tree declspecs)
{
  tree t = check_tag_decl (declspecs);

  if (!t)
    return NULL_TREE;

  maybe_process_partial_specialization (t);

  /* This is where the variables in an anonymous union are
     declared.  An anonymous union declaration looks like:
     union { ... } ;
     because there is no declarator after the union, the parser
     sends that declaration here.  */
  if (ANON_AGGR_TYPE_P (t))
    {
      fixup_anonymous_aggr (t);

      if (TYPE_FIELDS (t))
	{
	  tree decl = grokdeclarator (NULL_TREE, declspecs, NORMAL, 0,
				      NULL);
	  finish_anon_union (decl);
	}
    }

  return t;
}

/* Decode a "typename", such as "int **", returning a ..._TYPE node.  */

tree
groktypename (tree typename)
{
  tree specs, attrs;
  tree type;
  if (TREE_CODE (typename) != TREE_LIST)
    return typename;
  split_specs_attrs (TREE_PURPOSE (typename), &specs, &attrs);
  type = grokdeclarator (TREE_VALUE (typename), specs,
			 TYPENAME, 0, &attrs);
  if (attrs)
    cplus_decl_attributes (&type, attrs, 0);
  return type;
}

/* Decode a declarator in an ordinary declaration or data definition.
   This is called as soon as the type information and variable name
   have been parsed, before parsing the initializer if any.
   Here we create the ..._DECL node, fill in its type,
   and put it on the list of decls for the current context.
   The ..._DECL node is returned as the value.

   Exception: for arrays where the length is not specified,
   the type is left null, to be filled in by `cp_finish_decl'.

   Function definitions do not come here; they go to start_function
   instead.  However, external and forward declarations of functions
   do go through here.  Structure field declarations are done by
   grokfield and not through here.  */

tree
start_decl (tree declarator, 
            tree declspecs, 
            int initialized, 
            tree attributes, 
            tree prefix_attributes)
{
  tree decl;
  tree type, tem;
  tree context;

  /* This should only be done once on the top most decl.  */
  if (have_extern_spec)
    {
      declspecs = tree_cons (NULL_TREE, get_identifier ("extern"),
			     declspecs);
      have_extern_spec = false;
    }

  /* An object declared as __attribute__((deprecated)) suppresses
     warnings of uses of other deprecated items.  */
  if (lookup_attribute ("deprecated", attributes))
    deprecated_state = DEPRECATED_SUPPRESS;

  attributes = chainon (attributes, prefix_attributes);

  decl = grokdeclarator (declarator, declspecs, NORMAL, initialized,
			 &attributes);

  deprecated_state = DEPRECATED_NORMAL;

  if (decl == NULL_TREE || TREE_CODE (decl) == VOID_TYPE)
    return error_mark_node;

  type = TREE_TYPE (decl);

  if (type == error_mark_node)
    return error_mark_node;

  context = DECL_CONTEXT (decl);

  if (initialized && context && TREE_CODE (context) == NAMESPACE_DECL
      && context != current_namespace && TREE_CODE (decl) == VAR_DECL)
    {
      /* When parsing the initializer, lookup should use the object's
	 namespace.  */
      push_decl_namespace (context);
    }

  /* We are only interested in class contexts, later.  */
  if (context && TREE_CODE (context) == NAMESPACE_DECL)
    context = NULL_TREE;

  if (initialized)
    /* Is it valid for this decl to have an initializer at all?
       If not, set INITIALIZED to zero, which will indirectly
       tell `cp_finish_decl' to ignore the initializer once it is parsed.  */
    switch (TREE_CODE (decl))
      {
      case TYPE_DECL:
	error ("typedef `%D' is initialized (use __typeof__ instead)", decl);
	initialized = 0;
	break;

      case FUNCTION_DECL:
	error ("function `%#D' is initialized like a variable", decl);
	initialized = 0;
	break;

      default:
	break;
      }

  if (initialized)
    {
      if (! toplevel_bindings_p ()
	  && DECL_EXTERNAL (decl))
	warning ("declaration of `%#D' has `extern' and is initialized",
		    decl);
      DECL_EXTERNAL (decl) = 0;
      if (toplevel_bindings_p ())
	TREE_STATIC (decl) = 1;

      /* Tell `pushdecl' this is an initialized decl
	 even though we don't yet have the initializer expression.
	 Also tell `cp_finish_decl' it may store the real initializer.  */
      DECL_INITIAL (decl) = error_mark_node;
    }

  /* Set attributes here so if duplicate decl, will have proper attributes.  */
  cplus_decl_attributes (&decl, attributes, 0);

  /* If #pragma weak was used, mark the decl weak now.  */
  if (global_scope_p (current_binding_level))
    maybe_apply_pragma_weak (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_DECLARED_INLINE_P (decl)
      && DECL_UNINLINABLE (decl)
      && lookup_attribute ("noinline", DECL_ATTRIBUTES (decl)))
    warning ("%Jinline function '%D' given attribute noinline", decl, decl);

  if (context && COMPLETE_TYPE_P (complete_type (context)))
    {
      push_nested_class (context);

      if (TREE_CODE (decl) == VAR_DECL)
	{
	  tree field = lookup_field (context, DECL_NAME (decl), 0, false);
	  if (field == NULL_TREE || TREE_CODE (field) != VAR_DECL)
	    error ("`%#D' is not a static member of `%#T'", decl, context);
	  else
	    {
	      if (DECL_CONTEXT (field) != context)
		{
		  if (!same_type_p (DECL_CONTEXT (field), context))
		    pedwarn ("ISO C++ does not permit `%T::%D' to be defined as `%T::%D'",
			     DECL_CONTEXT (field), DECL_NAME (decl),
			     context, DECL_NAME (decl));
		  DECL_CONTEXT (decl) = DECL_CONTEXT (field);
		}
	      /* Static data member are tricky; an in-class initialization
		 still doesn't provide a definition, so the in-class
		 declaration will have DECL_EXTERNAL set, but will have an
		 initialization.  Thus, duplicate_decls won't warn
		 about this situation, and so we check here.  */
	      if (DECL_INITIAL (decl) && DECL_INITIAL (field))
		error ("duplicate initialization of %D", decl);
	      if (duplicate_decls (decl, field))
		decl = field;
	    }
	}
      else
	{
	  tree field = check_classfn (context, decl,
				      processing_template_decl
				      > template_class_depth (context));
	  if (field && duplicate_decls (decl, field))
	    decl = field;
	}

      /* cp_finish_decl sets DECL_EXTERNAL if DECL_IN_AGGR_P is set.  */
      DECL_IN_AGGR_P (decl) = 0;
      if ((DECL_LANG_SPECIFIC (decl) && DECL_USE_TEMPLATE (decl))
	  || CLASSTYPE_TEMPLATE_INSTANTIATION (context))
	{
	  SET_DECL_TEMPLATE_SPECIALIZATION (decl);
	  /* [temp.expl.spec] An explicit specialization of a static data
	     member of a template is a definition if the declaration
	     includes an initializer; otherwise, it is a declaration.

	     We check for processing_specialization so this only applies
	     to the new specialization syntax.  */
	  if (DECL_INITIAL (decl) == NULL_TREE && processing_specialization)
	    DECL_EXTERNAL (decl) = 1;
	}

      if (DECL_EXTERNAL (decl) && ! DECL_TEMPLATE_SPECIALIZATION (decl))
	pedwarn ("declaration of `%#D' outside of class is not definition",
		    decl);
    }

  /* Enter this declaration into the symbol table.  */
  tem = maybe_push_decl (decl);

  if (processing_template_decl)
    tem = push_template_decl (tem);
  if (tem == error_mark_node)
    return error_mark_node;

#if ! defined (ASM_OUTPUT_BSS) && ! defined (ASM_OUTPUT_ALIGNED_BSS)
  /* Tell the back-end to use or not use .common as appropriate.  If we say
     -fconserve-space, we want this to save .data space, at the expense of
     wrong semantics.  If we say -fno-conserve-space, we want this to
     produce errors about redefs; to do this we force variables into the
     data segment.  */
  DECL_COMMON (tem) = ((TREE_CODE (tem) != VAR_DECL
			|| !DECL_THREAD_LOCAL (tem))
		       && (flag_conserve_space || ! TREE_PUBLIC (tem)));
#endif

  if (! processing_template_decl)
    start_decl_1 (tem);

  return tem;
}

void
start_decl_1 (tree decl)
{
  tree type = TREE_TYPE (decl);
  int initialized = (DECL_INITIAL (decl) != NULL_TREE);

  if (type == error_mark_node)
    return;

  if (initialized)
    /* Is it valid for this decl to have an initializer at all?
       If not, set INITIALIZED to zero, which will indirectly
       tell `cp_finish_decl' to ignore the initializer once it is parsed.  */
    {
      /* Don't allow initializations for incomplete types except for
	 arrays which might be completed by the initialization.  */
      if (COMPLETE_TYPE_P (complete_type (type)))
	;			/* A complete type is ok.  */
      else if (TREE_CODE (type) != ARRAY_TYPE)
	{
	  error ("variable `%#D' has initializer but incomplete type",
		    decl);
	  initialized = 0;
	  type = TREE_TYPE (decl) = error_mark_node;
	}
      else if (!COMPLETE_TYPE_P (complete_type (TREE_TYPE (type))))
	{
	  if (DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl))
	    error ("elements of array `%#D' have incomplete type", decl);
	  /* else we already gave an error in start_decl.  */
	  initialized = 0;
	}
    }

  if (!initialized
      && TREE_CODE (decl) != TYPE_DECL
      && TREE_CODE (decl) != TEMPLATE_DECL
      && type != error_mark_node
      && IS_AGGR_TYPE (type)
      && ! DECL_EXTERNAL (decl))
    {
      if ((! processing_template_decl || ! uses_template_parms (type))
	  && !COMPLETE_TYPE_P (complete_type (type)))
	{
	  error ("aggregate `%#D' has incomplete type and cannot be defined",
		 decl);
	  /* Change the type so that assemble_variable will give
	     DECL an rtl we can live with: (mem (const_int 0)).  */
	  type = TREE_TYPE (decl) = error_mark_node;
	}
      else
	{
	  /* If any base type in the hierarchy of TYPE needs a constructor,
	     then we set initialized to 1.  This way any nodes which are
	     created for the purposes of initializing this aggregate
	     will live as long as it does.  This is necessary for global
	     aggregates which do not have their initializers processed until
	     the end of the file.  */
	  initialized = TYPE_NEEDS_CONSTRUCTING (type);
	}
    }

  if (! initialized)
    DECL_INITIAL (decl) = NULL_TREE;

  /* Create a new scope to hold this declaration if necessary.
     Whether or not a new scope is necessary cannot be determined
     until after the type has been completed; if the type is a
     specialization of a class template it is not until after
     instantiation has occurred that TYPE_HAS_NONTRIVIAL_DESTRUCTOR
     will be set correctly.  */
  maybe_push_cleanup_level (type);
}

/* Handle initialization of references.  DECL, TYPE, and INIT have the
   same meaning as in cp_finish_decl.  *CLEANUP must be NULL on entry,
   but will be set to a new CLEANUP_STMT if a temporary is created
   that must be destroyed subsequently.

   Returns an initializer expression to use to initialize DECL, or
   NULL if the initialization can be performed statically.

   Quotes on semantics can be found in ARM 8.4.3.  */

static tree
grok_reference_init (tree decl, tree type, tree init, tree *cleanup)
{
  tree tmp;

  if (init == NULL_TREE)
    {
      if ((DECL_LANG_SPECIFIC (decl) == 0
	   || DECL_IN_AGGR_P (decl) == 0)
	  && ! DECL_THIS_EXTERN (decl))
	error ("`%D' declared as reference but not initialized", decl);
      return NULL_TREE;
    }

  if (TREE_CODE (init) == CONSTRUCTOR)
    {
      error ("ISO C++ forbids use of initializer list to initialize reference `%D'", decl);
      return NULL_TREE;
    }

  if (TREE_CODE (init) == TREE_LIST)
    init = build_x_compound_expr_from_list (init, "initializer");

  if (TREE_CODE (TREE_TYPE (init)) == REFERENCE_TYPE)
    init = convert_from_reference (init);

  if (TREE_CODE (TREE_TYPE (type)) != ARRAY_TYPE
      && TREE_CODE (TREE_TYPE (init)) == ARRAY_TYPE)
    /* Note: default conversion is only called in very special cases.  */
    init = decay_conversion (init);

  /* Convert INIT to the reference type TYPE.  This may involve the
     creation of a temporary, whose lifetime must be the same as that
     of the reference.  If so, a DECL_STMT for the temporary will be
     added just after the DECL_STMT for DECL.  That's why we don't set
     DECL_INITIAL for local references (instead assigning to them
     explicitly); we need to allow the temporary to be initialized
     first.  */
  tmp = initialize_reference (type, init, decl, cleanup);

  if (tmp == error_mark_node)
    return NULL_TREE;
  else if (tmp == NULL_TREE)
    {
      error ("cannot initialize `%T' from `%T'", type, TREE_TYPE (init));
      return NULL_TREE;
    }

  if (TREE_STATIC (decl) && !TREE_CONSTANT (tmp))
    return tmp;

  DECL_INITIAL (decl) = tmp;

  return NULL_TREE;
}

/* When parsing `int a[] = {1, 2};' we don't know the size of the
   array until we finish parsing the initializer.  If that's the
   situation we're in, update DECL accordingly.  */

static void
maybe_deduce_size_from_array_init (tree decl, tree init)
{
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == NULL_TREE
      && TREE_CODE (decl) != TYPE_DECL)
    {
      /* do_default is really a C-ism to deal with tentative definitions.
	 But let's leave it here to ease the eventual merge.  */
      int do_default = !DECL_EXTERNAL (decl);
      tree initializer = init ? init : DECL_INITIAL (decl);
      int failure = complete_array_type (type, initializer, do_default);

      if (failure == 1)
	error ("initializer fails to determine size of `%D'", decl);

      if (failure == 2)
	{
	  if (do_default)
	    error ("array size missing in `%D'", decl);
	  /* If a `static' var's size isn't known, make it extern as
	     well as static, so it does not get allocated.  If it's not
	     `static', then don't mark it extern; finish_incomplete_decl
	     will give it a default size and it will get allocated.  */
	  else if (!pedantic && TREE_STATIC (decl) && !TREE_PUBLIC (decl))
	    DECL_EXTERNAL (decl) = 1;
	}

      if (pedantic && TYPE_DOMAIN (type) != NULL_TREE
	  && tree_int_cst_lt (TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
			      integer_zero_node))
	error ("zero-size array `%D'", decl);

      layout_decl (decl, 0);
    }
}

/* Set DECL_SIZE, DECL_ALIGN, etc. for DECL (a VAR_DECL), and issue
   any appropriate error messages regarding the layout.  */

static void
layout_var_decl (tree decl)
{
  tree type = TREE_TYPE (decl);
#if 0
  tree ttype = target_type (type);
#endif

  /* If we haven't already layed out this declaration, do so now.
     Note that we must not call complete type for an external object
     because it's type might involve templates that we are not
     supposed to instantiate yet.  (And it's perfectly valid to say
     `extern X x' for some incomplete type `X'.)  */
  if (!DECL_EXTERNAL (decl))
    complete_type (type);
  if (!DECL_SIZE (decl) 
      && TREE_TYPE (decl) != error_mark_node
      && (COMPLETE_TYPE_P (type)
	  || (TREE_CODE (type) == ARRAY_TYPE 
	      && !TYPE_DOMAIN (type)
	      && COMPLETE_TYPE_P (TREE_TYPE (type)))))
    layout_decl (decl, 0);

  if (!DECL_EXTERNAL (decl) && DECL_SIZE (decl) == NULL_TREE)
    {
      /* An automatic variable with an incomplete type: that is an error.
	 Don't talk about array types here, since we took care of that
	 message in grokdeclarator.  */
      error ("storage size of `%D' isn't known", decl);
      TREE_TYPE (decl) = error_mark_node;
    }
#if 0
  /* Keep this code around in case we later want to control debug info
     based on whether a type is "used".  (jason 1999-11-11) */

  else if (!DECL_EXTERNAL (decl) && IS_AGGR_TYPE (ttype))
    /* Let debugger know it should output info for this type.  */
    note_debug_info_needed (ttype);

  if (TREE_STATIC (decl) && DECL_CLASS_SCOPE_P (decl))
    note_debug_info_needed (DECL_CONTEXT (decl));
#endif

  if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
      && DECL_SIZE (decl) != NULL_TREE
      && ! TREE_CONSTANT (DECL_SIZE (decl)))
    {
      if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
	constant_expression_warning (DECL_SIZE (decl));
      else
	error ("storage size of `%D' isn't constant", decl);
    }

  if (TREE_STATIC (decl)
      && !DECL_ARTIFICIAL (decl)
      && current_function_decl
      && DECL_CONTEXT (decl) == current_function_decl)
    push_local_name (decl);
}

/* If a local static variable is declared in an inline function, or if
   we have a weak definition, we must endeavor to create only one
   instance of the variable at link-time.  */

static void
maybe_commonize_var (tree decl)
{
  /* Static data in a function with comdat linkage also has comdat
     linkage.  */
  if (TREE_STATIC (decl)
      /* Don't mess with __FUNCTION__.  */
      && ! DECL_ARTIFICIAL (decl)
      && DECL_FUNCTION_SCOPE_P (decl)
      /* Unfortunately, import_export_decl has not always been called
	 before the function is processed, so we cannot simply check
	 DECL_COMDAT.  */ 
      && (DECL_COMDAT (DECL_CONTEXT (decl))
	  || ((DECL_DECLARED_INLINE_P (DECL_CONTEXT (decl))
	       || DECL_TEMPLATE_INSTANTIATION (DECL_CONTEXT (decl)))
	      && TREE_PUBLIC (DECL_CONTEXT (decl)))))
    {
      if (flag_weak)
	{
	  /* With weak symbols, we simply make the variable COMDAT;
	     that will cause copies in multiple translations units to
	     be merged.  */
	  comdat_linkage (decl);
	}
      else
	{
	  if (DECL_INITIAL (decl) == NULL_TREE
	      || DECL_INITIAL (decl) == error_mark_node)
	    {
	      /* Without weak symbols, we can use COMMON to merge
		 uninitialized variables.  */
	      TREE_PUBLIC (decl) = 1;
	      DECL_COMMON (decl) = 1;
	    }
	  else
	    {
	      /* While for initialized variables, we must use internal
		 linkage -- which means that multiple copies will not
		 be merged.  */
	      TREE_PUBLIC (decl) = 0;
	      DECL_COMMON (decl) = 0;
	      cp_warning_at ("sorry: semantics of inline function static data `%#D' are wrong (you'll wind up with multiple copies)", decl);
	      warning ("%J  you can work around this by removing the initializer",
		       decl);
	    }
	}
    }
  else if (DECL_LANG_SPECIFIC (decl) && DECL_COMDAT (decl))
    /* Set it up again; we might have set DECL_INITIAL since the last
       time.  */
    comdat_linkage (decl);
}

/* Issue an error message if DECL is an uninitialized const variable.  */

static void
check_for_uninitialized_const_var (tree decl)
{
  tree type = TREE_TYPE (decl);

  /* ``Unless explicitly declared extern, a const object does not have
     external linkage and must be initialized. ($8.4; $12.1)'' ARM
     7.1.6 */
  if (TREE_CODE (decl) == VAR_DECL
      && TREE_CODE (type) != REFERENCE_TYPE
      && CP_TYPE_CONST_P (type)
      && !TYPE_NEEDS_CONSTRUCTING (type)
      && !DECL_INITIAL (decl))
    error ("uninitialized const `%D'", decl);
}

/* FIELD is a FIELD_DECL or NULL.  In the former case, the value
   returned is the next FIELD_DECL (possibly FIELD itself) that can be
   initialized.  If there are no more such fields, the return value
   will be NULL.  */

static tree
next_initializable_field (tree field)
{
  while (field
	 && (TREE_CODE (field) != FIELD_DECL
	     || (DECL_C_BIT_FIELD (field) && !DECL_NAME (field))
	     || DECL_ARTIFICIAL (field)))
    field = TREE_CHAIN (field);

  return field;
}

/* Subroutine of reshape_init. Reshape the constructor for an array. INITP
   is the pointer to the old constructor list (to the CONSTRUCTOR_ELTS of
   the CONSTRUCTOR we are processing), while NEW_INIT is the CONSTRUCTOR we
   are building.
   ELT_TYPE is the element type of the array. MAX_INDEX is an INTEGER_CST
   representing the size of the array minus one (the maximum index), or
   NULL_TREE if the array was declared without specifying the size.  */

static bool
reshape_init_array (tree elt_type, tree max_index,
		    tree *initp, tree new_init)
{
  bool sized_array_p = (max_index != NULL_TREE);
  unsigned HOST_WIDE_INT max_index_cst = 0;
  unsigned HOST_WIDE_INT index;

  if (sized_array_p)
    {
      if (host_integerp (max_index, 1))
	max_index_cst = tree_low_cst (max_index, 1);
      /* sizetype is sign extended, not zero extended.  */
      else
	max_index_cst = tree_low_cst (convert (size_type_node, max_index), 1);
    }

  /* Loop until there are no more initializers.  */
  for (index = 0;
       *initp && (!sized_array_p || index <= max_index_cst);
       ++index)
    {
      tree element_init;
      tree designated_index;

      element_init = reshape_init (elt_type, initp);
      if (element_init == error_mark_node)
	return false;
      TREE_CHAIN (element_init) = CONSTRUCTOR_ELTS (new_init);
      CONSTRUCTOR_ELTS (new_init) = element_init;
      designated_index = TREE_PURPOSE (element_init);
      if (designated_index)
	{
	  /* Handle array designated initializers (GNU extension).  */
	  if (TREE_CODE (designated_index) == IDENTIFIER_NODE)
	    {
	      error ("name `%D' used in a GNU-style designated "
		    "initializer for an array", designated_index);
	      TREE_PURPOSE (element_init) = NULL_TREE;
	    }
	  else
	    abort ();
	}
    }

  return true;
}

/* Undo the brace-elision allowed by [dcl.init.aggr] in a
   brace-enclosed aggregate initializer.

   *INITP is one of a list of initializers describing a brace-enclosed
   initializer for an entity of the indicated aggregate TYPE.  It may
   not presently match the shape of the TYPE; for example:
   
     struct S { int a; int b; };
     struct S a[] = { 1, 2, 3, 4 };

   Here *INITP will point to TREE_LIST of four elements, rather than a
   list of two elements, each itself a list of two elements.  This
   routine transforms INIT from the former form into the latter.  The
   revised initializer is returned.  */

static tree
reshape_init (tree type, tree *initp)
{
  tree inits;
  tree old_init;
  tree old_init_value;
  tree new_init;
  bool brace_enclosed_p;
  bool string_init_p;

  old_init = *initp;
  old_init_value = (TREE_CODE (*initp) == TREE_LIST
		    ? TREE_VALUE (*initp) : old_init);

  my_friendly_assert (old_init_value, 20030723);

  /* If the initializer is brace-enclosed, pull initializers from the
     enclosed elements.  Advance past the brace-enclosed initializer
     now.  */
  if (TREE_CODE (old_init_value) == CONSTRUCTOR
      && TREE_TYPE (old_init_value) == NULL_TREE
      && TREE_HAS_CONSTRUCTOR (old_init_value))
    {
      *initp = TREE_CHAIN (old_init);
      TREE_CHAIN (old_init) = NULL_TREE;
      inits = CONSTRUCTOR_ELTS (old_init_value);
      initp = &inits;
      brace_enclosed_p = true;
    }
  else
    {
      inits = NULL_TREE;
      brace_enclosed_p = false;
    }

  /* A non-aggregate type is always initialized with a single
     initializer.  */
  if (!CP_AGGREGATE_TYPE_P (type))
      {
	*initp = TREE_CHAIN (old_init);
	TREE_CHAIN (old_init) = NULL_TREE;
	/* It is invalid to initialize a non-aggregate type with a
	   brace-enclosed initializer.  */
	if (brace_enclosed_p)
	  {
	    error ("brace-enclosed initializer used to initialize `%T'",
		   type);
	    if (TREE_CODE (old_init) == TREE_LIST)
	      TREE_VALUE (old_init) = error_mark_node;
	    else
	      old_init = error_mark_node;
	  }
	
	return old_init;
      }

  /* [dcl.init.aggr]

     All implicit type conversions (clause _conv_) are considered when
     initializing the aggregate member with an initializer from an
     initializer-list.  If the initializer can initialize a member,
     the member is initialized.  Otherwise, if the member is itself a
     non-empty subaggregate, brace elision is assumed and the
     initializer is considered for the initialization of the first
     member of the subaggregate.  */
  if (!brace_enclosed_p
      && can_convert_arg (type, TREE_TYPE (old_init_value), old_init_value))
    {
      *initp = TREE_CHAIN (old_init);
      TREE_CHAIN (old_init) = NULL_TREE;
      return old_init;
    }

  string_init_p = false;
  if (TREE_CODE (old_init_value) == STRING_CST
      && TREE_CODE (type) == ARRAY_TYPE
      && char_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (type))))
    {
      /* [dcl.init.string]

	 A char array (whether plain char, signed char, or unsigned char)
	 can be initialized by a string-literal (optionally enclosed in
	 braces); a wchar_t array can be initialized by a wide
	 string-literal (optionally enclosed in braces).  */
      new_init = old_init;
      /* Move past the initializer.  */
      *initp = TREE_CHAIN (old_init);
      TREE_CHAIN (old_init) = NULL_TREE;
      string_init_p = true;
    }
  else
    {
      /* Build a CONSTRUCTOR to hold the contents of the aggregate.  */  
      new_init = build_constructor (type, NULL_TREE);
      TREE_HAS_CONSTRUCTOR (new_init) = 1;

      if (CLASS_TYPE_P (type))
	{
	  tree field;

	  field = next_initializable_field (TYPE_FIELDS (type));

	  if (!field)
	    {
	      /* [dcl.init.aggr]
	      
		 An initializer for an aggregate member that is an
		 empty class shall have the form of an empty
		 initializer-list {}.  */
	      if (!brace_enclosed_p)
		{
		  error ("initializer for `%T' must be brace-enclosed",
			 type);
		  return error_mark_node;
		}
	    }
	  else
	    {
	      /* Loop through the initializable fields, gathering
		 initializers.  */
	      while (*initp)
		{
		  tree field_init;

		  /* Handle designated initializers, as an extension.  */
		  if (TREE_PURPOSE (*initp))
		    {
		      if (pedantic)
			pedwarn ("ISO C++ does not allow designated initializers");
		      field = lookup_field_1 (type, TREE_PURPOSE (*initp),
					      /*want_type=*/false);
		      if (!field || TREE_CODE (field) != FIELD_DECL)
			error ("`%T' has no non-static data member named `%D'",
			       type, TREE_PURPOSE (*initp));
		    }
		  if (!field)
		    break;

		  field_init = reshape_init (TREE_TYPE (field), initp);
		  if (field_init == error_mark_node)
		    return error_mark_node;
		  TREE_CHAIN (field_init) = CONSTRUCTOR_ELTS (new_init);
		  CONSTRUCTOR_ELTS (new_init) = field_init;
		  /* [dcl.init.aggr] 

		     When a union  is  initialized with a brace-enclosed
		     initializer, the braces shall only contain an
		     initializer for the first member of the union.  */
		  if (TREE_CODE (type) == UNION_TYPE)
		    break;
		  field = next_initializable_field (TREE_CHAIN (field));
		}
	    }
	}
      else if ((TREE_CODE (type) == ARRAY_TYPE)|| (TREE_CODE (type) == VECTOR_TYPE))
	{
	  tree max_index;

	  /* If the bound of the array is known, take no more initializers
	     than are allowed.  */
	  max_index = ((TYPE_DOMAIN (type) && (TREE_CODE (type) == ARRAY_TYPE))
		       ? array_type_nelts (type) : NULL_TREE);
	  if (!reshape_init_array (TREE_TYPE (type), max_index,
				   initp, new_init))
	    return error_mark_node;
	}
      else
	abort ();

      /* The initializers were placed in reverse order in the
	 CONSTRUCTOR.  */
      CONSTRUCTOR_ELTS (new_init) = nreverse (CONSTRUCTOR_ELTS (new_init));

      if (TREE_CODE (old_init) == TREE_LIST)
	new_init = build_tree_list (TREE_PURPOSE (old_init), new_init);
    }

  /* If there are more initializers than necessary, issue a
     diagnostic.  */  
  if (*initp)
    {
      if (brace_enclosed_p)
	error ("too many initializers for `%T'", type);
      else if (warn_missing_braces && !string_init_p)
	warning ("missing braces around initializer");
    }

  return new_init;
}

/* Verify INIT (the initializer for DECL), and record the
   initialization in DECL_INITIAL, if appropriate.  CLEANUP is as for
   grok_reference_init.

   If the return value is non-NULL, it is an expression that must be
   evaluated dynamically to initialize DECL.  */

static tree
check_initializer (tree decl, tree init, int flags, tree *cleanup)
{
  tree type = TREE_TYPE (decl);
  tree init_code = NULL;

  /* If `start_decl' didn't like having an initialization, ignore it now.  */
  if (init != NULL_TREE && DECL_INITIAL (decl) == NULL_TREE)
    init = NULL_TREE;

  /* If an initializer is present, DECL_INITIAL has been
     error_mark_node, to indicate that an as-of-yet unevaluated
     initialization will occur.  From now on, DECL_INITIAL reflects
     the static initialization -- if any -- of DECL.  */
  DECL_INITIAL (decl) = NULL_TREE;

  /* Things that are going to be initialized need to have complete
     type.  */
  TREE_TYPE (decl) = type = complete_type (TREE_TYPE (decl));

  if (type == error_mark_node)
    /* We will have already complained.  */
    init = NULL_TREE;
  else if (init && COMPLETE_TYPE_P (type) 
	   && !TREE_CONSTANT (TYPE_SIZE (type)))
    {
      error ("variable-sized object `%D' may not be initialized", decl);
      init = NULL_TREE;
    }
  else if (TREE_CODE (type) == ARRAY_TYPE
	   && !COMPLETE_TYPE_P (complete_type (TREE_TYPE (type))))
    {
      error ("elements of array `%#D' have incomplete type", decl);
      init = NULL_TREE;
    }
  else if (TREE_CODE (type) != ARRAY_TYPE && !COMPLETE_TYPE_P (type))
    {
      error ("`%D' has incomplete type", decl);
      TREE_TYPE (decl) = error_mark_node;
      init = NULL_TREE;
    }

  if (TREE_CODE (decl) == CONST_DECL)
    {
      my_friendly_assert (TREE_CODE (decl) != REFERENCE_TYPE, 148);

      DECL_INITIAL (decl) = init;

      my_friendly_assert (init != NULL_TREE, 149);
      init = NULL_TREE;
    }
  else if (!DECL_EXTERNAL (decl) && TREE_CODE (type) == REFERENCE_TYPE)
    init = grok_reference_init (decl, type, init, cleanup);
  else if (init)
    {
      if (TREE_CODE (init) == CONSTRUCTOR && TREE_HAS_CONSTRUCTOR (init))
	{
	  /* [dcl.init] paragraph 13,
	     If T is a scalar type, then a declaration of the form
	     T x = { a };
	     is equivalent to
	     T x = a;
	     
	     reshape_init will complain about the extra braces,
	     and doesn't do anything useful in the case where TYPE is
	     scalar, so just don't call it.  */
	  if (CP_AGGREGATE_TYPE_P (type))
	    init = reshape_init (type, &init);

	  if ((*targetm.vector_opaque_p) (type))
	    {
	      error ("opaque vector types cannot be initialized");
	      init = error_mark_node;
	    }
	}

      /* If DECL has an array type without a specific bound, deduce the
	 array size from the initializer.  */
      maybe_deduce_size_from_array_init (decl, init);
      type = TREE_TYPE (decl);
      if (TREE_CODE (init) == CONSTRUCTOR && TREE_HAS_CONSTRUCTOR (init))
	TREE_TYPE (init) = type;

      if (TYPE_HAS_CONSTRUCTOR (type) || TYPE_NEEDS_CONSTRUCTING (type))
	{
	  if (TREE_CODE (type) == ARRAY_TYPE)
	    goto initialize_aggr;
	  else if (TREE_CODE (init) == CONSTRUCTOR
		   && TREE_HAS_CONSTRUCTOR (init))
	    {
	      if (TYPE_NON_AGGREGATE_CLASS (type))
		{
		  error ("`%D' must be initialized by constructor, not by `{...}'",
			 decl);
		  init = error_mark_node;
		}
	      else
		goto dont_use_constructor;
	    }
	  else
	    {
	      int saved_stmts_are_full_exprs_p;

	    initialize_aggr:
	      saved_stmts_are_full_exprs_p = 0;
	      if (building_stmt_tree ())
		{
		  saved_stmts_are_full_exprs_p = stmts_are_full_exprs_p ();
		  current_stmt_tree ()->stmts_are_full_exprs_p = 1;
		}
	      init = build_aggr_init (decl, init, flags);
	      if (building_stmt_tree ())
		current_stmt_tree ()->stmts_are_full_exprs_p =
		  saved_stmts_are_full_exprs_p;
	      return init;
	    }
	}
      else
	{
	dont_use_constructor:
	  if (TREE_CODE (init) != TREE_VEC)
	    {
	      init_code = store_init_value (decl, init);
	      init = NULL;
	    }
	}
    }
  else if (DECL_EXTERNAL (decl))
    ;
  else if (TYPE_P (type) && TYPE_NEEDS_CONSTRUCTING (type))
    goto initialize_aggr;
  else if (IS_AGGR_TYPE (type))
    {
      tree core_type = strip_array_types (type);

      if (CLASSTYPE_READONLY_FIELDS_NEED_INIT (core_type))
	error ("structure `%D' with uninitialized const members", decl);
      if (CLASSTYPE_REF_FIELDS_NEED_INIT (core_type))
	error ("structure `%D' with uninitialized reference members",
	       decl);

      check_for_uninitialized_const_var (decl);
    }
  else
    check_for_uninitialized_const_var (decl);

  if (init && init != error_mark_node)
    init_code = build (INIT_EXPR, type, decl, init);

  return init_code;
}

/* If DECL is not a local variable, give it RTL.  */

static void
make_rtl_for_nonlocal_decl (tree decl, tree init, const char* asmspec)
{
  int toplev = toplevel_bindings_p ();
  int defer_p;

  /* Handle non-variables up front.  */
  if (TREE_CODE (decl) != VAR_DECL)
    {
      rest_of_decl_compilation (decl, asmspec, toplev, at_eof);
      return;
    }

  /* If we see a class member here, it should be a static data
     member.  */
  if (DECL_LANG_SPECIFIC (decl) && DECL_IN_AGGR_P (decl))
    {
      my_friendly_assert (TREE_STATIC (decl), 19990828);
      /* An in-class declaration of a static data member should be
	 external; it is only a declaration, and not a definition.  */
      if (init == NULL_TREE)
	my_friendly_assert (DECL_EXTERNAL (decl), 20000723);
    }

  /* Set the DECL_ASSEMBLER_NAME for the variable.  */
  if (asmspec)
    {
      change_decl_assembler_name (decl, get_identifier (asmspec));
      /* The `register' keyword, when used together with an
	 asm-specification, indicates that the variable should be
	 placed in a particular register.  */
      if (DECL_REGISTER (decl))
	DECL_C_HARD_REGISTER (decl) = 1;
    }

  /* We don't create any RTL for local variables.  */
  if (DECL_FUNCTION_SCOPE_P (decl) && !TREE_STATIC (decl))
    return;

  /* We defer emission of local statics until the corresponding
     DECL_STMT is expanded.  */
  defer_p = DECL_FUNCTION_SCOPE_P (decl) || DECL_VIRTUAL_P (decl);

  /* We try to defer namespace-scope static constants so that they are
     not emitted into the object file unnecessarily.  */
  if (!DECL_VIRTUAL_P (decl)
      && TREE_READONLY (decl)
      && DECL_INITIAL (decl) != NULL_TREE
      && DECL_INITIAL (decl) != error_mark_node
      && ! EMPTY_CONSTRUCTOR_P (DECL_INITIAL (decl))
      && toplev
      && !TREE_PUBLIC (decl))
    {
      /* Fool with the linkage of static consts according to #pragma
	 interface.  */
      if (!interface_unknown && !TREE_PUBLIC (decl))
	{
	  TREE_PUBLIC (decl) = 1;
	  DECL_EXTERNAL (decl) = interface_only;
	}

      defer_p = 1;
    }
  /* Likewise for template instantiations.  */
  else if (DECL_COMDAT (decl))
    defer_p = 1;

  /* If we're deferring the variable, we only need to make RTL if
     there's an ASMSPEC.  Otherwise, we'll lazily create it later when
     we need it.  (There's no way to lazily create RTL for things that
     have assembly specs because the information about the specifier
     isn't stored in the tree, yet)  */
  if (defer_p && asmspec)
    make_decl_rtl (decl, asmspec);
  /* If we're not deferring, go ahead and assemble the variable.  */
  else if (!defer_p)
    rest_of_decl_compilation (decl, asmspec, toplev, at_eof);
}

/* Generate code to initialize DECL (a local variable).  */

static void
initialize_local_var (tree decl, tree init)
{
  tree type = TREE_TYPE (decl);
  tree cleanup;

  my_friendly_assert (TREE_CODE (decl) == VAR_DECL
		      || TREE_CODE (decl) == RESULT_DECL, 
		      20021010);
  my_friendly_assert (!TREE_STATIC (decl), 20021010);

  if (DECL_SIZE (decl) == NULL_TREE)
    {
      /* If we used it already as memory, it must stay in memory.  */
      DECL_INITIAL (decl) = NULL_TREE;
      TREE_ADDRESSABLE (decl) = TREE_USED (decl);
    }

  if (DECL_SIZE (decl) && type != error_mark_node)
    {
      int already_used;

      /* Compute and store the initial value.  */
      already_used = TREE_USED (decl) || TREE_USED (type);

      /* Perform the initialization.  */
      if (init)
	{
	  int saved_stmts_are_full_exprs_p;

	  my_friendly_assert (building_stmt_tree (), 20000906);
	  saved_stmts_are_full_exprs_p = stmts_are_full_exprs_p ();
	  current_stmt_tree ()->stmts_are_full_exprs_p = 1;
	  finish_expr_stmt (init);
	  current_stmt_tree ()->stmts_are_full_exprs_p =
	    saved_stmts_are_full_exprs_p;
	}

      /* Set this to 0 so we can tell whether an aggregate which was
	 initialized was ever used.  Don't do this if it has a
	 destructor, so we don't complain about the 'resource
	 allocation is initialization' idiom.  Now set
	 attribute((unused)) on types so decls of that type will be
	 marked used. (see TREE_USED, above.)  */
      if (TYPE_NEEDS_CONSTRUCTING (type)
	  && ! already_used
	  && TYPE_HAS_TRIVIAL_DESTRUCTOR (type)
	  && DECL_NAME (decl))
	TREE_USED (decl) = 0;
      else if (already_used)
	TREE_USED (decl) = 1;
    }

  /* Generate a cleanup, if necessary.  */
  cleanup = cxx_maybe_build_cleanup (decl);
  if (DECL_SIZE (decl) && cleanup)
    finish_decl_cleanup (decl, cleanup);
}

/* Finish processing of a declaration;
   install its line number and initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.

   INIT holds the value of an initializer that should be allowed to escape
   the normal rules.

   FLAGS is LOOKUP_ONLYCONVERTING if the = init syntax was used, else 0
   if the (init) syntax was used.  */

void
cp_finish_decl (tree decl, tree init, tree asmspec_tree, int flags)
{
  tree type;
  tree ttype = NULL_TREE;
  tree cleanup;
  const char *asmspec = NULL;
  int was_readonly = 0;
  bool var_definition_p = false;

  if (decl == error_mark_node)
    return;
  else if (! decl)
    {
      if (init)
	error ("assignment (not initialization) in declaration");
      return;
    }

  my_friendly_assert (TREE_CODE (decl) != RESULT_DECL, 20030619);

  /* Assume no cleanup is required.  */
  cleanup = NULL_TREE;

  /* If a name was specified, get the string.  */
  if (global_scope_p (current_binding_level))
    asmspec_tree = maybe_apply_renaming_pragma (decl, asmspec_tree);
  if (asmspec_tree) 
    asmspec = TREE_STRING_POINTER (asmspec_tree);

  if (init && TREE_CODE (init) == NAMESPACE_DECL)
    {
      error ("cannot initialize `%D' to namespace `%D'",
		decl, init);
      init = NULL_TREE;
    }

  if (current_class_type
      && CP_DECL_CONTEXT (decl) == current_class_type
      && TYPE_BEING_DEFINED (current_class_type)
      && (DECL_INITIAL (decl) || init))
    DECL_INITIALIZED_IN_CLASS_P (decl) = 1;

  if (TREE_CODE (decl) == VAR_DECL
      && DECL_CONTEXT (decl)
      && TREE_CODE (DECL_CONTEXT (decl)) == NAMESPACE_DECL
      && DECL_CONTEXT (decl) != current_namespace
      && init)
    {
      /* Leave the namespace of the object.  */
      pop_decl_namespace ();
    }

  type = TREE_TYPE (decl);

  if (type == error_mark_node)
    goto finish_end0;

  if (TYPE_HAS_MUTABLE_P (type))
    TREE_READONLY (decl) = 0;

  if (processing_template_decl)
    {
      /* Add this declaration to the statement-tree.  */
      if (at_function_scope_p ())
	add_decl_stmt (decl);

      if (init && DECL_INITIAL (decl))
	DECL_INITIAL (decl) = init;
      if (TREE_CODE (decl) == VAR_DECL
	  && !DECL_PRETTY_FUNCTION_P (decl)
	  && !dependent_type_p (TREE_TYPE (decl)))
	maybe_deduce_size_from_array_init (decl, init);
      goto finish_end0;
    }

  /* Parameters are handled by store_parm_decls, not cp_finish_decl.  */
  my_friendly_assert (TREE_CODE (decl) != PARM_DECL, 19990828);

  /* Take care of TYPE_DECLs up front.  */
  if (TREE_CODE (decl) == TYPE_DECL)
    {
      if (type != error_mark_node
	  && IS_AGGR_TYPE (type) && DECL_NAME (decl))
	{
	  if (TREE_TYPE (DECL_NAME (decl)) && TREE_TYPE (decl) != type)
	    warning ("shadowing previous type declaration of `%#D'", decl);
	  set_identifier_type_value (DECL_NAME (decl), decl);
	}

      /* If we have installed this as the canonical typedef for this
	 type, and that type has not been defined yet, delay emitting
	 the debug information for it, as we will emit it later.  */
      if (TYPE_MAIN_DECL (TREE_TYPE (decl)) == decl
	  && !COMPLETE_TYPE_P (TREE_TYPE (decl)))
	TYPE_DECL_SUPPRESS_DEBUG (decl) = 1;

      rest_of_decl_compilation (decl, NULL,
				DECL_CONTEXT (decl) == NULL_TREE, at_eof);
      goto finish_end;
    }

  if (TREE_CODE (decl) != FUNCTION_DECL)
    ttype = target_type (type);

  
  /* Currently, GNU C++ puts constants in text space, making them
     impossible to initialize.  In the future, one would hope for
     an operating system which understood the difference between
     initialization and the running of a program.  */
  if (! DECL_EXTERNAL (decl) && TREE_READONLY (decl))
    {
      was_readonly = 1;
      if (TYPE_NEEDS_CONSTRUCTING (type) 
	  || TREE_CODE (type) == REFERENCE_TYPE)
	TREE_READONLY (decl) = 0;
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      /* Only PODs can have thread-local storage.  Other types may require
	 various kinds of non-trivial initialization.  */
      if (DECL_THREAD_LOCAL (decl) && !pod_type_p (TREE_TYPE (decl)))
	error ("`%D' cannot be thread-local because it has non-POD type `%T'",
	       decl, TREE_TYPE (decl));
      /* Convert the initializer to the type of DECL, if we have not
	 already initialized DECL.  */
      if (!DECL_INITIALIZED_P (decl)
	  /* If !DECL_EXTERNAL then DECL is being defined.  In the
	     case of a static data member initialized inside the
	     class-specifier, there can be an initializer even if DECL
	     is *not* defined.  */
	  && (!DECL_EXTERNAL (decl) || init))
	{
	  init = check_initializer (decl, init, flags, &cleanup);
	  /* Thread-local storage cannot be dynamically initialized.  */
	  if (DECL_THREAD_LOCAL (decl) && init)
	    {
	      error ("`%D' is thread-local and so cannot be dynamically "
		     "initialized", decl);
	      init = NULL_TREE;
	    }
	  /* Handle:
	     
	     [dcl.init]
	     
	     The memory occupied by any object of static storage
	     duration is zero-initialized at program startup before
	     any other initialization takes place.
	     
	     We cannot create an appropriate initializer until after
	     the type of DECL is finalized.  If DECL_INITIAL is set,
	     then the DECL is statically initialized, and any
	     necessary zero-initialization has already been performed.  */
	  if (TREE_STATIC (decl) && !DECL_INITIAL (decl))
	    DECL_INITIAL (decl) = build_zero_init (TREE_TYPE (decl),
						   /*nelts=*/NULL_TREE,
						   /*static_storage_p=*/true);
	  /* Remember that the initialization for this variable has
	     taken place.  */
	  DECL_INITIALIZED_P (decl) = 1;
	  /* This declaration is the definition of this variable,
	     unless we are initializing a static data member within
	     the class specifier.  */
	  if (!DECL_EXTERNAL (decl))
	    var_definition_p = true;
	}
      /* If the variable has an array type, lay out the type, even if
	 there is no initializer.  It is valid to index through the
	 array, and we must get TYPE_ALIGN set correctly on the array
	 type.  */
      else if (TREE_CODE (type) == ARRAY_TYPE)
	layout_type (type);
    }

  /* Add this declaration to the statement-tree.  This needs to happen
     after the call to check_initializer so that the DECL_STMT for a
     reference temp is added before the DECL_STMT for the reference itself.  */
  if (at_function_scope_p ())
    add_decl_stmt (decl);

  if (TREE_CODE (decl) == VAR_DECL)
    layout_var_decl (decl);

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */
  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (TREE_CODE (decl) == VAR_DECL)
	maybe_commonize_var (decl);

      make_rtl_for_nonlocal_decl (decl, init, asmspec);

      if (TREE_CODE (type) == FUNCTION_TYPE
	  || TREE_CODE (type) == METHOD_TYPE)
	abstract_virtuals_error (decl,
				 strip_array_types (TREE_TYPE (type)));
      else if (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
      {
	/* If it's either a pointer or an array type, strip through all
	   of them but the last one. If the last is an array type, issue 
	   an error if the element type is abstract.  */
	while (POINTER_TYPE_P (TREE_TYPE (type)) 
	       || TREE_CODE (TREE_TYPE (type)) == ARRAY_TYPE)
	  type = TREE_TYPE (type);
	if (TREE_CODE (type) == ARRAY_TYPE)
	  abstract_virtuals_error (decl, TREE_TYPE (type));
      }
      else
	abstract_virtuals_error (decl, type);

      if (TREE_CODE (decl) == FUNCTION_DECL 
	  || TREE_TYPE (decl) == error_mark_node)
	/* No initialization required.  */
	;
      else if (DECL_EXTERNAL (decl)
	       && ! (DECL_LANG_SPECIFIC (decl)
		     && DECL_NOT_REALLY_EXTERN (decl)))
	{
	  if (init)
	    DECL_INITIAL (decl) = init;
	}
      else
	{
	  /* A variable definition.  */
	  if (DECL_FUNCTION_SCOPE_P (decl))
	    {
	      /* This is a local declaration.  */
	      maybe_inject_for_scope_var (decl);
	      /* Initialize the local variable.  */
	      if (processing_template_decl)
		{
		  if (init || DECL_INITIAL (decl) == error_mark_node)
		    DECL_INITIAL (decl) = init;
		}
	      else if (!TREE_STATIC (decl))
		initialize_local_var (decl, init);
	    }

	  /* If a variable is defined, and then a subsequent
	     definintion with external linkage is encountered, we will
	     get here twice for the same variable.  We want to avoid
	     calling expand_static_init more than once.  For variables
	     that are not static data members, we can call
	     expand_static_init only when we actually process the
	     initializer.  It is not legal to redeclare a static data
	     member, so this issue does not arise in that case.  */
	  if (var_definition_p && TREE_STATIC (decl))
	    expand_static_init (decl, init); 
	}
    finish_end0:

      /* Undo call to `pushclass' that was done in `start_decl'
	 due to initialization of qualified member variable.
	 I.e., Foo::x = 10;  */
      {
	tree context = CP_DECL_CONTEXT (decl);
	if (context
	    && TYPE_P (context)
	    && (TREE_CODE (decl) == VAR_DECL
		/* We also have a pushclass done that we need to undo here
		   if we're at top level and declare a method.  */
		|| TREE_CODE (decl) == FUNCTION_DECL)
	    /* If size hasn't been set, we're still defining it,
	       and therefore inside the class body; don't pop
	       the binding level..  */
	    && COMPLETE_TYPE_P (context)
	    && context == current_class_type)
	  pop_nested_class ();
      }
    }

  /* If a CLEANUP_STMT was created to destroy a temporary bound to a
     reference, insert it in the statement-tree now.  */
  if (cleanup)
    add_stmt (cleanup);

 finish_end:

  if (was_readonly)
    TREE_READONLY (decl) = 1;

  /* If this was marked 'used', be sure it will be output.  */
  if (lookup_attribute ("used", DECL_ATTRIBUTES (decl)))
    mark_referenced (DECL_ASSEMBLER_NAME (decl));
}

/* This is here for a midend callback from c-common.c.  */

void
finish_decl (tree decl, tree init, tree asmspec_tree)
{
  cp_finish_decl (decl, init, asmspec_tree, 0);
}

/* Returns a declaration for a VAR_DECL as if:

     extern "C" TYPE NAME;

   had been seen.  Used to create compiler-generated global
   variables.  */

tree
declare_global_var (tree name, tree type)
{
  tree decl;

  push_to_top_level ();
  decl = build_decl (VAR_DECL, name, type);
  TREE_PUBLIC (decl) = 1;
  DECL_EXTERNAL (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  pushdecl (decl);
  cp_finish_decl (decl, NULL_TREE, NULL_TREE, 0);
  pop_from_top_level ();

  return decl;
}

/* Returns a pointer to the `atexit' function.  Note that if
   FLAG_USE_CXA_ATEXIT is nonzero, then this will actually be the new
   `__cxa_atexit' function specified in the IA64 C++ ABI.  */

static tree
get_atexit_node (void)
{
  tree atexit_fndecl;
  tree arg_types;
  tree fn_type;
  tree fn_ptr_type;
  const char *name;

  if (atexit_node)
    return atexit_node;

  if (flag_use_cxa_atexit)
    {
      /* The declaration for `__cxa_atexit' is:

	   int __cxa_atexit (void (*)(void *), void *, void *)

	 We build up the argument types and then then function type
	 itself.  */

      /* First, build the pointer-to-function type for the first
	 argument.  */
      arg_types = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
      fn_type = build_function_type (void_type_node, arg_types);
      fn_ptr_type = build_pointer_type (fn_type);
      /* Then, build the rest of the argument types.  */
      arg_types = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
      arg_types = tree_cons (NULL_TREE, ptr_type_node, arg_types);
      arg_types = tree_cons (NULL_TREE, fn_ptr_type, arg_types);
      /* And the final __cxa_atexit type.  */
      fn_type = build_function_type (integer_type_node, arg_types);
      fn_ptr_type = build_pointer_type (fn_type);
      name = "__cxa_atexit";
    }
  else
    {
      /* The declaration for `atexit' is:

           int atexit (void (*)());

	 We build up the argument types and then then function type
	 itself.  */
      fn_type = build_function_type (void_type_node, void_list_node);
      fn_ptr_type = build_pointer_type (fn_type);
      arg_types = tree_cons (NULL_TREE, fn_ptr_type, void_list_node);
      /* Build the final atexit type.  */
      fn_type = build_function_type (integer_type_node, arg_types);
      name = "atexit";
    }

  /* Now, build the function declaration.  */
  push_lang_context (lang_name_c);
  atexit_fndecl = build_library_fn_ptr (name, fn_type);
  mark_used (atexit_fndecl);
  pop_lang_context ();
  atexit_node = decay_conversion (atexit_fndecl);

  return atexit_node;
}

/* Returns the __dso_handle VAR_DECL.  */

static tree
get_dso_handle_node (void)
{
  if (dso_handle_node)
    return dso_handle_node;

  /* Declare the variable.  */
  dso_handle_node = declare_global_var (get_identifier ("__dso_handle"),
					ptr_type_node);

  return dso_handle_node;
}

/* Begin a new function with internal linkage whose job will be simply
   to destroy some particular variable.  */

static GTY(()) int start_cleanup_cnt;

static tree
start_cleanup_fn (void)
{
  int old_interface_only = interface_only;
  int old_interface_unknown = interface_unknown;
  char name[32];
  tree parmtypes;
  tree fntype;
  tree fndecl;

  push_to_top_level ();

  /* No need to mangle this.  */
  push_lang_context (lang_name_c);

  interface_only = 0;
  interface_unknown = 1;

  /* Build the parameter-types.  */
  parmtypes = void_list_node;
  /* Functions passed to __cxa_atexit take an additional parameter.
     We'll just ignore it.  After we implement the new calling
     convention for destructors, we can eliminate the use of
     additional cleanup functions entirely in the -fnew-abi case.  */
  if (flag_use_cxa_atexit)
    parmtypes = tree_cons (NULL_TREE, ptr_type_node, parmtypes);
  /* Build the function type itself.  */
  fntype = build_function_type (void_type_node, parmtypes);
  /* Build the name of the function.  */
  sprintf (name, "__tcf_%d", start_cleanup_cnt++);
  /* Build the function declaration.  */
  fndecl = build_lang_decl (FUNCTION_DECL, get_identifier (name), fntype);
  /* It's a function with internal linkage, generated by the
     compiler.  */
  TREE_PUBLIC (fndecl) = 0;
  DECL_ARTIFICIAL (fndecl) = 1;
  /* Make the function `inline' so that it is only emitted if it is
     actually needed.  It is unlikely that it will be inlined, since
     it is only called via a function pointer, but we avoid unnecessary
     emissions this way.  */
  DECL_INLINE (fndecl) = 1;
  DECL_DECLARED_INLINE_P (fndecl) = 1;
  DECL_INTERFACE_KNOWN (fndecl) = 1;
  /* Build the parameter.  */
  if (flag_use_cxa_atexit)
    {
      tree parmdecl;

      parmdecl = cp_build_parm_decl (NULL_TREE, ptr_type_node);
      DECL_CONTEXT (parmdecl) = fndecl;
      TREE_USED (parmdecl) = 1;
      DECL_ARGUMENTS (fndecl) = parmdecl;
    }

  pushdecl (fndecl);
  start_function (/*specs=*/NULL_TREE, fndecl, NULL_TREE, SF_PRE_PARSED);

  interface_unknown = old_interface_unknown;
  interface_only = old_interface_only;

  pop_lang_context ();

  return current_function_decl;
}

/* Finish the cleanup function begun by start_cleanup_fn.  */

static void
end_cleanup_fn (void)
{
  expand_or_defer_fn (finish_function (0));

  pop_from_top_level ();
}

/* Generate code to handle the destruction of DECL, an object with
   static storage duration.  */

void
register_dtor_fn (tree decl)
{
  tree cleanup;
  tree compound_stmt;
  tree args;
  tree fcall;

  if (TYPE_HAS_TRIVIAL_DESTRUCTOR (TREE_TYPE (decl)))
    return;

  /* Call build_cleanup before we enter the anonymous function so that
     any access checks will be done relative to the current scope,
     rather than the scope of the anonymous function.  */
  build_cleanup (decl);

  /* Now start the function.  */
  cleanup = start_cleanup_fn ();

  /* Now, recompute the cleanup.  It may contain SAVE_EXPRs that refer
     to the original function, rather than the anonymous one.  That
     will make the back-end think that nested functions are in use,
     which causes confusion.  */
  
  push_deferring_access_checks (dk_no_check);
  fcall = build_cleanup (decl);
  pop_deferring_access_checks ();

  /* Create the body of the anonymous function.  */
  compound_stmt = begin_compound_stmt (/*has_no_scope=*/false);
  finish_expr_stmt (fcall);
  finish_compound_stmt (compound_stmt);
  end_cleanup_fn ();

  /* Call atexit with the cleanup function.  */
  cxx_mark_addressable (cleanup);
  mark_used (cleanup);
  cleanup = build_unary_op (ADDR_EXPR, cleanup, 0);
  if (flag_use_cxa_atexit)
    {
      args = tree_cons (NULL_TREE, 
			build_unary_op (ADDR_EXPR, get_dso_handle_node (), 0),
			NULL_TREE);
      args = tree_cons (NULL_TREE, null_pointer_node, args);
      args = tree_cons (NULL_TREE, cleanup, args);
    }
  else
    args = tree_cons (NULL_TREE, cleanup, NULL_TREE);
  finish_expr_stmt (build_function_call (get_atexit_node (), args));
}

/* DECL is a VAR_DECL with static storage duration.  INIT, if present,
   is its initializer.  Generate code to handle the construction
   and destruction of DECL.  */

static void
expand_static_init (tree decl, tree init)
{
  my_friendly_assert (TREE_CODE (decl) == VAR_DECL, 20021010);
  my_friendly_assert (TREE_STATIC (decl), 20021010);

  /* Some variables require no initialization.  */
  if (!init 
      && !TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl))
      && TYPE_HAS_TRIVIAL_DESTRUCTOR (TREE_TYPE (decl)))
    return;

  if (! toplevel_bindings_p ())
    {
      /* Emit code to perform this initialization but once.  */
      tree if_stmt;
      tree then_clause;
      tree assignment;
      tree guard;
      tree guard_init;

      /* Emit code to perform this initialization but once.  This code
	 looks like:

           static int guard = 0;
           if (!guard) {
             // Do initialization.
	     guard = 1;
	     // Register variable for destruction at end of program.
	   }

	 Note that the `temp' variable is only set to 1 *after* the
	 initialization is complete.  This ensures that an exception,
	 thrown during the construction, will cause the variable to
	 reinitialized when we pass through this code again, as per:

	   [stmt.dcl]

	   If the initialization exits by throwing an exception, the
	   initialization is not complete, so it will be tried again
	   the next time control enters the declaration.

         In theory, this process should be thread-safe, too; multiple
	 threads should not be able to initialize the variable more
	 than once.  We don't yet attempt to ensure thread-safety.  */

      /* Create the guard variable.  */
      guard = get_guard (decl);

      /* Begin the conditional initialization.  */
      if_stmt = begin_if_stmt ();
      finish_if_stmt_cond (get_guard_cond (guard), if_stmt);
      then_clause = begin_compound_stmt (/*has_no_scope=*/false);

      /* Do the initialization itself.  */
      assignment = init ? init : NULL_TREE;

      /* Once the assignment is complete, set TEMP to 1.  Since the
	 construction of the static object is complete at this point,
	 we want to make sure TEMP is set to 1 even if a temporary
	 constructed during the initialization throws an exception
	 when it is destroyed.  So, we combine the initialization and
	 the assignment to TEMP into a single expression, ensuring
	 that when we call finish_expr_stmt the cleanups will not be
	 run until after TEMP is set to 1.  */
      guard_init = set_guard (guard);
      if (assignment)
	assignment = build_compound_expr (assignment, guard_init);
      else
	assignment = guard_init;
      finish_expr_stmt (assignment);

      /* Use atexit to register a function for destroying this static
	 variable.  */
      register_dtor_fn (decl);

      finish_compound_stmt (then_clause);
      finish_then_clause (if_stmt);
      finish_if_stmt ();
    }
  else
    static_aggregates = tree_cons (init, decl, static_aggregates);
}

/* Finish the declaration of a catch-parameter.  */

tree
start_handler_parms (tree declspecs, tree declarator)
{
  tree decl;
  if (declspecs)
    {
      decl = grokdeclarator (declarator, declspecs, CATCHPARM,
			     1, NULL);
      if (decl == NULL_TREE)
	error ("invalid catch parameter");
    }
  else
    decl = NULL_TREE;

  return decl;
}


/* Make TYPE a complete type based on INITIAL_VALUE.
   Return 0 if successful, 1 if INITIAL_VALUE can't be deciphered,
   2 if there was no information (in which case assume 0 if DO_DEFAULT).  */

int
complete_array_type (tree type, tree initial_value, int do_default)
{
  tree maxindex = NULL_TREE;
  int value = 0;

  if (initial_value)
    {
      /* An array of character type can be initialized from a
	 brace-enclosed string constant.  */
      if (char_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (type)))
	  && TREE_CODE (initial_value) == CONSTRUCTOR
	  && CONSTRUCTOR_ELTS (initial_value)
	  && (TREE_CODE (TREE_VALUE (CONSTRUCTOR_ELTS (initial_value)))
	      == STRING_CST)
	  && TREE_CHAIN (CONSTRUCTOR_ELTS (initial_value)) == NULL_TREE)
	initial_value = TREE_VALUE (CONSTRUCTOR_ELTS (initial_value));

      /* Note MAXINDEX is really the maximum index, one less than the
	 size.  */
      if (TREE_CODE (initial_value) == STRING_CST)
	{
	  int eltsize
	    = int_size_in_bytes (TREE_TYPE (TREE_TYPE (initial_value)));
	  maxindex = build_int_2 ((TREE_STRING_LENGTH (initial_value)
				   / eltsize) - 1, 0);
	}
      else if (TREE_CODE (initial_value) == CONSTRUCTOR)
	{
	  tree elts = CONSTRUCTOR_ELTS (initial_value);

	  maxindex = ssize_int (-1);
	  for (; elts; elts = TREE_CHAIN (elts))
	    {
	      if (TREE_PURPOSE (elts))
		maxindex = TREE_PURPOSE (elts);
	      else
		maxindex = size_binop (PLUS_EXPR, maxindex, ssize_int (1));
	    }
	  maxindex = copy_node (maxindex);
	}
      else
	{
	  /* Make an error message unless that happened already.  */
	  if (initial_value != error_mark_node)
	    value = 1;
	  else
	    initial_value = NULL_TREE;

	  /* Prevent further error messages.  */
	  maxindex = build_int_2 (0, 0);
	}
    }

  if (!maxindex)
    {
      if (do_default)
	maxindex = build_int_2 (0, 0);
      value = 2;
    }

  if (maxindex)
    {
      tree itype;
      tree domain;

      domain = build_index_type (maxindex);
      TYPE_DOMAIN (type) = domain;

      if (! TREE_TYPE (maxindex))
	TREE_TYPE (maxindex) = domain;
      if (initial_value)
        itype = TREE_TYPE (initial_value);
      else
	itype = NULL;
      if (itype && !TYPE_DOMAIN (itype))
	TYPE_DOMAIN (itype) = domain;
      /* The type of the main variant should never be used for arrays
	 of different sizes.  It should only ever be completed with the
	 size of the array.  */
      if (! TYPE_DOMAIN (TYPE_MAIN_VARIANT (type)))
	TYPE_DOMAIN (TYPE_MAIN_VARIANT (type)) = domain;
    }

  /* Lay out the type now that we can get the real answer.  */

  layout_type (type);

  return value;
}

/* Return zero if something is declared to be a member of type
   CTYPE when in the context of CUR_TYPE.  STRING is the error
   message to print in that case.  Otherwise, quietly return 1.  */

static int
member_function_or_else (tree ctype, tree cur_type, enum overload_flags flags)
{
  if (ctype && ctype != cur_type)
    {
      if (flags == DTOR_FLAG)
	error ("destructor for alien class `%T' cannot be a member",
	          ctype);
      else
	error ("constructor for alien class `%T' cannot be a member",
	          ctype);
      return 0;
    }
  return 1;
}

/* Subroutine of `grokdeclarator'.  */

/* Generate errors possibly applicable for a given set of specifiers.
   This is for ARM $7.1.2.  */

static void
bad_specifiers (tree object,
                const char* type,
                int virtualp,
                int quals,
                int inlinep,
                int friendp,
                int raises)
{
  if (virtualp)
    error ("`%D' declared as a `virtual' %s", object, type);
  if (inlinep)
    error ("`%D' declared as an `inline' %s", object, type);
  if (quals)
    error ("`const' and `volatile' function specifiers on `%D' invalid in %s declaration",
	      object, type);
  if (friendp)
    cp_error_at ("`%D' declared as a friend", object);
  if (raises
      && (TREE_CODE (object) == TYPE_DECL
	  || (!TYPE_PTRFN_P (TREE_TYPE (object))
	      && !TYPE_REFFN_P (TREE_TYPE (object))
	      && !TYPE_PTRMEMFUNC_P (TREE_TYPE (object)))))
    cp_error_at ("`%D' declared with an exception specification", object);
}

/* CTYPE is class type, or null if non-class.
   TYPE is type this FUNCTION_DECL should have, either FUNCTION_TYPE
   or METHOD_TYPE.
   DECLARATOR is the function's name.
   PARMS is a chain of PARM_DECLs for the function.
   VIRTUALP is truthvalue of whether the function is virtual or not.
   FLAGS are to be passed through to `grokclassfn'.
   QUALS are qualifiers indicating whether the function is `const'
   or `volatile'.
   RAISES is a list of exceptions that this function can raise.
   CHECK is 1 if we must find this method in CTYPE, 0 if we should
   not look, and -1 if we should not call `grokclassfn' at all.

   Returns `NULL_TREE' if something goes wrong, after issuing
   applicable error messages.  */

static tree
grokfndecl (tree ctype, 
            tree type,
            tree declarator,
	    tree parms,
            tree orig_declarator,
            int virtualp,
            enum overload_flags flags,
            tree quals, 
            tree raises,
            int check, 
            int friendp, 
            int publicp, 
            int inlinep, 
            int funcdef_flag, 
            int template_count,
            tree in_namespace)
{
  tree decl;
  int staticp = ctype && TREE_CODE (type) == FUNCTION_TYPE;
  int has_default_arg = 0;
  tree t;

  if (raises)
    type = build_exception_variant (type, raises);

  decl = build_lang_decl (FUNCTION_DECL, declarator, type);
  DECL_ARGUMENTS (decl) = parms;
  /* Propagate volatile out from type to decl.  */
  if (TYPE_VOLATILE (type))
    TREE_THIS_VOLATILE (decl) = 1;

  /* If this decl has namespace scope, set that up.  */
  if (in_namespace)
    set_decl_namespace (decl, in_namespace, friendp);
  else if (!ctype)
    DECL_CONTEXT (decl) = FROB_CONTEXT (current_namespace);

  /* `main' and builtins have implicit 'C' linkage.  */
  if ((MAIN_NAME_P (declarator)
       || (IDENTIFIER_LENGTH (declarator) > 10
	   && IDENTIFIER_POINTER (declarator)[0] == '_'
	   && IDENTIFIER_POINTER (declarator)[1] == '_'
	   && strncmp (IDENTIFIER_POINTER (declarator)+2, "builtin_", 8) == 0))
      && current_lang_name == lang_name_cplusplus
      && ctype == NULL_TREE
      /* NULL_TREE means global namespace.  */
      && DECL_CONTEXT (decl) == NULL_TREE)
    SET_DECL_LANGUAGE (decl, lang_c);

  /* Should probably propagate const out from type to decl I bet (mrs).  */
  if (staticp)
    {
      DECL_STATIC_FUNCTION_P (decl) = 1;
      DECL_CONTEXT (decl) = ctype;
    }

  if (ctype)
    DECL_CONTEXT (decl) = ctype;

  if (ctype == NULL_TREE && DECL_MAIN_P (decl))
    {
      if (processing_template_decl)
	error ("cannot declare `::main' to be a template");
      if (inlinep)
	error ("cannot declare `::main' to be inline");
      if (!publicp)
	error ("cannot declare `::main' to be static");
      if (!same_type_p (TREE_TYPE (TREE_TYPE (decl)),
			integer_type_node))
	error ("`main' must return `int'");
      inlinep = 0;
      publicp = 1;
    }

  /* Members of anonymous types and local classes have no linkage; make
     them internal.  */
  /* FIXME what if it gets a name from typedef?  */
  if (ctype && (TYPE_ANONYMOUS_P (ctype)
		|| decl_function_context (TYPE_MAIN_DECL (ctype))))
    publicp = 0;

  if (publicp)
    {
      /* [basic.link]: A name with no linkage (notably, the name of a class
	 or enumeration declared in a local scope) shall not be used to
	 declare an entity with linkage.

	 Only check this for public decls for now.  See core 319, 389.  */
      t = no_linkage_check (TREE_TYPE (decl));
      if (t)
	{
	  if (TYPE_ANONYMOUS_P (t))
	    {
	      if (DECL_EXTERN_C_P (decl))
		/* Allow this; it's pretty common in C.  */;
	      else
		{
		  pedwarn ("non-local function `%#D' uses anonymous type",
			      decl);
		  if (DECL_ORIGINAL_TYPE (TYPE_NAME (t)))
		    cp_pedwarn_at ("\
`%#D' does not refer to the unqualified type, so it is not used for linkage",
				TYPE_NAME (t));
		}
	    }
	  else
	    pedwarn ("non-local function `%#D' uses local type `%T'",
			decl, t);
	}
    }

  TREE_PUBLIC (decl) = publicp;
  if (! publicp)
    {
      DECL_INTERFACE_KNOWN (decl) = 1;
      DECL_NOT_REALLY_EXTERN (decl) = 1;
    }

  /* If the declaration was declared inline, mark it as such.  */
  if (inlinep)
    DECL_DECLARED_INLINE_P (decl) = 1;
  /* We inline functions that are explicitly declared inline, or, when
     the user explicitly asks us to, all functions.  */
  if (DECL_DECLARED_INLINE_P (decl)
      || (flag_inline_trees == 2 && !DECL_INLINE (decl) && funcdef_flag))
    DECL_INLINE (decl) = 1;

  DECL_EXTERNAL (decl) = 1;
  if (quals != NULL_TREE && TREE_CODE (type) == FUNCTION_TYPE)
    {
      error ("%smember function `%D' cannot have `%T' method qualifier",
		(ctype ? "static " : "non-"), decl, TREE_VALUE (quals));
      quals = NULL_TREE;
    }

  if (IDENTIFIER_OPNAME_P (DECL_NAME (decl)))
    grok_op_properties (decl, friendp, /*complain=*/true);

  if (ctype && decl_function_context (decl))
    DECL_NO_STATIC_CHAIN (decl) = 1;

  for (t = TYPE_ARG_TYPES (TREE_TYPE (decl)); t; t = TREE_CHAIN (t))
    if (TREE_PURPOSE (t)
	&& TREE_CODE (TREE_PURPOSE (t)) == DEFAULT_ARG)
      {
	has_default_arg = 1;
	break;
      }

  if (friendp
      && TREE_CODE (orig_declarator) == TEMPLATE_ID_EXPR)
    {
      if (funcdef_flag)
	error
	  ("defining explicit specialization `%D' in friend declaration",
	   orig_declarator);
      else
	{
	  tree fns = TREE_OPERAND (orig_declarator, 0);
	  tree args = TREE_OPERAND (orig_declarator, 1);

	  if (PROCESSING_REAL_TEMPLATE_DECL_P ())
	    {
	      /* Something like `template <class T> friend void f<T>()'.  */
	      error ("invalid use of template-id `%D' in declaration of primary template",
			orig_declarator);
	      return NULL_TREE;
	    }


	  /* A friend declaration of the form friend void f<>().  Record
	     the information in the TEMPLATE_ID_EXPR.  */
	  SET_DECL_IMPLICIT_INSTANTIATION (decl);

          if (TREE_CODE (fns) == COMPONENT_REF)
            {
              /* Due to bison parser ickiness, we will have already looked
                 up an operator_name or PFUNCNAME within the current class
                 (see template_id in parse.y). If the current class contains
                 such a name, we'll get a COMPONENT_REF here. Undo that.  */

              my_friendly_assert (TREE_TYPE (TREE_OPERAND (fns, 0))
                                  == current_class_type, 20001120);
              fns = TREE_OPERAND (fns, 1);
            }
	  my_friendly_assert (TREE_CODE (fns) == IDENTIFIER_NODE
	                      || TREE_CODE (fns) == OVERLOAD, 20001120);
	  DECL_TEMPLATE_INFO (decl) = tree_cons (fns, args, NULL_TREE);

	  if (has_default_arg)
	    {
	      error ("default arguments are not allowed in declaration of friend template specialization `%D'",
			decl);
	      return NULL_TREE;
	    }

	  if (inlinep)
	    {
	      error ("`inline' is not allowed in declaration of friend template specialization `%D'",
			decl);
	      return NULL_TREE;
	    }
	}
    }

  if (funcdef_flag)
    /* Make the init_value nonzero so pushdecl knows this is not
       tentative.  error_mark_node is replaced later with the BLOCK.  */
    DECL_INITIAL (decl) = error_mark_node;

  if (TYPE_NOTHROW_P (type) || nothrow_libfn_p (decl))
    TREE_NOTHROW (decl) = 1;

  /* Caller will do the rest of this.  */
  if (check < 0)
    return decl;

  if (flags == NO_SPECIAL && ctype && constructor_name_p (declarator, ctype))
    DECL_CONSTRUCTOR_P (decl) = 1;

  /* Function gets the ugly name, field gets the nice one.  This call
     may change the type of the function (because of default
     parameters)!  */
  if (ctype != NULL_TREE)
    grokclassfn (ctype, decl, flags, quals);

  decl = check_explicit_specialization (orig_declarator, decl,
					template_count,
					2 * (funcdef_flag != 0) +
					4 * (friendp != 0));
  if (decl == error_mark_node)
    return NULL_TREE;

  if (ctype != NULL_TREE
      && (! TYPE_FOR_JAVA (ctype) || check_java_method (decl))
      && check)
    {
      tree old_decl;

      old_decl = check_classfn (ctype, decl,
				processing_template_decl
				> template_class_depth (ctype));

      if (old_decl && TREE_CODE (old_decl) == TEMPLATE_DECL)
	/* Because grokfndecl is always supposed to return a
	   FUNCTION_DECL, we pull out the DECL_TEMPLATE_RESULT
	   here.  We depend on our callers to figure out that its
	   really a template that's being returned.  */
	old_decl = DECL_TEMPLATE_RESULT (old_decl);

      if (old_decl && DECL_STATIC_FUNCTION_P (old_decl)
	  && TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	/* Remove the `this' parm added by grokclassfn.
	   XXX Isn't this done in start_function, too?  */
	revert_static_member_fn (decl);
      if (old_decl && DECL_ARTIFICIAL (old_decl))
	error ("definition of implicitly-declared `%D'", old_decl);

      if (old_decl)
	{
	  tree ok;
	  bool pop_p;

	  /* Since we've smashed OLD_DECL to its
	     DECL_TEMPLATE_RESULT, we must do the same to DECL.  */
	  if (TREE_CODE (decl) == TEMPLATE_DECL)
	    decl = DECL_TEMPLATE_RESULT (decl);

	  /* Attempt to merge the declarations.  This can fail, in
	     the case of some invalid specialization declarations.  */
	  pop_p = push_scope (ctype);
	  ok = duplicate_decls (decl, old_decl);
	  if (pop_p)
	    pop_scope (ctype);
	  if (!ok)
	    {
	      error ("no `%#D' member function declared in class `%T'",
		     decl, ctype);
	      return NULL_TREE;
	    }
	  return old_decl;
	}
    }

  if (DECL_CONSTRUCTOR_P (decl) && !grok_ctor_properties (ctype, decl))
    return NULL_TREE;

  if (ctype == NULL_TREE || check)
    return decl;

  if (virtualp)
    DECL_VIRTUAL_P (decl) = 1;

  return decl;
}

/* Create a VAR_DECL named NAME with the indicated TYPE.  

   If SCOPE is non-NULL, it is the class type or namespace containing
   the variable.  If SCOPE is NULL, the variable should is created in
   the innermost enclosings scope.  */

static tree
grokvardecl (tree type,
             tree name,
             RID_BIT_TYPE * specbits_in,
             int initialized,
             int constp,
             tree scope)
{
  tree decl;
  RID_BIT_TYPE specbits;

  my_friendly_assert (!name || TREE_CODE (name) == IDENTIFIER_NODE, 
		      20020808);

  specbits = *specbits_in;

  /* Compute the scope in which to place the variable.  */
  if (!scope)
    {
      /* An explicit "extern" specifier indicates a namespace-scope
	 variable.  */
      if (RIDBIT_SETP (RID_EXTERN, specbits))
	scope = current_namespace;
      else if (!at_function_scope_p ())
	{
	  scope = current_scope ();
	  if (!scope)
	    scope = current_namespace;
	}
    }

  if (scope
      && (/* If the variable is a namespace-scope variable declared in a
	     template, we need DECL_LANG_SPECIFIC.  */
	  (TREE_CODE (scope) == NAMESPACE_DECL && processing_template_decl)
	  /* Similarly for namespace-scope variables with language linkage
	     other than C++.  */
	  || (TREE_CODE (scope) == NAMESPACE_DECL 
	      && current_lang_name != lang_name_cplusplus)
	  /* Similarly for static data members.  */
	  || TYPE_P (scope)))
    decl = build_lang_decl (VAR_DECL, name, type);
  else
    decl = build_decl (VAR_DECL, name, type);

  if (scope && TREE_CODE (scope) == NAMESPACE_DECL)
    set_decl_namespace (decl, scope, 0);
  else
    DECL_CONTEXT (decl) = scope;

  if (name && scope && current_lang_name != lang_name_c)
    /* We can't mangle lazily here because we don't have any
       way to recover whether or not a variable was `extern
       "C"' later.  */
    mangle_decl (decl);

  if (RIDBIT_SETP (RID_EXTERN, specbits))
    {
      DECL_THIS_EXTERN (decl) = 1;
      DECL_EXTERNAL (decl) = !initialized;
    }

  /* In class context, static means one per class,
     public access, and static storage.  */
  if (DECL_CLASS_SCOPE_P (decl))
    {
      TREE_PUBLIC (decl) = 1;
      TREE_STATIC (decl) = 1;
      DECL_EXTERNAL (decl) = 0;
    }
  /* At top level, either `static' or no s.c. makes a definition
     (perhaps tentative), and absence of `static' makes it public.  */
  else if (toplevel_bindings_p ())
    {
      TREE_PUBLIC (decl) = (RIDBIT_NOTSETP (RID_STATIC, specbits)
			    && (DECL_THIS_EXTERN (decl) || ! constp));
      TREE_STATIC (decl) = ! DECL_EXTERNAL (decl);
    }
  /* Not at top level, only `static' makes a static definition.  */
  else
    {
      TREE_STATIC (decl) = !! RIDBIT_SETP (RID_STATIC, specbits);
      TREE_PUBLIC (decl) = DECL_EXTERNAL (decl);
    }

  if (RIDBIT_SETP (RID_THREAD, specbits))
    {
      if (targetm.have_tls)
	DECL_THREAD_LOCAL (decl) = 1;
      else
	/* A mere warning is sure to result in improper semantics
	   at runtime.  Don't bother to allow this to compile.  */
	error ("thread-local storage not supported for this target");
    }

  if (TREE_PUBLIC (decl))
    {
      /* [basic.link]: A name with no linkage (notably, the name of a class
	 or enumeration declared in a local scope) shall not be used to
	 declare an entity with linkage.

	 Only check this for public decls for now.  */
      tree t = no_linkage_check (TREE_TYPE (decl));
      if (t)
	{
	  if (TYPE_ANONYMOUS_P (t))
	    /* Ignore for now; `enum { foo } e' is pretty common.  */;
	  else
	    pedwarn ("non-local variable `%#D' uses local type `%T'",
			decl, t);
	}
    }

  return decl;
}

/* Create and return a canonical pointer to member function type, for
   TYPE, which is a POINTER_TYPE to a METHOD_TYPE.  */

tree
build_ptrmemfunc_type (tree type)
{
  tree field, fields;
  tree t;
  tree unqualified_variant = NULL_TREE;

  if (type == error_mark_node)
    return type;

  /* If a canonical type already exists for this type, use it.  We use
     this method instead of type_hash_canon, because it only does a
     simple equality check on the list of field members.  */

  if ((t = TYPE_GET_PTRMEMFUNC_TYPE (type)))
    return t;

  /* Make sure that we always have the unqualified pointer-to-member
     type first.  */
  if (cp_type_quals (type) != TYPE_UNQUALIFIED)
    unqualified_variant
      = build_ptrmemfunc_type (TYPE_MAIN_VARIANT (type));

  t = make_aggr_type (RECORD_TYPE);
  /* Let the front-end know this is a pointer to member function...  */
  TYPE_PTRMEMFUNC_FLAG (t) = 1;
  /* ... and not really an aggregate.  */
  SET_IS_AGGR_TYPE (t, 0);

  field = build_decl (FIELD_DECL, pfn_identifier, type);
  fields = field;
  
  field = build_decl (FIELD_DECL, delta_identifier, delta_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;
  
  finish_builtin_struct (t, "__ptrmemfunc_type", fields, ptr_type_node);

  /* Zap out the name so that the back-end will give us the debugging
     information for this anonymous RECORD_TYPE.  */
  TYPE_NAME (t) = NULL_TREE;

  /* If this is not the unqualified form of this pointer-to-member
     type, set the TYPE_MAIN_VARIANT for this type to be the
     unqualified type.  Since they are actually RECORD_TYPEs that are
     not variants of each other, we must do this manually.  */
  if (cp_type_quals (type) != TYPE_UNQUALIFIED)
    {
      t = build_qualified_type (t, cp_type_quals (type));
      TYPE_MAIN_VARIANT (t) = unqualified_variant;
      TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (unqualified_variant);
      TYPE_NEXT_VARIANT (unqualified_variant) = t;
    }

  /* Cache this pointer-to-member type so that we can find it again
     later.  */
  TYPE_SET_PTRMEMFUNC_TYPE (type, t);

  return t;
}

/* Create and return a pointer to data member type.  */

tree
build_ptrmem_type (tree class_type, tree member_type)
{
  if (TREE_CODE (member_type) == METHOD_TYPE)
    {
      tree arg_types;

      arg_types = TYPE_ARG_TYPES (member_type);
      class_type = (cp_build_qualified_type 
		    (class_type,
		     cp_type_quals (TREE_TYPE (TREE_VALUE (arg_types)))));
      member_type 
	= build_method_type_directly (class_type, 
				      TREE_TYPE (member_type),
				      TREE_CHAIN (arg_types));
      return build_ptrmemfunc_type (build_pointer_type (member_type));
    }
  else
    {
      my_friendly_assert (TREE_CODE (member_type) != FUNCTION_TYPE,
			  20030716);
      return build_offset_type (class_type, member_type);
    }
}

/* DECL is a VAR_DECL defined in-class, whose TYPE is also given.
   Check to see that the definition is valid.  Issue appropriate error
   messages.  Return 1 if the definition is particularly bad, or 0
   otherwise.  */

int
check_static_variable_definition (tree decl, tree type)
{
  /* Motion 10 at San Diego: If a static const integral data member is
     initialized with an integral constant expression, the initializer
     may appear either in the declaration (within the class), or in
     the definition, but not both.  If it appears in the class, the
     member is a member constant.  The file-scope definition is always
     required.  */
  if (!ARITHMETIC_TYPE_P (type) && TREE_CODE (type) != ENUMERAL_TYPE)
    {
      error ("invalid in-class initialization of static data member of non-integral type `%T'",
	     type);
      /* If we just return the declaration, crashes will sometimes
	 occur.  We therefore return void_type_node, as if this was a
	 friend declaration, to cause callers to completely ignore
	 this declaration.  */
      return 1;
    }
  else if (!CP_TYPE_CONST_P (type))
    error ("ISO C++ forbids in-class initialization of non-const static member `%D'",
	      decl);
  else if (pedantic && !INTEGRAL_TYPE_P (type))
    pedwarn ("ISO C++ forbids initialization of member constant `%D' of non-integral type `%T'", decl, type);

  return 0;
}

/* Given the SIZE (i.e., number of elements) in an array, compute an
   appropriate index type for the array.  If non-NULL, NAME is the
   name of the thing being declared.  */

tree
compute_array_index_type (tree name, tree size)
{
  tree type = TREE_TYPE (size);
  tree itype;

  /* The array bound must be an integer type.  */
  if (!dependent_type_p (type) && !INTEGRAL_TYPE_P (type))
    {
      if (name)
	error ("size of array `%D' has non-integral type `%T'", name, type);
      else
	error ("size of array has non-integral type `%T'", type);
      size = integer_one_node;
      type = TREE_TYPE (size);
    }

  if (abi_version_at_least (2)
      /* We should only handle value dependent expressions specially.  */
      ? value_dependent_expression_p (size)
      /* But for abi-1, we handled all instances in templates. This
	 effects the manglings produced.  */
      : processing_template_decl)
    return build_index_type (build_min (MINUS_EXPR, sizetype,
					size, integer_one_node));

  /* The size might be the result of a cast.  */
  STRIP_TYPE_NOPS (size);

  /* It might be a const variable or enumeration constant.  */
  size = decl_constant_value (size);

  /* Normally, the array-bound will be a constant.  */
  if (TREE_CODE (size) == INTEGER_CST)
    {
      /* Check to see if the array bound overflowed.  Make that an
	 error, no matter how generous we're being.  */
      int old_flag_pedantic_errors = flag_pedantic_errors;
      int old_pedantic = pedantic;
      pedantic = flag_pedantic_errors = 1;
      constant_expression_warning (size);
      pedantic = old_pedantic;
      flag_pedantic_errors = old_flag_pedantic_errors;

      /* An array must have a positive number of elements.  */
      if (INT_CST_LT (size, integer_zero_node))
	{
	  if (name)
	    error ("size of array `%D' is negative", name);
	  else
	    error ("size of array is negative");
	  size = integer_one_node;
	}
      /* As an extension we allow zero-sized arrays.  We always allow
	 them in system headers because glibc uses them.  */
      else if (integer_zerop (size) && pedantic && !in_system_header)
	{
	  if (name)
	    pedwarn ("ISO C++ forbids zero-size array `%D'", name);
	  else
	    pedwarn ("ISO C++ forbids zero-size array");
	}
    }
  else if (TREE_CONSTANT (size))
    {
      /* `(int) &fn' is not a valid array bound.  */
      if (name)
	error ("size of array `%D' is not an integral constant-expression",
		  name);
      else
	error ("size of array is not an integral constant-expression");
    }
  else if (pedantic)
    {
      if (name)
	pedwarn ("ISO C++ forbids variable-size array `%D'", name);
      else
	pedwarn ("ISO C++ forbids variable-size array");
    }

  if (processing_template_decl && !TREE_CONSTANT (size))
    /* A variable sized array.  */
    itype = build_min (MINUS_EXPR, sizetype, size, integer_one_node);
  else
    {
      /* Compute the index of the largest element in the array.  It is
     	 one less than the number of elements in the array.  */
      itype
	= fold (cp_build_binary_op (MINUS_EXPR,
				    cp_convert (ssizetype, size),
				    cp_convert (ssizetype, integer_one_node)));
      if (!TREE_CONSTANT (itype))
	/* A variable sized array.  */
	itype = variable_size (itype);
      /* Make sure that there was no overflow when creating to a signed
     	 index type.  (For example, on a 32-bit machine, an array with
     	 size 2^32 - 1 is too big.)  */
      else if (TREE_OVERFLOW (itype))
	{
	  error ("overflow in array dimension");
	  TREE_OVERFLOW (itype) = 0;
	}
    }

  /* Create and return the appropriate index type.  */
  return build_index_type (itype);
}

/* Returns the scope (if any) in which the entity declared by
   DECLARATOR will be located.  If the entity was declared with an
   unqualified name, NULL_TREE is returned.  */

tree
get_scope_of_declarator (tree declarator)
{
  if (!declarator)
    return NULL_TREE;
  
  switch (TREE_CODE (declarator))
    {
    case CALL_EXPR:
    case ARRAY_REF:
    case INDIRECT_REF:
    case ADDR_EXPR:
      /* For any of these, the main declarator is the first operand.  */
      return get_scope_of_declarator (TREE_OPERAND
				      (declarator, 0));

    case SCOPE_REF:
      /* For a pointer-to-member, continue descending.  */
      if (TREE_CODE (TREE_OPERAND (declarator, 1))
	  == INDIRECT_REF)
	return get_scope_of_declarator (TREE_OPERAND
					(declarator, 1));
      /* Otherwise, if the declarator-id is a SCOPE_REF, the scope in
	 which the declaration occurs is the first operand.  */
      return TREE_OPERAND (declarator, 0);

    case TREE_LIST:
      /* Attributes to be applied. The declarator is TREE_VALUE.  */
      return get_scope_of_declarator (TREE_VALUE (declarator));
      
    default:
      /* Otherwise, we have a declarator-id which is not a qualified
	 name; the entity will be declared in the current scope.  */
      return NULL_TREE;
    }
}

/* Returns an ARRAY_TYPE for an array with SIZE elements of the
   indicated TYPE.  If non-NULL, NAME is the NAME of the declaration
   with this type.  */

static tree
create_array_type_for_decl (tree name, tree type, tree size)
{
  tree itype = NULL_TREE;
  const char* error_msg;

  /* If things have already gone awry, bail now.  */
  if (type == error_mark_node || size == error_mark_node)
    return error_mark_node;

  /* Assume that everything will go OK.  */
  error_msg = NULL;

  /* There are some types which cannot be array elements.  */
  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
      error_msg = "array of void";
      break;

    case FUNCTION_TYPE:
      error_msg = "array of functions";
      break;

    case REFERENCE_TYPE:
      error_msg = "array of references";
      break;

    case METHOD_TYPE:
      error_msg = "array of function members";
      break;

    default:
      break;
    }

  /* If something went wrong, issue an error-message and return.  */
  if (error_msg)
    {
      if (name)
	error ("declaration of `%D' as %s", name, error_msg);
      else
	error ("creating %s", error_msg);

      return error_mark_node;
    }

  /* [dcl.array]

     The constant expressions that specify the bounds of the arrays
     can be omitted only for the first member of the sequence.  */
  if (TREE_CODE (type) == ARRAY_TYPE && !TYPE_DOMAIN (type))
    {
      if (name)
	error ("declaration of `%D' as multidimensional array must have bounds for all dimensions except the first",
		  name);
      else
	error ("multidimensional array must have bounds for all dimensions except the first");

      return error_mark_node;
    }

  /* Figure out the index type for the array.  */
  if (size)
    itype = compute_array_index_type (name, size);

  return build_cplus_array_type (type, itype);
}

/* Check that it's OK to declare a function with the indicated TYPE.
   SFK indicates the kind of special function (if any) that this
   function is.  OPTYPE is the type given in a conversion operator
   declaration.  Returns the actual return type of the function; that
   may be different than TYPE if an error occurs, or for certain
   special functions.  */

static tree
check_special_function_return_type (special_function_kind sfk,
                                    tree type,
                                    tree optype)
{
  switch (sfk)
    {
    case sfk_constructor:
      if (type)
	error ("return type specification for constructor invalid");

      type = void_type_node;
      break;

    case sfk_destructor:
      if (type)
	error ("return type specification for destructor invalid");
      type = void_type_node;
      break;

    case sfk_conversion:
      if (type && !same_type_p (type, optype))
	error ("operator `%T' declared to return `%T'", optype, type);
      else if (type)
	pedwarn ("return type specified for `operator %T'",  optype);
      type = optype;
      break;

    default:
      abort ();
      break;
    }

  return type;
}

/* A variable or data member (whose unqualified name is IDENTIFIER)
   has been declared with the indicated TYPE.  If the TYPE is not
   acceptable, issue an error message and return a type to use for
   error-recovery purposes. */

tree
check_var_type (tree identifier, tree type)
{
  if (VOID_TYPE_P (type))
    {
      if (!identifier)
	error ("unnamed variable or field declared void");
      else if (TREE_CODE (identifier) == IDENTIFIER_NODE)
	{
	  if (IDENTIFIER_OPNAME_P (identifier))
	    abort ();
	  error ("variable or field `%E' declared void", identifier);
	}
      else
	error ("variable or field declared void");
      type = integer_type_node;
    }
  
  return type;
}

/* Given declspecs and a declarator (abstract or otherwise), determine
   the name and type of the object declared and construct a DECL node
   for it.

   DECLSPECS is a chain of tree_list nodes whose value fields
    are the storage classes and type specifiers.

   DECL_CONTEXT says which syntactic context this declaration is in:
     NORMAL for most contexts.  Make a VAR_DECL or FUNCTION_DECL or TYPE_DECL.
     FUNCDEF for a function definition.  Like NORMAL but a few different
      error messages in each case.  Return value may be zero meaning
      this definition is too screwy to try to parse.
     MEMFUNCDEF for a function definition.  Like FUNCDEF but prepares to
      handle member functions (which have FIELD context).
      Return value may be zero meaning this definition is too screwy to
      try to parse.
     PARM for a parameter declaration (either within a function prototype
      or before a function body).  Make a PARM_DECL, or return void_type_node.
     CATCHPARM for a parameter declaration before a catch clause.
     TYPENAME if for a typename (in a cast or sizeof).
      Don't make a DECL node; just return the ..._TYPE node.
     FIELD for a struct or union field; make a FIELD_DECL.
     BITFIELD for a field with specified width.
   INITIALIZED is 1 if the decl has an initializer.

   ATTRLIST is a pointer to the list of attributes, which may be NULL
   if there are none; *ATTRLIST may be modified if attributes from inside
   the declarator should be applied to the declaration.

   When this function is called, scoping variables (such as
   CURRENT_CLASS_TYPE) should reflect the scope in which the
   declaration occurs, not the scope in which the new declaration will
   be placed.  For example, on:

     void S::f() { ... }

   when grokdeclarator is called for `S::f', the CURRENT_CLASS_TYPE
   should not be `S'.  */

tree
grokdeclarator (tree declarator,
                tree declspecs,
                enum decl_context decl_context,
                int initialized,
                tree* attrlist)
{
  RID_BIT_TYPE specbits;
  int nclasses = 0;
  tree spec;
  tree type = NULL_TREE;
  int longlong = 0;
  int type_quals;
  int virtualp, explicitp, friendp, inlinep, staticp;
  int explicit_int = 0;
  int explicit_char = 0;
  int defaulted_int = 0;
  int extern_langp = 0;
  tree dependant_name = NULL_TREE;
  
  tree typedef_decl = NULL_TREE;
  const char *name;
  tree typedef_type = NULL_TREE;
  int funcdef_flag = 0;
  enum tree_code innermost_code = ERROR_MARK;
  int bitfield = 0;
#if 0
  /* See the code below that used this.  */
  tree decl_attr = NULL_TREE;
#endif

  /* Keep track of what sort of function is being processed
     so that we can warn about default return values, or explicit
     return values which do not match prescribed defaults.  */
  special_function_kind sfk = sfk_none;

  tree dname = NULL_TREE;
  tree ctype = current_class_type;
  tree ctor_return_type = NULL_TREE;
  enum overload_flags flags = NO_SPECIAL;
  tree quals = NULL_TREE;
  tree raises = NULL_TREE;
  int template_count = 0;
  tree in_namespace = NULL_TREE;
  tree returned_attrs = NULL_TREE;
  tree scope = NULL_TREE;
  tree parms = NULL_TREE;

  RIDBIT_RESET_ALL (specbits);
  if (decl_context == FUNCDEF)
    funcdef_flag = 1, decl_context = NORMAL;
  else if (decl_context == MEMFUNCDEF)
    funcdef_flag = -1, decl_context = FIELD;
  else if (decl_context == BITFIELD)
    bitfield = 1, decl_context = FIELD;

  /* Look inside a declarator for the name being declared
     and get it as a string, for an error message.  */
  {
    tree *next = &declarator;
    tree decl;
    name = NULL;

    while (next && *next)
      {
	decl = *next;
	switch (TREE_CODE (decl))
	  {
	  case TREE_LIST:
	    /* For attributes.  */
	    next = &TREE_VALUE (decl);
	    break;

	  case COND_EXPR:
	    ctype = NULL_TREE;
	    next = &TREE_OPERAND (decl, 0);
	    break;

	  case BIT_NOT_EXPR:	/* For C++ destructors!  */
	    {
	      tree name = TREE_OPERAND (decl, 0);
	      tree rename = NULL_TREE;

	      my_friendly_assert (flags == NO_SPECIAL, 152);
	      flags = DTOR_FLAG;
	      sfk = sfk_destructor;
	      if (TYPE_P (name))
		TREE_OPERAND (decl, 0) = name = constructor_name (name);
	      my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 153);
	      if (ctype == NULL_TREE)
		{
		  if (current_class_type == NULL_TREE)
		    {
		      error ("destructors must be member functions");
		      flags = NO_SPECIAL;
		    }
		  else
		    {
		      tree t = constructor_name (current_class_type);
		      if (t != name)
			rename = t;
		    }
		}
	      else
		{
		  tree t = constructor_name (ctype);
		  if (t != name)
		    rename = t;
		}

	      if (rename)
		{
		  error ("destructor `%T' must match class name `%T'",
			    name, rename);
		  TREE_OPERAND (decl, 0) = rename;
		}
	      next = &name;
	    }
	    break;

	  case ADDR_EXPR:	/* C++ reference declaration */
	    /* Fall through.  */
	  case ARRAY_REF:
	  case INDIRECT_REF:
	    ctype = NULL_TREE;
	    innermost_code = TREE_CODE (decl);
	    next = &TREE_OPERAND (decl, 0);
	    break;

	  case CALL_EXPR:
	    innermost_code = TREE_CODE (decl);
	    if (decl_context == FIELD && ctype == NULL_TREE)
	      ctype = current_class_type;
	    if (ctype
		&& TREE_OPERAND (decl, 0)
		&& (TREE_CODE (TREE_OPERAND (decl, 0)) == TYPE_DECL
		    && constructor_name_p (DECL_NAME (TREE_OPERAND (decl, 0)),
					   ctype)))
	      TREE_OPERAND (decl, 0) = constructor_name (ctype);
	    next = &TREE_OPERAND (decl, 0);
	    decl = *next;
	    if (ctype != NULL_TREE
		&& decl != NULL_TREE && flags != DTOR_FLAG
		&& constructor_name_p (decl, ctype))
	      {
		sfk = sfk_constructor;
		ctor_return_type = ctype;
	      }
	    ctype = NULL_TREE;
	    break;

	  case TEMPLATE_ID_EXPR:
	      {
		tree fns = TREE_OPERAND (decl, 0);

		dname = fns;
		if (TREE_CODE (dname) == COMPONENT_REF)
		  dname = TREE_OPERAND (dname, 1);
		if (TREE_CODE (dname) != IDENTIFIER_NODE)
		  {
		    my_friendly_assert (is_overloaded_fn (dname),
					19990331);
		    dname = DECL_NAME (get_first_fn (dname));
		  }
	      }
	  /* Fall through.  */

	  case IDENTIFIER_NODE:
	    if (TREE_CODE (decl) == IDENTIFIER_NODE)
	      dname = decl;

	    next = 0;

	    if (C_IS_RESERVED_WORD (dname))
	      {
		error ("declarator-id missing; using reserved word `%D'",
			  dname);
		name = IDENTIFIER_POINTER (dname);
	      }
	    else if (!IDENTIFIER_TYPENAME_P (dname))
	      name = IDENTIFIER_POINTER (dname);
	    else
	      {
		my_friendly_assert (flags == NO_SPECIAL, 154);
		flags = TYPENAME_FLAG;
		ctor_return_type = TREE_TYPE (dname);
		sfk = sfk_conversion;
		if (is_typename_at_global_scope (dname))
		  name = IDENTIFIER_POINTER (dname);
		else
		  name = "<invalid operator>";
	      }
	    break;

	    /* C++ extension */
	  case SCOPE_REF:
	    {
	      /* Perform error checking, and decide on a ctype.  */
	      tree cname = TREE_OPERAND (decl, 0);
	      if (cname == NULL_TREE)
		ctype = NULL_TREE;
	      else if (TREE_CODE (cname) == NAMESPACE_DECL)
		{
		  ctype = NULL_TREE;
		  in_namespace = TREE_OPERAND (decl, 0);
		}
	      else if (! is_aggr_type (cname, 1))
		ctype = NULL_TREE;
	      /* Must test TREE_OPERAND (decl, 1), in case user gives
		 us `typedef (class::memfunc)(int); memfunc *memfuncptr;'  */
	      else if (TREE_OPERAND (decl, 1)
		       && TREE_CODE (TREE_OPERAND (decl, 1)) == INDIRECT_REF)
		ctype = cname;
	      else if (TREE_CODE (cname) == TEMPLATE_TYPE_PARM
		       || TREE_CODE (cname) == BOUND_TEMPLATE_TEMPLATE_PARM)
		{
	  	  /* This might be declaring a member of a template
		     parm to be a friend.  */
		  ctype = cname;
		  dependant_name = TREE_OPERAND (decl, 1);
		}
	      else if (ctype == NULL_TREE)
		ctype = cname;
	      else if (TREE_COMPLEXITY (decl) == current_class_depth)
		;
	      else
		{
		  if (! UNIQUELY_DERIVED_FROM_P (cname, ctype))
		    {
		      error ("type `%T' is not derived from type `%T'",
				cname, ctype);
		      ctype = NULL_TREE;
		    }
		  else
		    ctype = cname;
		}

	      /* It is valid to write:

		   class C { void f(); };
		   typedef C D;
		   void D::f();

		 The standard is not clear about whether `typedef const C D' is
		 legal; as of 2002-09-15 the committee is considering
		 that question.  EDG 3.0 allows that syntax.
		 Therefore, we do as well.  */
	      if (ctype)
		ctype = TYPE_MAIN_VARIANT (ctype);
	      /* Update the declarator so that when we process it
		 again the correct type is present.  */
	      TREE_OPERAND (decl, 0) = ctype;

	      if (ctype && TREE_CODE (TREE_OPERAND (decl, 1)) == TYPE_DECL
		  && constructor_name_p (DECL_NAME (TREE_OPERAND (decl, 1)),
					 ctype))
		TREE_OPERAND (decl, 1) = constructor_name (ctype);
	      next = &TREE_OPERAND (decl, 1);
	      decl = *next;
	      if (ctype)
		{
		  tree name = decl;

		  if (TREE_CODE (name) == BIT_NOT_EXPR)
		    name = TREE_OPERAND (name, 0);

		  if (!constructor_name_p (decl, ctype))
		    ;
		  else if (decl == name)
		    {
		      sfk = sfk_constructor;
		      ctor_return_type = ctype;
		    }
		  else
		    {
		      sfk = sfk_destructor;
		      ctor_return_type = ctype;
		      flags = DTOR_FLAG;
		      TREE_OPERAND (decl, 0) = constructor_name (ctype);
		      next = &TREE_OPERAND (decl, 0);
		    }
		}
	    }
	    break;

	  case ERROR_MARK:
	    next = 0;
	    break;

	  case TYPE_DECL:
	    /* Parse error puts this typespec where
	       a declarator should go.  */
	    error ("`%T' specified as declarator-id", DECL_NAME (decl));
	    if (TREE_TYPE (decl) == current_class_type)
	      error ("  perhaps you want `%T' for a constructor",
			current_class_name);
	    dname = DECL_NAME (decl);
	    name = IDENTIFIER_POINTER (dname);

	    /* Avoid giving two errors for this.  */
	    IDENTIFIER_CLASS_VALUE (dname) = NULL_TREE;

	    declspecs = tree_cons (NULL_TREE, integer_type_node, declspecs);
	    *next = dname;
	    next = 0;
	    break;

	  case BASELINK:
	    next = &BASELINK_FUNCTIONS (decl);
	    break;

	  case TEMPLATE_DECL:
	    /* Sometimes, we see a template-name used as part of a 
	       decl-specifier like in 
	          std::allocator alloc;
	       Handle that gracefully.  */
	    error ("invalid use of template-name '%E' in a declarator", decl);
	    return error_mark_node;
	    break;
	    
	  default:
	    my_friendly_assert (0, 20020917);
	  }
      }
  }

  /* A function definition's declarator must have the form of
     a function declarator.  */

  if (funcdef_flag && innermost_code != CALL_EXPR)
    return 0;

  if (((dname && IDENTIFIER_OPNAME_P (dname)) || flags == TYPENAME_FLAG)
      && innermost_code != CALL_EXPR
      && ! (ctype && declspecs == NULL_TREE))
    {
      error ("declaration of `%D' as non-function", dname);
      return void_type_node;
    }

  /* Anything declared one level down from the top level
     must be one of the parameters of a function
     (because the body is at least two levels down).  */

  /* This heuristic cannot be applied to C++ nodes! Fixed, however,
     by not allowing C++ class definitions to specify their parameters
     with xdecls (must be spec.d in the parmlist).

     Since we now wait to push a class scope until we are sure that
     we are in a legitimate method context, we must set oldcname
     explicitly (since current_class_name is not yet alive).

     We also want to avoid calling this a PARM if it is in a namespace.  */

  if (decl_context == NORMAL && !toplevel_bindings_p ())
    {
      struct cp_binding_level *b = current_binding_level;
      current_binding_level = b->level_chain;
      if (current_binding_level != 0 && toplevel_bindings_p ())
	decl_context = PARM;
      current_binding_level = b;
    }

  if (name == NULL)
    name = decl_context == PARM ? "parameter" : "type name";

  /* Look through the decl specs and record which ones appear.
     Some typespecs are defined as built-in typenames.
     Others, the ones that are modifiers of other types,
     are represented by bits in SPECBITS: set the bits for
     the modifiers that appear.  Storage class keywords are also in SPECBITS.

     If there is a typedef name or a type, store the type in TYPE.
     This includes builtin typedefs such as `int'.

     Set EXPLICIT_INT if the type is `int' or `char' and did not
     come from a user typedef.

     Set LONGLONG if `long' is mentioned twice.

     For C++, constructors and destructors have their own fast treatment.  */

  for (spec = declspecs; spec; spec = TREE_CHAIN (spec))
    {
      int i;
      tree id;

      /* Certain parse errors slip through.  For example,
	 `int class;' is not caught by the parser. Try
	 weakly to recover here.  */
      if (TREE_CODE (spec) != TREE_LIST)
	return 0;

      id = TREE_VALUE (spec);

      /* If the entire declaration is itself tagged as deprecated then
         suppress reports of deprecated items.  */
      if (!adding_implicit_members && id && TREE_DEPRECATED (id))
        {
	  if (deprecated_state != DEPRECATED_SUPPRESS)
	    warn_deprecated_use (id);
        }

      if (TREE_CODE (id) == IDENTIFIER_NODE)
	{
	  if (id == ridpointers[(int) RID_INT]
	      || id == ridpointers[(int) RID_CHAR]
	      || id == ridpointers[(int) RID_BOOL]
	      || id == ridpointers[(int) RID_WCHAR])
	    {
	      if (type)
		{
		  if (id == ridpointers[(int) RID_BOOL])
		    error ("`bool' is now a keyword");
		  else
		    error ("extraneous `%T' ignored", id);
		}
	      else
		{
		  if (id == ridpointers[(int) RID_INT])
		    explicit_int = 1;
		  else if (id == ridpointers[(int) RID_CHAR])
		    explicit_char = 1;
		  type = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (id));
		}
	      goto found;
	    }
	  /* C++ aggregate types.  */
	  if (IDENTIFIER_HAS_TYPE_VALUE (id))
	    {
	      if (type)
		error ("multiple declarations `%T' and `%T'", type, id);
	      else
		type = IDENTIFIER_TYPE_VALUE (id);
	      goto found;
	    }

	  for (i = (int) RID_FIRST_MODIFIER; i <= (int) RID_LAST_MODIFIER; i++)
	    {
	      if (ridpointers[i] == id)
		{
		  if (i == (int) RID_LONG && RIDBIT_SETP (i, specbits))
		    {
		      if (pedantic && ! in_system_header && warn_long_long)
			pedwarn ("ISO C++ does not support `long long'");
		      if (longlong)
			error ("`long long long' is too long for GCC");
		      else
			longlong = 1;
		    }
		  else if (RIDBIT_SETP (i, specbits))
		    pedwarn ("duplicate `%s'", IDENTIFIER_POINTER (id));

		  /* Diagnose "__thread extern" or "__thread static".  */
		  if (RIDBIT_SETP (RID_THREAD, specbits))
		    {
		      if (i == (int)RID_EXTERN)
			error ("`__thread' before `extern'");
		      else if (i == (int)RID_STATIC)
			error ("`__thread' before `static'");
		    }

		  if (i == (int)RID_EXTERN
		      && TREE_PURPOSE (spec) == error_mark_node)
		    /* This extern was part of a language linkage.  */
		    extern_langp = 1;

		  RIDBIT_SET (i, specbits);
		  goto found;
		}
	    }
	}
      else if (TREE_CODE (id) == TYPE_DECL)
	{
	  if (type)
	    error ("multiple declarations `%T' and `%T'", type,
		      TREE_TYPE (id));
	  else
	    {
	      type = TREE_TYPE (id);
	      TREE_VALUE (spec) = type;
	      typedef_decl = id;
	    }
	  goto found;
	}
      if (type)
	error ("two or more data types in declaration of `%s'", name);
      else if (TREE_CODE (id) == IDENTIFIER_NODE)
	{
	  tree t = lookup_name (id, 1);
	  if (!t || TREE_CODE (t) != TYPE_DECL)
	    error ("`%s' fails to be a typedef or built in type",
		   IDENTIFIER_POINTER (id));
	  else
	    {
	      type = TREE_TYPE (t);
	      typedef_decl = t;
	    }
	}
      else if (id != error_mark_node)
	/* Can't change CLASS nodes into RECORD nodes here!  */
	type = id;

    found: ;
    }

#if 0
  /* See the code below that used this.  */
  if (typedef_decl)
    decl_attr = DECL_ATTRIBUTES (typedef_decl);
#endif
  typedef_type = type;

  /* No type at all: default to `int', and set DEFAULTED_INT
     because it was not a user-defined typedef.  */

  if (type == NULL_TREE
      && (RIDBIT_SETP (RID_SIGNED, specbits)
	  || RIDBIT_SETP (RID_UNSIGNED, specbits)
	  || RIDBIT_SETP (RID_LONG, specbits)
	  || RIDBIT_SETP (RID_SHORT, specbits)))
    {
      /* These imply 'int'.  */
      type = integer_type_node;
      defaulted_int = 1;
    }

  if (sfk != sfk_none)
    type = check_special_function_return_type (sfk, type,
					       ctor_return_type);
  else if (type == NULL_TREE)
    {
      int is_main;

      explicit_int = -1;

      /* We handle `main' specially here, because 'main () { }' is so
	 common.  With no options, it is allowed.  With -Wreturn-type,
	 it is a warning.  It is only an error with -pedantic-errors.  */
      is_main = (funcdef_flag
		 && dname && MAIN_NAME_P (dname)
		 && ctype == NULL_TREE
		 && in_namespace == NULL_TREE
		 && current_namespace == global_namespace);

      if (in_system_header || flag_ms_extensions)
	/* Allow it, sigh.  */;
      else if (pedantic || ! is_main)
	pedwarn ("ISO C++ forbids declaration of `%s' with no type",
		    name);
      else if (warn_return_type)
	warning ("ISO C++ forbids declaration of `%s' with no type",
		    name);

      type = integer_type_node;
    }
  
  ctype = NULL_TREE;

  /* Now process the modifiers that were specified
     and check for invalid combinations.  */

  /* Long double is a special combination.  */

  if (RIDBIT_SETP (RID_LONG, specbits)
      && TYPE_MAIN_VARIANT (type) == double_type_node)
    {
      RIDBIT_RESET (RID_LONG, specbits);
      type = build_qualified_type (long_double_type_node,
				   cp_type_quals (type));
    }

  /* Check all other uses of type modifiers.  */

  if (RIDBIT_SETP (RID_UNSIGNED, specbits)
      || RIDBIT_SETP (RID_SIGNED, specbits)
      || RIDBIT_SETP (RID_LONG, specbits)
      || RIDBIT_SETP (RID_SHORT, specbits))
    {
      int ok = 0;

      if (TREE_CODE (type) == REAL_TYPE)
	error ("short, signed or unsigned invalid for `%s'", name);
      else if (TREE_CODE (type) != INTEGER_TYPE)
	error ("long, short, signed or unsigned invalid for `%s'", name);
      else if (RIDBIT_SETP (RID_LONG, specbits)
	       && RIDBIT_SETP (RID_SHORT, specbits))
	error ("long and short specified together for `%s'", name);
      else if ((RIDBIT_SETP (RID_LONG, specbits)
		|| RIDBIT_SETP (RID_SHORT, specbits))
	       && explicit_char)
	error ("long or short specified with char for `%s'", name);
      else if ((RIDBIT_SETP (RID_LONG, specbits)
		|| RIDBIT_SETP (RID_SHORT, specbits))
	       && TREE_CODE (type) == REAL_TYPE)
	error ("long or short specified with floating type for `%s'", name);
      else if (RIDBIT_SETP (RID_SIGNED, specbits)
	       && RIDBIT_SETP (RID_UNSIGNED, specbits))
	error ("signed and unsigned given together for `%s'", name);
      else
	{
	  ok = 1;
	  if (!explicit_int && !defaulted_int && !explicit_char && pedantic)
	    {
	      pedwarn ("long, short, signed or unsigned used invalidly for `%s'",
		       name);
	      if (flag_pedantic_errors)
		ok = 0;
	    }
	}

      /* Discard the type modifiers if they are invalid.  */
      if (! ok)
	{
	  RIDBIT_RESET (RID_UNSIGNED, specbits);
	  RIDBIT_RESET (RID_SIGNED, specbits);
	  RIDBIT_RESET (RID_LONG, specbits);
	  RIDBIT_RESET (RID_SHORT, specbits);
	  longlong = 0;
	}
    }

  if (RIDBIT_SETP (RID_COMPLEX, specbits)
      && TREE_CODE (type) != INTEGER_TYPE && TREE_CODE (type) != REAL_TYPE)
    {
      error ("complex invalid for `%s'", name);
      RIDBIT_RESET (RID_COMPLEX, specbits);
    }

  /* Decide whether an integer type is signed or not.
     Optionally treat bitfields as signed by default.  */
  if (RIDBIT_SETP (RID_UNSIGNED, specbits)
      /* [class.bit]

	 It is implementation-defined whether a plain (neither
	 explicitly signed or unsigned) char, short, int, or long
	 bit-field is signed or unsigned.

	 Naturally, we extend this to long long as well.  Note that
	 this does not include wchar_t.  */
      || (bitfield && !flag_signed_bitfields
	  && RIDBIT_NOTSETP (RID_SIGNED, specbits)
	  /* A typedef for plain `int' without `signed' can be
	     controlled just like plain `int', but a typedef for
	     `signed int' cannot be so controlled.  */
	  && !(typedef_decl
	       && C_TYPEDEF_EXPLICITLY_SIGNED (typedef_decl))
	  && (TREE_CODE (type) == INTEGER_TYPE
	      || TREE_CODE (type) == CHAR_TYPE)
	  && !same_type_p (TYPE_MAIN_VARIANT (type), wchar_type_node)))
    {
      if (longlong)
	type = long_long_unsigned_type_node;
      else if (RIDBIT_SETP (RID_LONG, specbits))
	type = long_unsigned_type_node;
      else if (RIDBIT_SETP (RID_SHORT, specbits))
	type = short_unsigned_type_node;
      else if (type == char_type_node)
	type = unsigned_char_type_node;
      else if (typedef_decl)
	type = c_common_unsigned_type (type);
      else
	type = unsigned_type_node;
    }
  else if (RIDBIT_SETP (RID_SIGNED, specbits)
	   && type == char_type_node)
    type = signed_char_type_node;
  else if (longlong)
    type = long_long_integer_type_node;
  else if (RIDBIT_SETP (RID_LONG, specbits))
    type = long_integer_type_node;
  else if (RIDBIT_SETP (RID_SHORT, specbits))
    type = short_integer_type_node;

  if (RIDBIT_SETP (RID_COMPLEX, specbits))
    {
      /* If we just have "complex", it is equivalent to
	 "complex double", but if any modifiers at all are specified it is
	 the complex form of TYPE.  E.g, "complex short" is
	 "complex short int".  */

      if (defaulted_int && ! longlong
	  && ! (RIDBIT_SETP (RID_LONG, specbits)
		|| RIDBIT_SETP (RID_SHORT, specbits)
		|| RIDBIT_SETP (RID_SIGNED, specbits)
		|| RIDBIT_SETP (RID_UNSIGNED, specbits)))
	type = complex_double_type_node;
      else if (type == integer_type_node)
	type = complex_integer_type_node;
      else if (type == float_type_node)
	type = complex_float_type_node;
      else if (type == double_type_node)
	type = complex_double_type_node;
      else if (type == long_double_type_node)
	type = complex_long_double_type_node;
      else
	type = build_complex_type (type);
    }

  type_quals = TYPE_UNQUALIFIED;
  if (RIDBIT_SETP (RID_CONST, specbits))
    type_quals |= TYPE_QUAL_CONST;
  if (RIDBIT_SETP (RID_VOLATILE, specbits))
    type_quals |= TYPE_QUAL_VOLATILE;
  if (RIDBIT_SETP (RID_RESTRICT, specbits))
    type_quals |= TYPE_QUAL_RESTRICT;
  if (sfk == sfk_conversion && type_quals != TYPE_UNQUALIFIED)
    error ("qualifiers are not allowed on declaration of `operator %T'",
	      ctor_return_type);

  type_quals |= cp_type_quals (type);
  type = cp_build_qualified_type_real
    (type, type_quals, ((typedef_decl && !DECL_ARTIFICIAL (typedef_decl)
 			 ? tf_ignore_bad_quals : 0) | tf_error | tf_warning));
  /* We might have ignored or rejected some of the qualifiers.  */
  type_quals = cp_type_quals (type);
  
  staticp = 0;
  inlinep = !! RIDBIT_SETP (RID_INLINE, specbits);
  virtualp = RIDBIT_SETP (RID_VIRTUAL, specbits);
  RIDBIT_RESET (RID_VIRTUAL, specbits);
  explicitp = RIDBIT_SETP (RID_EXPLICIT, specbits) != 0;
  RIDBIT_RESET (RID_EXPLICIT, specbits);

  if (RIDBIT_SETP (RID_STATIC, specbits))
    staticp = 1 + (decl_context == FIELD);

  if (virtualp && staticp == 2)
    {
      error ("member `%D' cannot be declared both virtual and static",
		dname);
      staticp = 0;
    }
  friendp = RIDBIT_SETP (RID_FRIEND, specbits);
  RIDBIT_RESET (RID_FRIEND, specbits);

  if (dependant_name && !friendp)
    {
      error ("`%T::%D' is not a valid declarator", ctype, dependant_name);
      return void_type_node;
    }
  
  /* Warn if two storage classes are given. Default to `auto'.  */

  if (RIDBIT_ANY_SET (specbits))
    {
      if (RIDBIT_SETP (RID_STATIC, specbits)) nclasses++;
      if (RIDBIT_SETP (RID_EXTERN, specbits) && !extern_langp) nclasses++;
      if (RIDBIT_SETP (RID_THREAD, specbits)) nclasses++;
      if (decl_context == PARM && nclasses > 0)
	error ("storage class specifiers invalid in parameter declarations");
      if (RIDBIT_SETP (RID_TYPEDEF, specbits))
	{
	  if (decl_context == PARM)
	    error ("typedef declaration invalid in parameter declaration");
	  nclasses++;
	}
      if (RIDBIT_SETP (RID_AUTO, specbits)) nclasses++;
      if (RIDBIT_SETP (RID_REGISTER, specbits)) nclasses++;
      if (!nclasses && !friendp && extern_langp)
	nclasses++;
    }

  /* Give error if `virtual' is used outside of class declaration.  */
  if (virtualp
      && (current_class_name == NULL_TREE || decl_context != FIELD))
    {
      error ("virtual outside class declaration");
      virtualp = 0;
    }

  /* Static anonymous unions are dealt with here.  */
  if (staticp && decl_context == TYPENAME
      && TREE_CODE (declspecs) == TREE_LIST
      && ANON_AGGR_TYPE_P (TREE_VALUE (declspecs)))
    decl_context = FIELD;

  /* Warn about storage classes that are invalid for certain
     kinds of declarations (parameters, typenames, etc.).  */

  /* "static __thread" and "extern __thread" are allowed.  */
  if (nclasses == 2
      && RIDBIT_SETP (RID_THREAD, specbits)
      && (RIDBIT_SETP (RID_EXTERN, specbits)
	  || RIDBIT_SETP (RID_STATIC, specbits)))
    nclasses = 1;
    
  if (nclasses > 1)
    error ("multiple storage classes in declaration of `%s'", name);
  else if (decl_context != NORMAL && nclasses > 0)
    {
      if ((decl_context == PARM || decl_context == CATCHPARM)
	  && (RIDBIT_SETP (RID_REGISTER, specbits)
	      || RIDBIT_SETP (RID_AUTO, specbits)))
	;
      else if (RIDBIT_SETP (RID_TYPEDEF, specbits))
	;
      else if (decl_context == FIELD
	       /* C++ allows static class elements.  */
	       && RIDBIT_SETP (RID_STATIC, specbits))
	/* C++ also allows inlines and signed and unsigned elements,
	   but in those cases we don't come in here.  */
	;
      else
	{
	  if (decl_context == FIELD)
	    {
	      tree tmp = NULL_TREE;
	      int op = 0;

	      if (declarator)
		{
		  /* Avoid trying to get an operand off an identifier node.  */
		  if (TREE_CODE (declarator) == IDENTIFIER_NODE)
		    tmp = declarator;
		  else
		    tmp = TREE_OPERAND (declarator, 0);
		  op = IDENTIFIER_OPNAME_P (tmp);
		  if (IDENTIFIER_TYPENAME_P (tmp))
		    {
		      if (is_typename_at_global_scope (tmp))
			name = IDENTIFIER_POINTER (tmp);
		      else
			name = "<invalid operator>";
		    }
		}
	      error ("storage class specified for %s `%s'",
		     op ? "member operator" : "field",
		     name);
	    }
	  else
	    {
	      if (decl_context == PARM || decl_context == CATCHPARM)
		error ("storage class specified for parameter `%s'", name);
	      else
		error ("storage class specified for typename");
	    }
	  RIDBIT_RESET (RID_REGISTER, specbits);
	  RIDBIT_RESET (RID_AUTO, specbits);
	  RIDBIT_RESET (RID_EXTERN, specbits);
	  RIDBIT_RESET (RID_THREAD, specbits);
	}
    }
  else if (RIDBIT_SETP (RID_EXTERN, specbits) && initialized && !funcdef_flag)
    {
      if (toplevel_bindings_p ())
	{
	  /* It's common practice (and completely valid) to have a const
	     be initialized and declared extern.  */
	  if (!(type_quals & TYPE_QUAL_CONST))
	    warning ("`%s' initialized and declared `extern'", name);
	}
      else
	error ("`%s' has both `extern' and initializer", name);
    }
  else if (RIDBIT_SETP (RID_EXTERN, specbits) && funcdef_flag
	   && ! toplevel_bindings_p ())
    error ("nested function `%s' declared `extern'", name);
  else if (toplevel_bindings_p ())
    {
      if (RIDBIT_SETP (RID_AUTO, specbits))
	error ("top-level declaration of `%s' specifies `auto'", name);
    }
  else if (RIDBIT_SETP (RID_THREAD, specbits)
	   && !RIDBIT_SETP (RID_EXTERN, specbits)
	   && !RIDBIT_SETP (RID_STATIC, specbits))
    {
      error ("function-scope `%s' implicitly auto and declared `__thread'",
	     name);
      RIDBIT_RESET (RID_THREAD, specbits);
    }

  if (nclasses > 0 && friendp)
    error ("storage class specifiers invalid in friend function declarations");

  scope = get_scope_of_declarator (declarator);

  /* Now figure out the structure of the declarator proper.
     Descend through it, creating more complex types, until we reach
     the declared identifier (or NULL_TREE, in an abstract declarator).  */

  while (declarator && TREE_CODE (declarator) != IDENTIFIER_NODE
	 && TREE_CODE (declarator) != TEMPLATE_ID_EXPR)
    {
      /* Each level of DECLARATOR is either an ARRAY_REF (for ...[..]),
	 an INDIRECT_REF (for *...),
	 a CALL_EXPR (for ...(...)),
	 an identifier (for the name being declared)
	 or a null pointer (for the place in an absolute declarator
	 where the name was omitted).
	 For the last two cases, we have just exited the loop.

	 For C++ it could also be
	 a SCOPE_REF (for class :: ...).  In this case, we have converted
	 sensible names to types, and those are the values we use to
	 qualify the member name.
	 an ADDR_EXPR (for &...),
	 a BIT_NOT_EXPR (for destructors)

	 At this point, TYPE is the type of elements of an array,
	 or for a function to return, or for a pointer to point to.
	 After this sequence of ifs, TYPE is the type of the
	 array or function or pointer, and DECLARATOR has had its
	 outermost layer removed.  */

      if (type == error_mark_node)
	{
	  if (declarator == error_mark_node)
	    return error_mark_node;
	  else if (TREE_CODE (declarator) == SCOPE_REF)
	    declarator = TREE_OPERAND (declarator, 1);
	  else
	    declarator = TREE_OPERAND (declarator, 0);
	  continue;
	}
      if (quals != NULL_TREE
	  && (declarator == NULL_TREE
	      || TREE_CODE (declarator) != SCOPE_REF))
	{
	  if (ctype == NULL_TREE && TREE_CODE (type) == METHOD_TYPE)
	    ctype = TYPE_METHOD_BASETYPE (type);
	  if (ctype != NULL_TREE)
	    {
	      tree dummy = build_decl (TYPE_DECL, NULL_TREE, type);
	      grok_method_quals (ctype, dummy, quals);
	      type = TREE_TYPE (dummy);
	      quals = NULL_TREE;
	    }
	}

      switch (TREE_CODE (declarator))
	{
	case TREE_LIST:
	  {
	    /* We encode a declarator with embedded attributes using
	       a TREE_LIST.  */
	    tree attrs = TREE_PURPOSE (declarator);
	    tree inner_decl;
	    int attr_flags;

	    declarator = TREE_VALUE (declarator);
	    inner_decl = declarator;
	    while (inner_decl != NULL_TREE
		   && TREE_CODE (inner_decl) == TREE_LIST)
	      inner_decl = TREE_VALUE (inner_decl);
	    attr_flags = 0;
	    if (inner_decl == NULL_TREE
		|| TREE_CODE (inner_decl) == IDENTIFIER_NODE)
	      attr_flags |= (int) ATTR_FLAG_DECL_NEXT;
	    if (TREE_CODE (inner_decl) == CALL_EXPR)
	      attr_flags |= (int) ATTR_FLAG_FUNCTION_NEXT;
	    if (TREE_CODE (inner_decl) == ARRAY_REF)
	      attr_flags |= (int) ATTR_FLAG_ARRAY_NEXT;
	    returned_attrs = decl_attributes (&type,
					      chainon (returned_attrs, attrs),
					      attr_flags);
	  }
	  break;

	case ARRAY_REF:
	  {
	    tree size = TREE_OPERAND (declarator, 1);
	    declarator = TREE_OPERAND (declarator, 0);

	    type = create_array_type_for_decl (dname, type, size);

	    ctype = NULL_TREE;
	  }
	  break;

	case CALL_EXPR:
	  {
	    tree arg_types;
	    int funcdecl_p;
	    tree inner_parms = CALL_DECLARATOR_PARMS (declarator);
	    tree inner_decl = TREE_OPERAND (declarator, 0);

	    /* Declaring a function type.
	       Make sure we have a valid type for the function to return.  */

	    /* We now know that the TYPE_QUALS don't apply to the
               decl, but to its return type.  */
	    type_quals = TYPE_UNQUALIFIED;

	    /* Warn about some types functions can't return.  */

	    if (TREE_CODE (type) == FUNCTION_TYPE)
	      {
		error ("`%s' declared as function returning a function", name);
		type = integer_type_node;
	      }
	    if (TREE_CODE (type) == ARRAY_TYPE)
	      {
		error ("`%s' declared as function returning an array", name);
		type = integer_type_node;
	      }

	    if (inner_decl && TREE_CODE (inner_decl) == SCOPE_REF)
	      inner_decl = TREE_OPERAND (inner_decl, 1);

	    if (inner_decl && TREE_CODE (inner_decl) == TEMPLATE_ID_EXPR)
	      inner_decl = dname;

	    /* Pick up type qualifiers which should be applied to `this'.  */
	    quals = CALL_DECLARATOR_QUALS (declarator);

	    /* Pick up the exception specifications.  */
	    raises = CALL_DECLARATOR_EXCEPTION_SPEC (declarator);

	    /* Say it's a definition only for the CALL_EXPR
	       closest to the identifier.  */
	    funcdecl_p
	      = inner_decl
	      && (TREE_CODE (inner_decl) == IDENTIFIER_NODE
		  || TREE_CODE (inner_decl) == TEMPLATE_ID_EXPR
		  || TREE_CODE (inner_decl) == BIT_NOT_EXPR);

	    if (ctype == NULL_TREE
		&& decl_context == FIELD
		&& funcdecl_p
		&& (friendp == 0 || dname == current_class_name))
	      ctype = current_class_type;

	    if (ctype && sfk == sfk_conversion)
	      TYPE_HAS_CONVERSION (ctype) = 1;
	    if (ctype && constructor_name_p (dname, ctype))
	      {
		/* We are within a class's scope. If our declarator name
		   is the same as the class name, and we are defining
		   a function, then it is a constructor/destructor, and
		   therefore returns a void type.  */

		if (flags == DTOR_FLAG)
		  {
		    /* ISO C++ 12.4/2.  A destructor may not be
		       declared const or volatile.  A destructor may
		       not be static.  */
		    if (staticp == 2)
		      error ("destructor cannot be static member function");
		    if (quals)
		      {
			error ("destructors may not be `%s'",
				  IDENTIFIER_POINTER (TREE_VALUE (quals)));
			quals = NULL_TREE;
		      }
		    if (decl_context == FIELD)
		      {
			if (! member_function_or_else (ctype,
						       current_class_type,
						       flags))
			  return void_type_node;
		      }
		  }
		else            /* It's a constructor.  */
		  {
		    if (explicitp == 1)
		      explicitp = 2;
		    /* ISO C++ 12.1.  A constructor may not be
		       declared const or volatile.  A constructor may
		       not be virtual.  A constructor may not be
		       static.  */
		    if (staticp == 2)
		      error ("constructor cannot be static member function");
		    if (virtualp)
		      {
			pedwarn ("constructors cannot be declared virtual");
			virtualp = 0;
		      }
		    if (quals)
		      {
			error ("constructors may not be `%s'",
				  IDENTIFIER_POINTER (TREE_VALUE (quals)));
			quals = NULL_TREE;
		      }
		    {
		      RID_BIT_TYPE tmp_bits;
		      memcpy (&tmp_bits, &specbits, sizeof (RID_BIT_TYPE));
		      RIDBIT_RESET (RID_INLINE, tmp_bits);
		      RIDBIT_RESET (RID_STATIC, tmp_bits);
		      if (RIDBIT_ANY_SET (tmp_bits))
			error ("return value type specifier for constructor ignored");
		    }
		    if (decl_context == FIELD)
		      {
			if (! member_function_or_else (ctype,
						       current_class_type,
						       flags))
			  return void_type_node;
			TYPE_HAS_CONSTRUCTOR (ctype) = 1;
			if (sfk != sfk_constructor)
			  return NULL_TREE;
		      }
		  }
		if (decl_context == FIELD)
		  staticp = 0;
	      }
	    else if (friendp)
	      {
		if (initialized)
		  error ("can't initialize friend function `%s'", name);
		if (virtualp)
		  {
		    /* Cannot be both friend and virtual.  */
		    error ("virtual functions cannot be friends");
		    RIDBIT_RESET (RID_FRIEND, specbits);
		    friendp = 0;
		  }
		if (decl_context == NORMAL)
		  error ("friend declaration not in class definition");
		if (current_function_decl && funcdef_flag)
		  error ("can't define friend function `%s' in a local class definition",
			    name);
	      }

	    /* Construct the function type and go to the next
	       inner layer of declarator.  */

	    declarator = TREE_OPERAND (declarator, 0);

	    arg_types = grokparms (inner_parms, &parms);

	    if (declarator && flags == DTOR_FLAG)
	      {
		/* A destructor declared in the body of a class will
		   be represented as a BIT_NOT_EXPR.  But, we just
		   want the underlying IDENTIFIER.  */
		if (TREE_CODE (declarator) == BIT_NOT_EXPR)
		  declarator = TREE_OPERAND (declarator, 0);

                if (arg_types != void_list_node)
		  {
		    error ("destructors may not have parameters");
		    arg_types = void_list_node;
		    parms = NULL_TREE;
		  }
	      }

	    /* ANSI says that `const int foo ();'
	       does not make the function foo const.  */
	    type = build_function_type (type, arg_types);
	  }
	  break;

	case ADDR_EXPR:
	case INDIRECT_REF:
	  /* Filter out pointers-to-references and references-to-references.
	     We can get these if a TYPE_DECL is used.  */

	  if (TREE_CODE (type) == REFERENCE_TYPE)
	    {
	      error (TREE_CODE (declarator) == ADDR_EXPR
		     ? "cannot declare reference to `%#T'"
		     : "cannot declare pointer to `%#T'", type);
	      type = TREE_TYPE (type);
	    }
	  else if (VOID_TYPE_P (type)
		   && (ctype || TREE_CODE (declarator) == ADDR_EXPR))
	    error (ctype ? "cannot declare pointer to `%#T' member"
		     : "cannot declare reference to `%#T'", type);

	  /* Merge any constancy or volatility into the target type
	     for the pointer.  */

	  /* We now know that the TYPE_QUALS don't apply to the decl,
	     but to the target of the pointer.  */
	  type_quals = TYPE_UNQUALIFIED;

	  if (TREE_CODE (declarator) == ADDR_EXPR)
	    {
	      if (!VOID_TYPE_P (type))
		type = build_reference_type (type);
	    }
	  else if (TREE_CODE (type) == METHOD_TYPE)
	    type = build_ptrmemfunc_type (build_pointer_type (type));
	  else if (ctype)
	    type = build_ptrmem_type (ctype, type);
	  else
	    type = build_pointer_type (type);

	  /* Process a list of type modifier keywords (such as
	     const or volatile) that were given inside the `*' or `&'.  */

	  if (TREE_TYPE (declarator))
	    {
	      tree typemodlist;
	      int erred = 0;
	      int constp = 0;
	      int volatilep = 0;
	      int restrictp = 0;
	      
	      for (typemodlist = TREE_TYPE (declarator); typemodlist;
		   typemodlist = TREE_CHAIN (typemodlist))
		{
		  tree qualifier = TREE_VALUE (typemodlist);

		  if (qualifier == ridpointers[(int) RID_CONST])
		    {
		      constp++;
		      type_quals |= TYPE_QUAL_CONST;
		    }
		  else if (qualifier == ridpointers[(int) RID_VOLATILE])
		    {
		      volatilep++;
		      type_quals |= TYPE_QUAL_VOLATILE;
		    }
		  else if (qualifier == ridpointers[(int) RID_RESTRICT])
		    {
		      restrictp++;
		      type_quals |= TYPE_QUAL_RESTRICT;
		    }
		  else if (!erred)
		    {
		      erred = 1;
		      error ("invalid type modifier within pointer declarator");
		    }
		}
	      if (constp > 1)
		pedwarn ("duplicate `const'");
	      if (volatilep > 1)
		pedwarn ("duplicate `volatile'");
	      if (restrictp > 1)
		pedwarn ("duplicate `restrict'");
	      type = cp_build_qualified_type (type, type_quals);
	      type_quals = cp_type_quals (type);
	    }
	  declarator = TREE_OPERAND (declarator, 0);
	  ctype = NULL_TREE;
	  break;

	case SCOPE_REF:
	  {
	    /* We have converted type names to NULL_TREE if the
	       name was bogus, or to a _TYPE node, if not.

	       The variable CTYPE holds the type we will ultimately
	       resolve to.  The code here just needs to build
	       up appropriate member types.  */
	    tree sname = TREE_OPERAND (declarator, 1);
	    tree t;

	    /* Destructors can have their visibilities changed as well.  */
	    if (TREE_CODE (sname) == BIT_NOT_EXPR)
	      sname = TREE_OPERAND (sname, 0);

	    if (TREE_OPERAND (declarator, 0) == NULL_TREE)
	      {
		/* We had a reference to a global decl, or
		   perhaps we were given a non-aggregate typedef,
		   in which case we cleared this out, and should just
		   keep going as though it wasn't there.  */
		declarator = sname;
		continue;
	      }
	    ctype = TREE_OPERAND (declarator, 0);

	    t = ctype;
	    if (TREE_CODE (TREE_OPERAND (declarator, 1)) != INDIRECT_REF)
	      while (t != NULL_TREE && CLASS_TYPE_P (t))
		{
		  /* You're supposed to have one `template <...>'
		     for every template class, but you don't need one
		     for a full specialization.  For example:
		     
		     template <class T> struct S{};
		     template <> struct S<int> { void f(); };
		     void S<int>::f () {}
		     
		     is correct; there shouldn't be a `template <>' for
		     the definition of `S<int>::f'.  */
		  if (CLASSTYPE_TEMPLATE_INFO (t)
		      && (CLASSTYPE_TEMPLATE_INSTANTIATION (t)
			  || uses_template_parms (CLASSTYPE_TI_ARGS (t)))
		      && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (t)))
		    template_count += 1;
		  
		  t = TYPE_MAIN_DECL (t);
		  t = DECL_CONTEXT (t);
		}

	    if (sname == NULL_TREE)
	      goto done_scoping;

	    if (TREE_CODE (sname) == IDENTIFIER_NODE)
	      {
		/* This is the `standard' use of the scoping operator:
		   basetype :: member .  */

		if (ctype == current_class_type)
		  {
		    /* class A {
		         void A::f ();
		       };

		       Is this ill-formed?  */

		    if (pedantic)
		      pedwarn ("extra qualification `%T::' on member `%s' ignored",
				  ctype, name);
		  }
		else if (TREE_CODE (type) == FUNCTION_TYPE)
		  {
		    if (NEW_DELETE_OPNAME_P (sname))
		      /* Overloaded operator new and operator delete
			 are always static functions.  */
		      ;
		    else if (current_class_type == NULL_TREE || friendp)
		      type 
			= build_method_type_directly (ctype, 
						      TREE_TYPE (type),
						      TYPE_ARG_TYPES (type));
		    else
		      {
			error ("cannot declare member function `%T::%s' within `%T'",
				  ctype, name, current_class_type);
			return error_mark_node;
		      }
		  }
		else if (RIDBIT_SETP (RID_TYPEDEF, specbits)
			 || COMPLETE_TYPE_P (complete_type (ctype)))
		  {
		    /* Have to move this code elsewhere in this function.
		       this code is used for i.e., typedef int A::M; M *pm;

		       It is?  How? jason 10/2/94 */

		    if (current_class_type)
		      {
			error ("cannot declare member `%T::%s' within `%T'",
				  ctype, name, current_class_type);
			return void_type_node;
		      }
		  }
		else
	          {
	            cxx_incomplete_type_error (NULL_TREE, ctype);
	            return error_mark_node;
		  }

		declarator = sname;
	      }
	    else if (TREE_CODE (sname) == SCOPE_REF)
	      abort ();
	    else
	      {
	      done_scoping:
		declarator = TREE_OPERAND (declarator, 1);
		if (declarator && TREE_CODE (declarator) == CALL_EXPR)
		  /* In this case, we will deal with it later.  */
		  ;
		else if (TREE_CODE (type) == FUNCTION_TYPE)
		  type = build_method_type_directly (ctype, 
						     TREE_TYPE (type),
						     TYPE_ARG_TYPES (type));
	      }
	  }
	  break;

	case BIT_NOT_EXPR:
	  declarator = TREE_OPERAND (declarator, 0);
	  break;

	case BASELINK:
	  declarator = BASELINK_FUNCTIONS (declarator);
	  break;

	case RECORD_TYPE:
	case UNION_TYPE:
	case ENUMERAL_TYPE:
	  declarator = NULL_TREE;
	  break;

	case ERROR_MARK:
	  declarator = NULL_TREE;
	  break;

	default:
	  abort ();
	}
    }

  if (returned_attrs)
    {
      if (attrlist)
	*attrlist = chainon (returned_attrs, *attrlist);
      else
	attrlist = &returned_attrs;
    }

  /* Now TYPE has the actual type.  */

  /* Did array size calculations overflow?  */

  if (TREE_CODE (type) == ARRAY_TYPE
      && COMPLETE_TYPE_P (type)
      && TREE_OVERFLOW (TYPE_SIZE (type)))
    {
      error ("size of array `%s' is too large", name);
      /* If we proceed with the array type as it is, we'll eventually
	 crash in tree_low_cst().  */
      type = error_mark_node;
    }

  if ((decl_context == FIELD || decl_context == PARM)
      && !processing_template_decl 
      && variably_modified_type_p (type))
    {
      if (decl_context == FIELD)
	error ("data member may not have variably modified type `%T'", type);
      else
	error ("parameter may not have variably modified type `%T'", type);
      type = error_mark_node;
    }

  if (explicitp == 1 || (explicitp && friendp))
    {
      /* [dcl.fct.spec] The explicit specifier shall only be used in
         declarations of constructors within a class definition.  */
      error ("only declarations of constructors can be `explicit'");
      explicitp = 0;
    }

  if (RIDBIT_SETP (RID_MUTABLE, specbits))
    {
      if (decl_context != FIELD || friendp)
        {
	  error ("non-member `%s' cannot be declared `mutable'", name);
          RIDBIT_RESET (RID_MUTABLE, specbits);
        }
      else if (decl_context == TYPENAME || RIDBIT_SETP (RID_TYPEDEF, specbits))
	{
	  error ("non-object member `%s' cannot be declared `mutable'", name);
	  RIDBIT_RESET (RID_MUTABLE, specbits);
	}
      else if (TREE_CODE (type) == FUNCTION_TYPE
               || TREE_CODE (type) == METHOD_TYPE)
        {
	  error ("function `%s' cannot be declared `mutable'", name);
	  RIDBIT_RESET (RID_MUTABLE, specbits);
        }
      else if (staticp)
	{
	  error ("static `%s' cannot be declared `mutable'", name);
	  RIDBIT_RESET (RID_MUTABLE, specbits);
	}
      else if (type_quals & TYPE_QUAL_CONST)
	{
	  error ("const `%s' cannot be declared `mutable'", name);
 	  RIDBIT_RESET (RID_MUTABLE, specbits);
	}
    }

  if (declarator == NULL_TREE
      || TREE_CODE (declarator) == IDENTIFIER_NODE
      || (TREE_CODE (declarator) == TEMPLATE_ID_EXPR
	  && (TREE_CODE (type) == FUNCTION_TYPE
	      || TREE_CODE (type) == METHOD_TYPE)))
    /* OK */;
  else if (TREE_CODE (declarator) == TEMPLATE_ID_EXPR)
    {
      error ("template-id `%D' used as a declarator", declarator);
      declarator = dname;
    }
  else
    /* Unexpected declarator format.  */
    abort ();

  /* If this is declaring a typedef name, return a TYPE_DECL.  */

  if (RIDBIT_SETP (RID_TYPEDEF, specbits) && decl_context != TYPENAME)
    {
      tree decl;

      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters.  */
      if (current_lang_name == lang_name_java)
	TYPE_FOR_JAVA (type) = 1;

      if (decl_context == FIELD)
	{
	  if (constructor_name_p (declarator, current_class_type))
	    pedwarn ("ISO C++ forbids nested type `%D' with same name as enclosing class",
			declarator);
	  decl = build_lang_decl (TYPE_DECL, declarator, type);
	}
      else
	{
	  decl = build_decl (TYPE_DECL, declarator, type);
	  if (in_namespace || ctype)
	    error ("%Jtypedef name may not be a nested-name-specifier", decl);
	  if (!current_function_decl)
	    DECL_CONTEXT (decl) = FROB_CONTEXT (current_namespace);
	}
      
      /* If the user declares "typedef struct {...} foo" then the
	 struct will have an anonymous name.  Fill that name in now.
	 Nothing can refer to it, so nothing needs know about the name
	 change.  */
      if (type != error_mark_node
	  && declarator
	  && TYPE_NAME (type)
	  && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	  && TYPE_ANONYMOUS_P (type)
	  /* Don't do this if there are attributes.  */
	  && (!attrlist || !*attrlist)
	  && cp_type_quals (type) == TYPE_UNQUALIFIED)
	{
	  tree oldname = TYPE_NAME (type);
	  tree t;

	  /* Replace the anonymous name with the real name everywhere.  */
	  lookup_tag_reverse (type, declarator);
	  for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	    if (TYPE_NAME (t) == oldname)
	      TYPE_NAME (t) = decl;

	  if (TYPE_LANG_SPECIFIC (type))
	    TYPE_WAS_ANONYMOUS (type) = 1;

	  /* If this is a typedef within a template class, the nested
	     type is a (non-primary) template.  The name for the
	     template needs updating as well.  */
	  if (TYPE_LANG_SPECIFIC (type) && CLASSTYPE_TEMPLATE_INFO (type))
	    DECL_NAME (CLASSTYPE_TI_TEMPLATE (type))
	      = TYPE_IDENTIFIER (type);

	  /* FIXME remangle member functions; member functions of a
	     type with external linkage have external linkage.  */
	}

      if (quals)
	{
	  if (ctype == NULL_TREE)
	    {
	      if (TREE_CODE (type) != METHOD_TYPE)
		error ("%Jinvalid type qualifier for non-member function type",
		       decl);
	      else
		ctype = TYPE_METHOD_BASETYPE (type);
	    }
	  if (ctype != NULL_TREE)
	    grok_method_quals (ctype, decl, quals);
	}

      if (RIDBIT_SETP (RID_SIGNED, specbits)
	  || (typedef_decl && C_TYPEDEF_EXPLICITLY_SIGNED (typedef_decl)))
	C_TYPEDEF_EXPLICITLY_SIGNED (decl) = 1;

      bad_specifiers (decl, "type", virtualp, quals != NULL_TREE,
		      inlinep, friendp, raises != NULL_TREE);

      return decl;
    }

  /* Detect the case of an array type of unspecified size
     which came, as such, direct from a typedef name.
     We must copy the type, so that the array's domain can be
     individually set by the object's initializer.  */

  if (type && typedef_type
      && TREE_CODE (type) == ARRAY_TYPE && !TYPE_DOMAIN (type)
      && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (typedef_type))
    type = build_cplus_array_type (TREE_TYPE (type), NULL_TREE);

  /* Detect where we're using a typedef of function type to declare a
     function. PARMS will not be set, so we must create it now.  */
  
  if (type == typedef_type && TREE_CODE (type) == FUNCTION_TYPE)
    {
      tree decls = NULL_TREE;
      tree args;

      for (args = TYPE_ARG_TYPES (type); args; args = TREE_CHAIN (args))
	{
	  tree decl = cp_build_parm_decl (NULL_TREE, TREE_VALUE (args));

	  TREE_CHAIN (decl) = decls;
	  decls = decl;
	}
      
      parms = nreverse (decls);
    }

  /* If this is a type name (such as, in a cast or sizeof),
     compute the type and return it now.  */

  if (decl_context == TYPENAME)
    {
      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters.  */
      if (type_quals != TYPE_UNQUALIFIED)
	type_quals = TYPE_UNQUALIFIED;

      /* Special case: "friend class foo" looks like a TYPENAME context.  */
      if (friendp)
	{
	  if (type_quals != TYPE_UNQUALIFIED)
	    {
	      error ("type qualifiers specified for friend class declaration");
	      type_quals = TYPE_UNQUALIFIED;
	    }
	  if (inlinep)
	    {
	      error ("`inline' specified for friend class declaration");
	      inlinep = 0;
	    }

	  if (!current_aggr)
	    {
	      /* Don't allow friend declaration without a class-key.  */
	      if (TREE_CODE (type) == TEMPLATE_TYPE_PARM)
		pedwarn ("template parameters cannot be friends");
	      else if (TREE_CODE (type) == TYPENAME_TYPE)
	        pedwarn ("friend declaration requires class-key, "
			 "i.e. `friend class %T::%D'",
			 TYPE_CONTEXT (type), TYPENAME_TYPE_FULLNAME (type));
	      else
	        pedwarn ("friend declaration requires class-key, "
			 "i.e. `friend %#T'",
			 type);
	    }

	  /* Only try to do this stuff if we didn't already give up.  */
	  if (type != integer_type_node)
	    {
	      /* A friendly class?  */
	      if (current_class_type)
		make_friend_class (current_class_type, TYPE_MAIN_VARIANT (type),
				   /*complain=*/true);
	      else
		error ("trying to make class `%T' a friend of global scope",
		          type);

	      type = void_type_node;
	    }
	}
      else if (quals)
	{
	  if (ctype == NULL_TREE)
	    {
	      if (TREE_CODE (type) != METHOD_TYPE)
	        error ("invalid qualifiers on non-member function type");
	      else
	        ctype = TYPE_METHOD_BASETYPE (type);
	    }
	  if (ctype)
	    {
	      tree dummy = build_decl (TYPE_DECL, declarator, type);
	      grok_method_quals (ctype, dummy, quals);
	      type = TREE_TYPE (dummy);
	    }
	}

      return type;
    }
  else if (declarator == NULL_TREE && decl_context != PARM
	   && decl_context != CATCHPARM
	   && TREE_CODE (type) != UNION_TYPE
	   && ! bitfield)
    {
      error ("abstract declarator `%T' used as declaration", type);
      return error_mark_node;
    }

  /* Only functions may be declared using an operator-function-id.  */
  if (declarator
      && TREE_CODE (declarator) == IDENTIFIER_NODE
      && IDENTIFIER_OPNAME_P (declarator)
      && TREE_CODE (type) != FUNCTION_TYPE
      && TREE_CODE (type) != METHOD_TYPE)
    {
      error ("declaration of `%D' as non-function", declarator);
      return error_mark_node;
    }

  /* We don't check parameter types here because we can emit a better
     error message later.  */
  if (decl_context != PARM)
    type = check_var_type (declarator, type);

  /* Now create the decl, which may be a VAR_DECL, a PARM_DECL
     or a FUNCTION_DECL, depending on DECL_CONTEXT and TYPE.  */

  if (decl_context == PARM || decl_context == CATCHPARM)
    {
      if (ctype || in_namespace)
	error ("cannot use `::' in parameter declaration");

      /* A parameter declared as an array of T is really a pointer to T.
	 One declared as a function is really a pointer to a function.
	 One declared as a member is really a pointer to member.  */

      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  /* Transfer const-ness of array into that of type pointed to.  */
	  type = build_pointer_type (TREE_TYPE (type));
	  type_quals = TYPE_UNQUALIFIED;
	}
      else if (TREE_CODE (type) == FUNCTION_TYPE)
	type = build_pointer_type (type);
    }

  {
    tree decl;

    if (decl_context == PARM)
      {
	decl = cp_build_parm_decl (declarator, type);

	bad_specifiers (decl, "parameter", virtualp, quals != NULL_TREE,
			inlinep, friendp, raises != NULL_TREE);
      }
    else if (decl_context == FIELD)
      {
	/* The C99 flexible array extension.  */
	if (!staticp && TREE_CODE (type) == ARRAY_TYPE
	    && TYPE_DOMAIN (type) == NULL_TREE)
	  {
	    tree itype = compute_array_index_type (dname, integer_zero_node);
	    type = build_cplus_array_type (TREE_TYPE (type), itype);
	  }

	if (type == error_mark_node)
	  {
	    /* Happens when declaring arrays of sizes which
	       are error_mark_node, for example.  */
	    decl = NULL_TREE;
	  }
	else if (in_namespace && !friendp)
	  {
	    /* Something like struct S { int N::j; };  */
	    error ("invalid use of `::'");
	    decl = NULL_TREE;
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    int publicp = 0;
	    tree function_context;

	    /* We catch the others as conflicts with the builtin
	       typedefs.  */
	    if (friendp && declarator == ridpointers[(int) RID_SIGNED])
	      {
		error ("function `%D' cannot be declared friend",
			  declarator);
		friendp = 0;
	      }

	    if (friendp == 0)
	      {
		if (ctype == NULL_TREE)
		  ctype = current_class_type;

		if (ctype == NULL_TREE)
		  {
		    error ("can't make `%D' into a method -- not in a class",
			      declarator);
		    return void_type_node;
		  }

		/* ``A union may [ ... ] not [ have ] virtual functions.''
		   ARM 9.5 */
		if (virtualp && TREE_CODE (ctype) == UNION_TYPE)
		  {
		    error ("function `%D' declared virtual inside a union",
			      declarator);
		    return void_type_node;
		  }

		if (NEW_DELETE_OPNAME_P (declarator))
		  {
		    if (virtualp)
		      {
			error ("`%D' cannot be declared virtual, since it is always static",
				  declarator);
			virtualp = 0;
		      }
		  }
		else if (staticp < 2)
		  type = build_method_type_directly (ctype, 
						     TREE_TYPE (type),
						     TYPE_ARG_TYPES (type));
	      }

	    /* Tell grokfndecl if it needs to set TREE_PUBLIC on the node.  */
	    function_context = (ctype != NULL_TREE) ?
	      decl_function_context (TYPE_MAIN_DECL (ctype)) : NULL_TREE;
	    publicp = (! friendp || ! staticp)
	      && function_context == NULL_TREE;
	    decl = grokfndecl (ctype, type,
			       TREE_CODE (declarator) != TEMPLATE_ID_EXPR
			       ? declarator : dname,
			       parms,
			       declarator,
			       virtualp, flags, quals, raises,
			       friendp ? -1 : 0, friendp, publicp, inlinep,
			       funcdef_flag, template_count, in_namespace);
	    if (decl == NULL_TREE)
	      return decl;
#if 0
	    /* This clobbers the attrs stored in `decl' from `attrlist'.  */
	    /* The decl and setting of decl_attr is also turned off.  */
	    decl = build_decl_attribute_variant (decl, decl_attr);
#endif

	    /* [class.conv.ctor]

	       A constructor declared without the function-specifier
	       explicit that can be called with a single parameter
	       specifies a conversion from the type of its first
	       parameter to the type of its class.  Such a constructor
	       is called a converting constructor.  */
	    if (explicitp == 2)
	      DECL_NONCONVERTING_P (decl) = 1;
	    else if (DECL_CONSTRUCTOR_P (decl))
	      {
		/* The constructor can be called with exactly one
		   parameter if there is at least one parameter, and
		   any subsequent parameters have default arguments.
		   Ignore any compiler-added parms.  */
		tree arg_types = FUNCTION_FIRST_USER_PARMTYPE (decl);

		if (arg_types == void_list_node
		    || (arg_types
			&& TREE_CHAIN (arg_types)
			&& TREE_CHAIN (arg_types) != void_list_node
			&& !TREE_PURPOSE (TREE_CHAIN (arg_types))))
		  DECL_NONCONVERTING_P (decl) = 1;
	      }
	  }
	else if (TREE_CODE (type) == METHOD_TYPE)
	  {
	    /* We only get here for friend declarations of
	       members of other classes.  */
	    /* All method decls are public, so tell grokfndecl to set
	       TREE_PUBLIC, also.  */
	    decl = grokfndecl (ctype, type,
			       TREE_CODE (declarator) != TEMPLATE_ID_EXPR
			       ? declarator : dname,
			       parms,
			       declarator,
			       virtualp, flags, quals, raises,
			       friendp ? -1 : 0, friendp, 1, 0, funcdef_flag,
			       template_count, in_namespace);
	    if (decl == NULL_TREE)
	      return NULL_TREE;
	  }
	else if (!staticp && !dependent_type_p (type)
		 && !COMPLETE_TYPE_P (complete_type (type))
		 && (TREE_CODE (type) != ARRAY_TYPE || initialized == 0))
	  {
	    if (declarator)
	      error ("field `%D' has incomplete type", declarator);
	    else
	      error ("name `%T' has incomplete type", type);

	    /* If we're instantiating a template, tell them which
	       instantiation made the field's type be incomplete.  */
	    if (current_class_type
		&& TYPE_NAME (current_class_type)
		&& IDENTIFIER_TEMPLATE (TYPE_IDENTIFIER (current_class_type))
		&& declspecs && TREE_VALUE (declspecs)
		&& TREE_TYPE (TREE_VALUE (declspecs)) == type)
	      error ("  in instantiation of template `%T'",
			current_class_type);

	    type = error_mark_node;
	    decl = NULL_TREE;
	  }
	else
	  {
	    if (friendp)
	      {
		error ("`%s' is neither function nor member function; cannot be declared friend",
		       IDENTIFIER_POINTER (declarator));
		friendp = 0;
	      }
	    decl = NULL_TREE;
	  }

	if (friendp)
	  {
	    /* Friends are treated specially.  */
	    if (ctype == current_class_type)
	      warning ("member functions are implicitly friends of their class");
 	    else if (decl && DECL_NAME (decl))
	      {
		if (template_class_depth (current_class_type) == 0)
		  {
		    decl = check_explicit_specialization
		      (declarator, decl, template_count,
		       2 * (funcdef_flag != 0) + 4);
		    if (decl == error_mark_node)
		      return error_mark_node;
		  }
		
		decl = do_friend (ctype, declarator, decl,
				  *attrlist, flags, quals, funcdef_flag);
		return decl;
	      }
	    else
	      return void_type_node;
	  }

	/* Structure field.  It may not be a function, except for C++.  */

	if (decl == NULL_TREE)
	  {
	    if (initialized)
	      {
		if (!staticp)
		  {
		    /* An attempt is being made to initialize a non-static
		       member.  But, from [class.mem]:

		       4 A member-declarator can contain a
		       constant-initializer only if it declares a static
		       member (_class.static_) of integral or enumeration
		       type, see _class.static.data_.

		       This used to be relatively common practice, but
		       the rest of the compiler does not correctly
		       handle the initialization unless the member is
		       static so we make it static below.  */
		    pedwarn ("ISO C++ forbids initialization of member `%D'",
				declarator);
		    pedwarn ("making `%D' static", declarator);
		    staticp = 1;
		  }

		if (uses_template_parms (type))
		  /* We'll check at instantiation time.  */
		  ;
		else if (check_static_variable_definition (declarator,
							   type))
		  /* If we just return the declaration, crashes
		     will sometimes occur.  We therefore return
		     void_type_node, as if this was a friend
		     declaration, to cause callers to completely
		     ignore this declaration.  */
		  return void_type_node;
	      }

	    if (staticp)
	      {
		/* C++ allows static class members.  All other work
		   for this is done by grokfield.  */
		decl = build_lang_decl (VAR_DECL, declarator, type);
		TREE_STATIC (decl) = 1;
		/* In class context, 'static' means public access.  */
		TREE_PUBLIC (decl) = DECL_EXTERNAL (decl) = 1;
	      }
	    else
	      {
		decl = build_decl (FIELD_DECL, declarator, type);
		DECL_NONADDRESSABLE_P (decl) = bitfield;
		if (RIDBIT_SETP (RID_MUTABLE, specbits))
		  {
		    DECL_MUTABLE_P (decl) = 1;
		    RIDBIT_RESET (RID_MUTABLE, specbits);
		  }
	      }

	    bad_specifiers (decl, "field", virtualp, quals != NULL_TREE,
			    inlinep, friendp, raises != NULL_TREE);
	  }
      }
    else if (TREE_CODE (type) == FUNCTION_TYPE
	     || TREE_CODE (type) == METHOD_TYPE)
      {
	tree original_name;
	int publicp = 0;

	if (! declarator)
	  return NULL_TREE;

	if (TREE_CODE (declarator) == TEMPLATE_ID_EXPR)
	  original_name = dname;
	else
	  original_name = declarator;

	if (RIDBIT_SETP (RID_AUTO, specbits))
	  error ("storage class `auto' invalid for function `%s'", name);
	else if (RIDBIT_SETP (RID_REGISTER, specbits))
	  error ("storage class `register' invalid for function `%s'", name);
	else if (RIDBIT_SETP (RID_THREAD, specbits))
	  error ("storage class `__thread' invalid for function `%s'", name);

	/* Function declaration not at top level.
	   Storage classes other than `extern' are not allowed
	   and `extern' makes no difference.  */
	if (! toplevel_bindings_p ()
	    && (RIDBIT_SETP (RID_STATIC, specbits)
		|| RIDBIT_SETP (RID_INLINE, specbits))
	    && pedantic)
	  {
	    if (RIDBIT_SETP (RID_STATIC, specbits))
	      pedwarn ("storage class `static' invalid for function `%s' declared out of global scope", name);
	    else
	      pedwarn ("storage class `inline' invalid for function `%s' declared out of global scope", name);
	  }

	if (ctype == NULL_TREE)
	  {
	    if (virtualp)
	      {
		error ("virtual non-class function `%s'", name);
		virtualp = 0;
	      }
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE && staticp < 2
		 && !NEW_DELETE_OPNAME_P (original_name))
	  type = build_method_type_directly (ctype, 
					     TREE_TYPE (type),
					     TYPE_ARG_TYPES (type));

	/* Record presence of `static'.  */
	publicp = (ctype != NULL_TREE
		   || RIDBIT_SETP (RID_EXTERN, specbits)
		   || !RIDBIT_SETP (RID_STATIC, specbits));

	decl = grokfndecl (ctype, type, original_name, parms, declarator,
			   virtualp, flags, quals, raises,
			   1, friendp,
			   publicp, inlinep, funcdef_flag,
			   template_count, in_namespace);
	if (decl == NULL_TREE)
	  return NULL_TREE;

	if (staticp == 1)
	  {
	    int invalid_static = 0;

	    /* Don't allow a static member function in a class, and forbid
	       declaring main to be static.  */
	    if (TREE_CODE (type) == METHOD_TYPE)
	      {
		pedwarn ("cannot declare member function `%D' to have static linkage", decl);
		invalid_static = 1;
	      }
	    else if (current_function_decl)
	      {
		/* FIXME need arm citation */
		error ("cannot declare static function inside another function");
		invalid_static = 1;
	      }

	    if (invalid_static)
	      {
		staticp = 0;
		RIDBIT_RESET (RID_STATIC, specbits);
	      }
	  }
      }
    else
      {
	/* It's a variable.  */

	/* An uninitialized decl with `extern' is a reference.  */
	decl = grokvardecl (type, declarator, &specbits,
			    initialized,
			    (type_quals & TYPE_QUAL_CONST) != 0,
			    ctype ? ctype : in_namespace);
	bad_specifiers (decl, "variable", virtualp, quals != NULL_TREE,
			inlinep, friendp, raises != NULL_TREE);

	if (ctype)
	  {
	    DECL_CONTEXT (decl) = ctype;
	    if (staticp == 1)
	      {
                pedwarn ("`static' may not be used when defining (as opposed to declaring) a static data member");
	        staticp = 0;
		RIDBIT_RESET (RID_STATIC, specbits);
	      }
	    if (RIDBIT_SETP (RID_REGISTER, specbits) && TREE_STATIC (decl))
	      {
		error ("static member `%D' declared `register'", decl);
		RIDBIT_RESET (RID_REGISTER, specbits);
	      }
	    if (RIDBIT_SETP (RID_EXTERN, specbits) && pedantic)
	      {
	        pedwarn ("cannot explicitly declare member `%#D' to have extern linkage",
			    decl);
		RIDBIT_RESET (RID_EXTERN, specbits);
	      }
	  }
      }

    my_friendly_assert (!RIDBIT_SETP (RID_MUTABLE, specbits), 19990927);

    /* Record `register' declaration for warnings on &
       and in case doing stupid register allocation.  */

    if (RIDBIT_SETP (RID_REGISTER, specbits))
      DECL_REGISTER (decl) = 1;

    if (RIDBIT_SETP (RID_EXTERN, specbits))
      DECL_THIS_EXTERN (decl) = 1;

    if (RIDBIT_SETP (RID_STATIC, specbits))
      DECL_THIS_STATIC (decl) = 1;

    /* Record constancy and volatility.  There's no need to do this
       when processing a template; we'll do this for the instantiated
       declaration based on the type of DECL.  */
    if (!processing_template_decl)
      c_apply_type_quals_to_decl (type_quals, decl);

    return decl;
  }
}

/* Subroutine of start_function.  Ensure that each of the parameter
   types (as listed in PARMS) is complete, as is required for a
   function definition.  */

static void
require_complete_types_for_parms (tree parms)
{
  for (; parms; parms = TREE_CHAIN (parms))
    {
      if (VOID_TYPE_P (TREE_TYPE (parms)))
        /* grokparms will have already issued an error.  */
        TREE_TYPE (parms) = error_mark_node;
      else if (complete_type_or_else (TREE_TYPE (parms), parms))
	{
	  layout_decl (parms, 0);
	  DECL_ARG_TYPE (parms) = type_passed_as (TREE_TYPE (parms));
	}
    }
}

/* Returns nonzero if T is a local variable.  */

int
local_variable_p (tree t)
{
  if ((TREE_CODE (t) == VAR_DECL
       /* A VAR_DECL with a context that is a _TYPE is a static data
	  member.  */
       && !TYPE_P (CP_DECL_CONTEXT (t))
       /* Any other non-local variable must be at namespace scope.  */
       && !DECL_NAMESPACE_SCOPE_P (t))
      || (TREE_CODE (t) == PARM_DECL))
    return 1;

  return 0;
}

/* Returns nonzero if T is an automatic local variable or a label.
   (These are the declarations that need to be remapped when the code
   containing them is duplicated.)  */

int
nonstatic_local_decl_p (tree t)
{
  return ((local_variable_p (t) && !TREE_STATIC (t))
	  || TREE_CODE (t) == LABEL_DECL
	  || TREE_CODE (t) == RESULT_DECL);
}

/* Like local_variable_p, but suitable for use as a tree-walking
   function.  */

static tree
local_variable_p_walkfn (tree* tp,
                         int* walk_subtrees ATTRIBUTE_UNUSED ,
                         void* data ATTRIBUTE_UNUSED )
{
  return ((local_variable_p (*tp) && !DECL_ARTIFICIAL (*tp))
	  ? *tp : NULL_TREE);
}

/* Check that ARG, which is a default-argument expression for a
   parameter DECL, is valid.  Returns ARG, or ERROR_MARK_NODE, if
   something goes wrong.  DECL may also be a _TYPE node, rather than a
   DECL, if there is no DECL available.  */

tree
check_default_argument (tree decl, tree arg)
{
  tree var;
  tree decl_type;

  if (TREE_CODE (arg) == DEFAULT_ARG)
    /* We get a DEFAULT_ARG when looking at an in-class declaration
       with a default argument.  Ignore the argument for now; we'll
       deal with it after the class is complete.  */
    return arg;

  if (processing_template_decl || uses_template_parms (arg))
    /* We don't do anything checking until instantiation-time.  Note
       that there may be uninstantiated arguments even for an
       instantiated function, since default arguments are not
       instantiated until they are needed.  */
    return arg;

  if (TYPE_P (decl))
    {
      decl_type = decl;
      decl = NULL_TREE;
    }
  else
    decl_type = TREE_TYPE (decl);

  if (arg == error_mark_node
      || decl == error_mark_node
      || TREE_TYPE (arg) == error_mark_node
      || decl_type == error_mark_node)
    /* Something already went wrong.  There's no need to check
       further.  */
    return error_mark_node;

  /* [dcl.fct.default]

     A default argument expression is implicitly converted to the
     parameter type.  */
  if (!TREE_TYPE (arg)
      || !can_convert_arg (decl_type, TREE_TYPE (arg), arg))
    {
      if (decl)
	error ("default argument for `%#D' has type `%T'",
		  decl, TREE_TYPE (arg));
      else
	error ("default argument for parameter of type `%T' has type `%T'",
		  decl_type, TREE_TYPE (arg));

      return error_mark_node;
    }

  /* [dcl.fct.default]

     Local variables shall not be used in default argument
     expressions.

     The keyword `this' shall not be used in a default argument of a
     member function.  */
  var = walk_tree_without_duplicates (&arg, local_variable_p_walkfn,
				      NULL);
  if (var)
    {
      error ("default argument `%E' uses local variable `%D'",
		arg, var);
      return error_mark_node;
    }

  /* All is well.  */
  return arg;
}

/* Decode the list of parameter types for a function type.
   Given the list of things declared inside the parens,
   return a list of types.

   We determine whether ellipsis parms are used by PARMLIST_ELLIPSIS_P
   flag. If unset, we append void_list_node. A parmlist declared
   as `(void)' is accepted as the empty parmlist.

   *PARMS is set to the chain of PARM_DECLs created.  */

static tree
grokparms (tree first_parm, tree *parms)
{
  tree result = NULL_TREE;
  tree decls = NULL_TREE;
  int ellipsis = !first_parm || PARMLIST_ELLIPSIS_P (first_parm);
  tree parm, chain;
  int any_error = 0;

  my_friendly_assert (!first_parm || TREE_PARMLIST (first_parm), 20001115);

  for (parm = first_parm; parm != NULL_TREE; parm = chain)
    {
      tree type = NULL_TREE;
      tree decl = TREE_VALUE (parm);
      tree init = TREE_PURPOSE (parm);
      tree specs, attrs;

      chain = TREE_CHAIN (parm);
      /* @@ weak defense against parse errors.  */
      if (TREE_CODE (decl) != VOID_TYPE
	  && TREE_CODE (decl) != TREE_LIST)
	{
	  /* Give various messages as the need arises.  */
	  if (TREE_CODE (decl) == STRING_CST)
	    error ("invalid string constant `%E'", decl);
	  else if (TREE_CODE (decl) == INTEGER_CST)
	    error ("invalid integer constant in parameter list, did you forget to give parameter name?");
	  continue;
	}

      if (parm == void_list_node)
        break;

      split_specs_attrs (TREE_PURPOSE (decl), &specs, &attrs);
      decl = grokdeclarator (TREE_VALUE (decl), specs,
			     PARM, init != NULL_TREE, &attrs);
      if (! decl || TREE_TYPE (decl) == error_mark_node)
        continue;

      if (attrs)
	cplus_decl_attributes (&decl, attrs, 0);

      type = TREE_TYPE (decl);
      if (VOID_TYPE_P (type))
        {
          if (same_type_p (type, void_type_node)
              && !DECL_NAME (decl) && !result && !chain && !ellipsis)
            /* this is a parmlist of `(void)', which is ok.  */
            break;
          cxx_incomplete_type_error (decl, type);
	  /* It's not a good idea to actually create parameters of
	     type `void'; other parts of the compiler assume that a
	     void type terminates the parameter list.  */
	  type = error_mark_node;
	  TREE_TYPE (decl) = error_mark_node;
        }

      if (type != error_mark_node)
	{
	  /* Top-level qualifiers on the parameters are
	     ignored for function types.  */
	  type = cp_build_qualified_type (type, 0);
	  if (TREE_CODE (type) == METHOD_TYPE)
	    {
	      error ("parameter `%D' invalidly declared method type", decl);
	      type = build_pointer_type (type);
	      TREE_TYPE (decl) = type;
	    }
	  else if (abstract_virtuals_error (decl, type))
	    any_error = 1;  /* Seems like a good idea.  */
	  else if (POINTER_TYPE_P (type))
	    {
	      /* [dcl.fct]/6, parameter types cannot contain pointers
		 (references) to arrays of unknown bound.  */
	      tree t = TREE_TYPE (type);
	      int ptr = TYPE_PTR_P (type);

              while (1)
                {
                  if (TYPE_PTR_P (t))
                    ptr = 1;
                  else if (TREE_CODE (t) != ARRAY_TYPE)
                    break;
                  else if (!TYPE_DOMAIN (t))
	            break;
	          t = TREE_TYPE (t);
	        }
	      if (TREE_CODE (t) == ARRAY_TYPE)
		error ("parameter `%D' includes %s to array of unknown bound `%T'",
			  decl, ptr ? "pointer" : "reference", t);
	    }

	  if (!any_error && init)
	    init = check_default_argument (decl, init);
	  else
	    init = NULL_TREE;
	}

      TREE_CHAIN (decl) = decls;
      decls = decl;
      result = tree_cons (init, type, result);
    }
  decls = nreverse (decls);
  result = nreverse (result);
  if (!ellipsis)
    result = chainon (result, void_list_node);
  *parms = decls;

  return result;
}


/* D is a constructor or overloaded `operator='.

   Let T be the class in which D is declared. Then, this function
   returns:

   -1 if D's is an ill-formed constructor or copy assignment operator
      whose first parameter is of type `T'.
   0  if D is not a copy constructor or copy assignment
      operator.
   1  if D is a copy constructor or copy assignment operator whose
      first parameter is a reference to const qualified T.
   2  if D is a copy constructor or copy assignment operator whose
      first parameter is a reference to non-const qualified T.

   This function can be used as a predicate. Positive values indicate
   a copy constructor and nonzero values indicate a copy assignment
   operator.  */

int
copy_fn_p (tree d)
{
  tree args;
  tree arg_type;
  int result = 1;
  
  my_friendly_assert (DECL_FUNCTION_MEMBER_P (d), 20011208);

  if (DECL_TEMPLATE_INFO (d) && is_member_template (DECL_TI_TEMPLATE (d)))
    /* Instantiations of template member functions are never copy
       functions.  Note that member functions of templated classes are
       represented as template functions internally, and we must
       accept those as copy functions.  */
    return 0;
    
  args = FUNCTION_FIRST_USER_PARMTYPE (d);
  if (!args)
    return 0;

  arg_type = TREE_VALUE (args);

  if (TYPE_MAIN_VARIANT (arg_type) == DECL_CONTEXT (d))
    {
      /* Pass by value copy assignment operator.  */
      result = -1;
    }
  else if (TREE_CODE (arg_type) == REFERENCE_TYPE
	   && TYPE_MAIN_VARIANT (TREE_TYPE (arg_type)) == DECL_CONTEXT (d))
    {
      if (CP_TYPE_CONST_P (TREE_TYPE (arg_type)))
	result = 2;
    }
  else
    return 0;
  
  args = TREE_CHAIN (args);

  if (args && args != void_list_node && !TREE_PURPOSE (args))
    /* There are more non-optional args.  */
    return 0;

  return result;
}

/* Remember any special properties of member function DECL.  */

void grok_special_member_properties (tree decl)
{
  if (!DECL_NONSTATIC_MEMBER_FUNCTION_P(decl))
    ; /* Not special.  */
  else if (DECL_CONSTRUCTOR_P (decl))
    {
      int ctor = copy_fn_p (decl);
      
      if (ctor > 0)
	{
	  /* [class.copy]
	      
     	     A non-template constructor for class X is a copy
     	     constructor if its first parameter is of type X&, const
     	     X&, volatile X& or const volatile X&, and either there
     	     are no other parameters or else all other parameters have
     	     default arguments.  */
	  TYPE_HAS_INIT_REF (DECL_CONTEXT (decl)) = 1;
	  if (ctor > 1)
	    TYPE_HAS_CONST_INIT_REF (DECL_CONTEXT (decl)) = 1;
	}
      else if (sufficient_parms_p (FUNCTION_FIRST_USER_PARMTYPE (decl)))
	TYPE_HAS_DEFAULT_CONSTRUCTOR (DECL_CONTEXT (decl)) = 1;
    }
  else if (DECL_OVERLOADED_OPERATOR_P (decl) == NOP_EXPR)
    {
      /* [class.copy]
	  
     	 A non-template assignment operator for class X is a copy
     	 assignment operator if its parameter is of type X, X&, const
     	 X&, volatile X& or const volatile X&.  */
      
      int assop = copy_fn_p (decl);
      
      if (assop)
	{
	  TYPE_HAS_ASSIGN_REF (DECL_CONTEXT (decl)) = 1;
	  if (assop != 1)
	    TYPE_HAS_CONST_ASSIGN_REF (DECL_CONTEXT (decl)) = 1;
	  if (DECL_PURE_VIRTUAL_P (decl))
	    TYPE_HAS_ABSTRACT_ASSIGN_REF (DECL_CONTEXT (decl)) = 1;
	}
    }
}

/* Check a constructor DECL has the correct form.  Complains
   if the class has a constructor of the form X(X).  */

int
grok_ctor_properties (tree ctype, tree decl)
{
  int ctor_parm = copy_fn_p (decl);

  if (ctor_parm < 0)
    {
      /* [class.copy]
	  
     	 A declaration of a constructor for a class X is ill-formed if
     	 its first parameter is of type (optionally cv-qualified) X
     	 and either there are no other parameters or else all other
     	 parameters have default arguments.
	  
     	 We *don't* complain about member template instantiations that
     	 have this form, though; they can occur as we try to decide
     	 what constructor to use during overload resolution.  Since
     	 overload resolution will never prefer such a constructor to
     	 the non-template copy constructor (which is either explicitly
     	 or implicitly defined), there's no need to worry about their
     	 existence.  Theoretically, they should never even be
     	 instantiated, but that's hard to forestall.  */
      error ("invalid constructor; you probably meant `%T (const %T&)'",
		ctype, ctype);
      SET_IDENTIFIER_ERROR_LOCUS (DECL_NAME (decl), ctype);
      return 0;
    }
  
  return 1;
}

/* An operator with this code is unary, but can also be binary.  */

static int
ambi_op_p (enum tree_code code)
{
  return (code == INDIRECT_REF
	  || code == ADDR_EXPR
	  || code == CONVERT_EXPR
	  || code == NEGATE_EXPR
	  || code == PREINCREMENT_EXPR
	  || code == PREDECREMENT_EXPR);
}

/* An operator with this name can only be unary.  */

static int
unary_op_p (enum tree_code code)
{
  return (code == TRUTH_NOT_EXPR
	  || code == BIT_NOT_EXPR
	  || code == COMPONENT_REF
	  || code == TYPE_EXPR);
}

/* DECL is a declaration for an overloaded operator.  Returns true if
   the declaration is valid; false otherwise.  If COMPLAIN is true,
   errors are issued for invalid declarations.  */

bool
grok_op_properties (tree decl, int friendp, bool complain)
{
  tree argtypes = TYPE_ARG_TYPES (TREE_TYPE (decl));
  tree argtype;
  int methodp = (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE);
  tree name = DECL_NAME (decl);
  enum tree_code operator_code;
  int arity;
  bool ok;

  /* Assume that the declaration is valid.  */
  ok = true;

  /* Count the number of arguments.  */
  for (argtype = argtypes, arity = 0;
       argtype && argtype != void_list_node;
       argtype = TREE_CHAIN (argtype))
    ++arity;

  if (current_class_type == NULL_TREE)
    friendp = 1;

  if (DECL_CONV_FN_P (decl))
    operator_code = TYPE_EXPR;
  else
    do
      {
#define DEF_OPERATOR(NAME, CODE, MANGLING, ARITY, ASSN_P)	\
	if (ansi_opname (CODE) == name)				\
	  {							\
	    operator_code = (CODE);				\
	    break;						\
	  }							\
	else if (ansi_assopname (CODE) == name)			\
	  {							\
	    operator_code = (CODE);				\
	    DECL_ASSIGNMENT_OPERATOR_P (decl) = 1;		\
	    break;						\
	  }

#include "operators.def"
#undef DEF_OPERATOR

	abort ();
      }
    while (0);
  my_friendly_assert (operator_code != LAST_CPLUS_TREE_CODE, 20000526);
  SET_OVERLOADED_OPERATOR_CODE (decl, operator_code);

  if (! friendp)
    {
      switch (operator_code)
	{
	case NEW_EXPR:
	  TYPE_HAS_NEW_OPERATOR (current_class_type) = 1;
	  break;

	case DELETE_EXPR:
	  TYPE_GETS_DELETE (current_class_type) |= 1;
	  break;

	case VEC_NEW_EXPR:
	  TYPE_HAS_ARRAY_NEW_OPERATOR (current_class_type) = 1;
	  break;

	case VEC_DELETE_EXPR:
	  TYPE_GETS_DELETE (current_class_type) |= 2;
	  break;

	default:
	  break;
	}
    }

  if (operator_code == NEW_EXPR || operator_code == VEC_NEW_EXPR)
    TREE_TYPE (decl) = coerce_new_type (TREE_TYPE (decl));
  else if (operator_code == DELETE_EXPR || operator_code == VEC_DELETE_EXPR)
    TREE_TYPE (decl) = coerce_delete_type (TREE_TYPE (decl));
  else
    {
      /* An operator function must either be a non-static member function
	 or have at least one parameter of a class, a reference to a class,
	 an enumeration, or a reference to an enumeration.  13.4.0.6 */
      if (! methodp || DECL_STATIC_FUNCTION_P (decl))
	{
	  if (operator_code == TYPE_EXPR
	      || operator_code == CALL_EXPR
	      || operator_code == COMPONENT_REF
	      || operator_code == ARRAY_REF
	      || operator_code == NOP_EXPR)
	    error ("`%D' must be a nonstatic member function", decl);
	  else
	    {
	      tree p;

	      if (DECL_STATIC_FUNCTION_P (decl))
		error ("`%D' must be either a non-static member function or a non-member function", decl);

	      for (p = argtypes; p && p != void_list_node; p = TREE_CHAIN (p))
		{
		  tree arg = non_reference (TREE_VALUE (p));
		  /* IS_AGGR_TYPE, rather than CLASS_TYPE_P, is used
		     because these checks are performed even on
		     template functions.  */
		  if (IS_AGGR_TYPE (arg) || TREE_CODE (arg) == ENUMERAL_TYPE)
		    break;
		}

	      if (!p || p == void_list_node)
		{
		  if (!complain)
		    return false;

		  error ("`%D' must have an argument of class or "
			 "enumerated type",
			 decl);
		  ok = false;
		}
	    }
	}

      /* There are no restrictions on the arguments to an overloaded
	 "operator ()".  */
      if (operator_code == CALL_EXPR)
	return ok;

      if (IDENTIFIER_TYPENAME_P (name) && ! DECL_TEMPLATE_INFO (decl))
	{
	  tree t = TREE_TYPE (name);
	  if (! friendp)
	    {
	      int ref = (TREE_CODE (t) == REFERENCE_TYPE);
	      const char *what = 0;

	      if (ref)
		t = TYPE_MAIN_VARIANT (TREE_TYPE (t));

	      if (TREE_CODE (t) == VOID_TYPE)
	        what = "void";
	      else if (t == current_class_type)
		what = "the same type";
	      /* Don't force t to be complete here.  */
	      else if (IS_AGGR_TYPE (t)
		       && COMPLETE_TYPE_P (t)
		       && DERIVED_FROM_P (t, current_class_type))
		what = "a base class";

	      if (what && warn_conversion)
		warning ("conversion to %s%s will never use a type conversion operator",
			 ref ? "a reference to " : "", what);
	    }
	}
      if (operator_code == COND_EXPR)
	{
	  /* 13.4.0.3 */
	  error ("ISO C++ prohibits overloading operator ?:");
	}
      else if (ambi_op_p (operator_code))
	{
	  if (arity == 1)
	    /* We pick the one-argument operator codes by default, so
	       we don't have to change anything.  */
	    ;
	  else if (arity == 2)
	    {
	      /* If we thought this was a unary operator, we now know
		 it to be a binary operator.  */
	      switch (operator_code)
		{
		case INDIRECT_REF:
		  operator_code = MULT_EXPR;
		  break;

		case ADDR_EXPR:
		  operator_code = BIT_AND_EXPR;
		  break;

		case CONVERT_EXPR:
		  operator_code = PLUS_EXPR;
		  break;

		case NEGATE_EXPR:
		  operator_code = MINUS_EXPR;
		  break;

		case PREINCREMENT_EXPR:
		  operator_code = POSTINCREMENT_EXPR;
		  break;

		case PREDECREMENT_EXPR:
		  operator_code = POSTDECREMENT_EXPR;
		  break;

		default:
		  abort ();
		}

	      SET_OVERLOADED_OPERATOR_CODE (decl, operator_code);

	      if ((operator_code == POSTINCREMENT_EXPR
		   || operator_code == POSTDECREMENT_EXPR)
		  && ! processing_template_decl
		  && ! same_type_p (TREE_VALUE (TREE_CHAIN (argtypes)), integer_type_node))
		{
		  if (methodp)
		    error ("postfix `%D' must take `int' as its argument",
			      decl);
		  else
		    error
		      ("postfix `%D' must take `int' as its second argument",
		       decl);
		}
	    }
	  else
	    {
	      if (methodp)
		error ("`%D' must take either zero or one argument", decl);
	      else
		error ("`%D' must take either one or two arguments", decl);
	    }

	  /* More Effective C++ rule 6.  */
	  if (warn_ecpp
	      && (operator_code == POSTINCREMENT_EXPR
		  || operator_code == POSTDECREMENT_EXPR
		  || operator_code == PREINCREMENT_EXPR
		  || operator_code == PREDECREMENT_EXPR))
	    {
	      tree arg = TREE_VALUE (argtypes);
	      tree ret = TREE_TYPE (TREE_TYPE (decl));
	      if (methodp || TREE_CODE (arg) == REFERENCE_TYPE)
		arg = TREE_TYPE (arg);
	      arg = TYPE_MAIN_VARIANT (arg);
	      if (operator_code == PREINCREMENT_EXPR
		  || operator_code == PREDECREMENT_EXPR)
		{
		  if (TREE_CODE (ret) != REFERENCE_TYPE
		      || !same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (ret)),
				       arg))
		    warning ("prefix `%D' should return `%T'", decl,
				build_reference_type (arg));
		}
	      else
		{
		  if (!same_type_p (TYPE_MAIN_VARIANT (ret), arg))
		    warning ("postfix `%D' should return `%T'", decl, arg);
		}
	    }
	}
      else if (unary_op_p (operator_code))
	{
	  if (arity != 1)
	    {
	      if (methodp)
		error ("`%D' must take `void'", decl);
	      else
		error ("`%D' must take exactly one argument", decl);
	    }
	}
      else /* if (binary_op_p (operator_code)) */
	{
	  if (arity != 2)
	    {
	      if (methodp)
		error ("`%D' must take exactly one argument", decl);
	      else
		error ("`%D' must take exactly two arguments", decl);
	    }

	  /* More Effective C++ rule 7.  */
	  if (warn_ecpp
	      && (operator_code == TRUTH_ANDIF_EXPR
		  || operator_code == TRUTH_ORIF_EXPR
		  || operator_code == COMPOUND_EXPR))
	    warning ("user-defined `%D' always evaluates both arguments",
			decl);
	}

      /* Effective C++ rule 23.  */
      if (warn_ecpp
	  && arity == 2
	  && !DECL_ASSIGNMENT_OPERATOR_P (decl)
	  && (operator_code == PLUS_EXPR
	      || operator_code == MINUS_EXPR
	      || operator_code == TRUNC_DIV_EXPR
	      || operator_code == MULT_EXPR
	      || operator_code == TRUNC_MOD_EXPR)
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (decl))) == REFERENCE_TYPE)
	warning ("`%D' should return by value", decl);

      /* [over.oper]/8 */
      for (; argtypes && argtypes != void_list_node;
          argtypes = TREE_CHAIN (argtypes))
        if (TREE_PURPOSE (argtypes))
          {
            TREE_PURPOSE (argtypes) = NULL_TREE;
            if (operator_code == POSTINCREMENT_EXPR
		|| operator_code == POSTDECREMENT_EXPR)
              {
                if (pedantic)
                  pedwarn ("`%D' cannot have default arguments", decl);
              }
            else
              error ("`%D' cannot have default arguments", decl);
          }

    }

  return ok;
}

static const char *
tag_name (enum tag_types code)
{
  switch (code)
    {
    case record_type:
      return "struct";
    case class_type:
      return "class";
    case union_type:
      return "union ";
    case enum_type:
      return "enum";
    default:
      abort ();
    }
}

/* Name lookup in an elaborated-type-specifier (after the keyword
   indicated by TAG_CODE) has found the TYPE_DECL DECL.  If the
   elaborated-type-specifier is invalid, issue a diagnostic and return
   error_mark_node; otherwise, return the *_TYPE to which it referred.
   If ALLOW_TEMPLATE_P is true, TYPE may be a class template.  */

tree
check_elaborated_type_specifier (enum tag_types tag_code,
				 tree decl,
				 bool allow_template_p)
{
  tree type;

  /* In the case of:

       struct S { struct S *p; };

     name lookup will find the TYPE_DECL for the implicit "S::S"
     typedef.  Adjust for that here.  */
  if (DECL_SELF_REFERENCE_P (decl))
    decl = TYPE_NAME (TREE_TYPE (decl));

  type = TREE_TYPE (decl);

  /*   [dcl.type.elab] 

       If the identifier resolves to a typedef-name or a template
       type-parameter, the elaborated-type-specifier is ill-formed.

     In other words, the only legitimate declaration to use in the
     elaborated type specifier is the implicit typedef created when
     the type is declared.  */
  if (!DECL_IMPLICIT_TYPEDEF_P (decl))
    {
      error ("using typedef-name `%D' after `%s'", decl, tag_name (tag_code));
      return IS_AGGR_TYPE (type) ? type : error_mark_node;
    }
    
  if (TREE_CODE (type) == TEMPLATE_TYPE_PARM)
    {
      error ("using template type parameter `%T' after `%s'",
	     type, tag_name (tag_code));
      return error_mark_node;
    }
  else if (TREE_CODE (type) != RECORD_TYPE
	   && TREE_CODE (type) != UNION_TYPE
	   && tag_code != enum_type)
    {
      error ("`%T' referred to as `%s'", type, tag_name (tag_code));
      return error_mark_node;
    }
  else if (TREE_CODE (type) != ENUMERAL_TYPE
	   && tag_code == enum_type)
    {
      error ("`%T' referred to as enum", type);
      return error_mark_node;
    }
  else if (!allow_template_p
	   && TREE_CODE (type) == RECORD_TYPE
	   && CLASSTYPE_IS_TEMPLATE (type))
    {
      /* If a class template appears as elaborated type specifier
	 without a template header such as:

	   template <class T> class C {};
	   void f(class C);		// No template header here

	 then the required template argument is missing.  */

      error ("template argument required for `%s %T'",
	     tag_name (tag_code),
	     DECL_NAME (CLASSTYPE_TI_TEMPLATE (type)));
      return error_mark_node;
    }

  return type;
}

/* Get the struct, enum or union (TAG_CODE says which) with tag NAME.
   Define the tag as a forward-reference if it is not defined.

   If a declaration is given, process it here, and report an error if
   multiple declarations are not identical.

   GLOBALIZE is false when this is also a definition.  Only look in
   the current frame for the name (since C++ allows new names in any
   scope.)

   TEMPLATE_HEADER_P is true when this declaration is preceded by
   a set of template parameters.  */

tree
xref_tag (enum tag_types tag_code, tree name,
	  bool globalize, bool template_header_p)
{
  enum tree_code code;
  tree t;
  struct cp_binding_level *b = current_binding_level;
  tree context = NULL_TREE;

  timevar_push (TV_NAME_LOOKUP);

  my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 0);

  switch (tag_code)
    {
    case record_type:
    case class_type:
      code = RECORD_TYPE;
      break;
    case union_type:
      code = UNION_TYPE;
      break;
    case enum_type:
      code = ENUMERAL_TYPE;
      break;
    default:
      abort ();
    }

  if (! globalize)
    {
      /* If we know we are defining this tag, only look it up in
	 this scope and don't try to find it as a type.  */
      t = lookup_tag (code, name, b, 1);
    }
  else
    {
      tree decl = lookup_name (name, 2);

      if (decl && DECL_CLASS_TEMPLATE_P (decl))
	decl = DECL_TEMPLATE_RESULT (decl);

      if (decl && TREE_CODE (decl) == TYPE_DECL)
	{
	  /* Two cases we need to consider when deciding if a class
	     template is allowed as an elaborated type specifier:
	     1. It is a self reference to its own class.
	     2. It comes with a template header.

	     For example:

	       template <class T> class C {
		 class C *c1;		// DECL_SELF_REFERENCE_P is true
	 	 class D;
	       };
	       template <class U> class C; // template_header_p is true
	       template <class T> class C<T>::D {
		 class C *c2;		// DECL_SELF_REFERENCE_P is true
	       };  */

	  t = check_elaborated_type_specifier (tag_code,
					       decl,
					       template_header_p
					       | DECL_SELF_REFERENCE_P (decl));
	  if (t == error_mark_node)
	    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);
	}
      else
	t = NULL_TREE;

      if (t && current_class_type
	  && template_class_depth (current_class_type)
	  && template_header_p)
	{
	  /* Since GLOBALIZE is nonzero, we are not looking at a
	     definition of this tag.  Since, in addition, we are currently
	     processing a (member) template declaration of a template
	     class, we must be very careful; consider:

	       template <class X>
	       struct S1

	       template <class U>
	       struct S2
	       { template <class V>
	       friend struct S1; };

	     Here, the S2::S1 declaration should not be confused with the
	     outer declaration.  In particular, the inner version should
	     have a template parameter of level 2, not level 1.  This
	     would be particularly important if the member declaration
	     were instead:

	       template <class V = U> friend struct S1;

	     say, when we should tsubst into `U' when instantiating
	     S2.  On the other hand, when presented with:

	         template <class T>
	         struct S1 {
		   template <class U>
	           struct S2 {};
		   template <class U>
		   friend struct S2;
		 };

              we must find the inner binding eventually.  We
	      accomplish this by making sure that the new type we
	      create to represent this declaration has the right
	      TYPE_CONTEXT.  */
	  context = TYPE_CONTEXT (t);
	  t = NULL_TREE;
	}
    }

  if (! t)
    {
      /* If no such tag is yet defined, create a forward-reference node
	 and record it as the "definition".
	 When a real declaration of this type is found,
	 the forward-reference will be altered into a real type.  */
      if (code == ENUMERAL_TYPE)
	{
	  error ("use of enum `%#D' without previous declaration", name);
	  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);
	}
      else
	{
	  t = make_aggr_type (code);
	  TYPE_CONTEXT (t) = context;
	  pushtag (name, t, globalize);
	}
    }
  else
    {
      if (!globalize && processing_template_decl && IS_AGGR_TYPE (t))
	redeclare_class_template (t, current_template_parms);
      else if (!processing_template_decl 
	       && CLASS_TYPE_P (t)
	       && CLASSTYPE_IS_TEMPLATE (t))
	{
	  error ("redeclaration of `%T' as a non-template", t);
	  t = error_mark_node;
	}
    }

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, t);
}

tree
xref_tag_from_type (tree old, tree id, int globalize)
{
  enum tag_types tag_kind;

  if (TREE_CODE (old) == RECORD_TYPE)
    tag_kind = (CLASSTYPE_DECLARED_CLASS (old) ? class_type : record_type);
  else
    tag_kind  = union_type;

  if (id == NULL_TREE)
    id = TYPE_IDENTIFIER (old);

  return xref_tag (tag_kind, id, globalize, false);
}

/* REF is a type (named NAME), for which we have just seen some
   baseclasses.  BASE_LIST is a list of those baseclasses; the
   TREE_PURPOSE is an access_* node, and the TREE_VALUE is the type of
   the base-class.  TREE_VIA_VIRTUAL indicates virtual
   inheritance. CODE_TYPE_NODE indicates whether REF is a class,
   struct, or union.  */

void
xref_basetypes (tree ref, tree base_list)
{
  /* In the declaration `A : X, Y, ... Z' we mark all the types
     (A, X, Y, ..., Z) so we can check for duplicates.  */
  tree *basep;

  int i;
  enum tag_types tag_code;

  if (ref == error_mark_node)
    return;

  if (TREE_CODE (ref) == UNION_TYPE)
    {
      error ("derived union `%T' invalid", ref);
      return;
    }

  tag_code = (CLASSTYPE_DECLARED_CLASS (ref) ? class_type : record_type);

  /* First, make sure that any templates in base-classes are
     instantiated.  This ensures that if we call ourselves recursively
     we do not get confused about which classes are marked and which
     are not.  */
  basep = &base_list; 
  while (*basep) 
    {
      tree basetype = TREE_VALUE (*basep);
      if (!(processing_template_decl && uses_template_parms (basetype))
	  && !complete_type_or_else (basetype, NULL))
	/* An incomplete type.  Remove it from the list.  */
	*basep = TREE_CHAIN (*basep);
      else
	basep = &TREE_CHAIN (*basep);
    }

  SET_CLASSTYPE_MARKED (ref);
  i = list_length (base_list);
  if (i)
    {
      tree binfo = TYPE_BINFO (ref);
      tree binfos = make_tree_vec (i);
      tree accesses = make_tree_vec (i);
      
      BINFO_BASETYPES (binfo) = binfos;
      BINFO_BASEACCESSES (binfo) = accesses;
  
      for (i = 0; base_list; base_list = TREE_CHAIN (base_list))
	{
	  tree access = TREE_PURPOSE (base_list);
	  int via_virtual = TREE_VIA_VIRTUAL (base_list);
	  tree basetype = TREE_VALUE (base_list);
	  tree base_binfo;
	  
	  if (access == access_default_node)
	    /* The base of a derived struct is public by default.  */
	    access = (tag_code == class_type
		      ? access_private_node : access_public_node);
	  
	  if (basetype && TREE_CODE (basetype) == TYPE_DECL)
	    basetype = TREE_TYPE (basetype);
	  if (!basetype
	      || (TREE_CODE (basetype) != RECORD_TYPE
		  && TREE_CODE (basetype) != TYPENAME_TYPE
		  && TREE_CODE (basetype) != TEMPLATE_TYPE_PARM
		  && TREE_CODE (basetype) != BOUND_TEMPLATE_TEMPLATE_PARM))
	    {
	      error ("base type `%T' fails to be a struct or class type",
		     basetype);
	      continue;
	    }
	  
	  if (CLASSTYPE_MARKED (basetype))
	    {
	      if (basetype == ref)
		error ("recursive type `%T' undefined", basetype);
	      else
		error ("duplicate base type `%T' invalid", basetype);
	      continue;
	    }
	  
	  if (TYPE_FOR_JAVA (basetype)
	      && (current_lang_depth () == 0))
	    TYPE_FOR_JAVA (ref) = 1;
	  
	  if (CLASS_TYPE_P (basetype))
	    {
	      base_binfo = TYPE_BINFO (basetype);
	      /* This flag will be in the binfo of the base type, we must
	     	 clear it after copying the base binfos.  */
	      BINFO_DEPENDENT_BASE_P (base_binfo)
		= dependent_type_p (basetype);
	    }
	  else
	    base_binfo = make_binfo (size_zero_node, basetype,
				     NULL_TREE, NULL_TREE);
	  
	  TREE_VEC_ELT (binfos, i) = base_binfo;
	  TREE_VEC_ELT (accesses, i) = access;
	  /* This flag will be in the binfo of the base type, we must
	     clear it after copying the base binfos.  */
	  TREE_VIA_VIRTUAL (base_binfo) = via_virtual;
	  
	  SET_CLASSTYPE_MARKED (basetype);
	  
	  /* We are free to modify these bits because they are meaningless
	     at top level, and BASETYPE is a top-level type.  */
	  if (via_virtual || TYPE_USES_VIRTUAL_BASECLASSES (basetype))
	    {
	      TYPE_USES_VIRTUAL_BASECLASSES (ref) = 1;
	      /* Converting to a virtual base class requires looking
	     	 up the offset of the virtual base.  */
	      TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (ref) = 1;
	    }
	  
	  if (CLASS_TYPE_P (basetype))
	    {
	      TYPE_HAS_NEW_OPERATOR (ref)
		|= TYPE_HAS_NEW_OPERATOR (basetype);
	      TYPE_HAS_ARRAY_NEW_OPERATOR (ref)
		|= TYPE_HAS_ARRAY_NEW_OPERATOR (basetype);
	      TYPE_GETS_DELETE (ref) |= TYPE_GETS_DELETE (basetype);
	      /* If the base-class uses multiple inheritance, so do we.  */
	      TYPE_USES_MULTIPLE_INHERITANCE (ref)
		|= TYPE_USES_MULTIPLE_INHERITANCE (basetype);
	      /* Likewise, if converting to a base of the base may require
	     	 code, then we may need to generate code to convert to a
	     	 base as well.  */
	      TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (ref)
		|= TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (basetype);
	    }
	  i++;
	}
      if (i)
	TREE_VEC_LENGTH (accesses) = TREE_VEC_LENGTH (binfos) = i;
      else
	BINFO_BASEACCESSES (binfo) = BINFO_BASETYPES (binfo) = NULL_TREE;
      
      if (i > 1)
	{
	  TYPE_USES_MULTIPLE_INHERITANCE (ref) = 1;
	  /* If there is more than one non-empty they cannot be at the same
	     address.  */
	  TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (ref) = 1;
	}
    }
  
  /* Copy the base binfos, collect the virtual bases and set the
     inheritance order chain.  */
  copy_base_binfos (TYPE_BINFO (ref), ref, NULL_TREE);
  CLASSTYPE_VBASECLASSES (ref) = nreverse (CLASSTYPE_VBASECLASSES (ref));

  if (TYPE_FOR_JAVA (ref))
    {
      if (TYPE_USES_MULTIPLE_INHERITANCE (ref))
	error ("Java class '%T' cannot have multiple bases", ref);
      if (CLASSTYPE_VBASECLASSES (ref))
	error ("Java class '%T' cannot have virtual bases", ref);
    }

  /* Unmark all the types.  */
  while (i--)
    {
      tree basetype = BINFO_TYPE (BINFO_BASETYPE (TYPE_BINFO (ref), i));
      
      CLEAR_CLASSTYPE_MARKED (basetype);
      if (CLASS_TYPE_P (basetype))
	{
	  TREE_VIA_VIRTUAL (TYPE_BINFO (basetype)) = 0;
	  BINFO_DEPENDENT_BASE_P (TYPE_BINFO (basetype)) = 0;
	}
    }
  CLEAR_CLASSTYPE_MARKED (ref);
}


/* Begin compiling the definition of an enumeration type.
   NAME is its name (or null if anonymous).
   Returns the type object, as yet incomplete.
   Also records info about it so that build_enumerator
   may be used to declare the individual values as they are read.  */

tree
start_enum (tree name)
{
  tree enumtype = NULL_TREE;
  struct cp_binding_level *b = current_binding_level;

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

  if (name != NULL_TREE)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, b, 1);

  if (enumtype != NULL_TREE && TREE_CODE (enumtype) == ENUMERAL_TYPE)
    {
      error ("multiple definition of `%#T'", enumtype);
      error ("%Jprevious definition here", TYPE_MAIN_DECL (enumtype));
      /* Clear out TYPE_VALUES, and start again.  */
      TYPE_VALUES (enumtype) = NULL_TREE;
    }
  else
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (name, enumtype, 0);
    }

  return enumtype;
}

/* After processing and defining all the values of an enumeration type,
   install their decls in the enumeration type and finish it off.
   ENUMTYPE is the type object and VALUES a list of name-value pairs.  */

void
finish_enum (tree enumtype)
{
  tree values;
  tree decl;
  tree value;
  tree minnode;
  tree maxnode;
  tree t;
  bool unsignedp;
  int lowprec;
  int highprec; 
  int precision;
  integer_type_kind itk;
  tree underlying_type = NULL_TREE;

  /* We built up the VALUES in reverse order.  */
  TYPE_VALUES (enumtype) = nreverse (TYPE_VALUES (enumtype));

  /* For an enum defined in a template, just set the type of the values;
     all further processing is postponed until the template is
     instantiated.  We need to set the type so that tsubst of a CONST_DECL
     works.  */
  if (processing_template_decl)
    {
      for (values = TYPE_VALUES (enumtype); 
	   values; 
	   values = TREE_CHAIN (values))
	TREE_TYPE (TREE_VALUE (values)) = enumtype;
      if (at_function_scope_p ())
	add_stmt (build_min (TAG_DEFN, enumtype));
      return;
    }

  /* Determine the minimum and maximum values of the enumerators.  */
  if (TYPE_VALUES (enumtype))
    {
      minnode = maxnode = NULL_TREE;

      for (values = TYPE_VALUES (enumtype); 
	   values; 
	   values = TREE_CHAIN (values))
	{
	  decl = TREE_VALUE (values);

	  /* [dcl.enum]: Following the closing brace of an enum-specifier,
	     each enumerator has the type of its enumeration.  Prior to the
	     closing brace, the type of each enumerator is the type of its
	     initializing value.  */
	  TREE_TYPE (decl) = enumtype;

	  /* Update the minimum and maximum values, if appropriate.  */
	  value = DECL_INITIAL (decl);
	  /* Figure out what the minimum and maximum values of the
	     enumerators are.  */
	  if (!minnode)
	    minnode = maxnode = value;
	  else if (tree_int_cst_lt (maxnode, value))
	    maxnode = value;
	  else if (tree_int_cst_lt (value, minnode))
	    minnode = value;

	  /* Set the TREE_TYPE for the values as well.  That's so that when
	     we call decl_constant_value we get an entity of the right type
	     (but with the constant value).  But first make a copy so we
	     don't clobber shared INTEGER_CSTs.  */
	  if (TREE_TYPE (value) != enumtype)
	    {
	      value = DECL_INITIAL (decl) = copy_node (value);
	      TREE_TYPE (value) = enumtype;
	    }
	}
    }
  else
    /* [dcl.enum]

       If the enumerator-list is empty, the underlying type is as if
       the enumeration had a single enumerator with value 0.  */
    minnode = maxnode = integer_zero_node;

  /* Compute the number of bits require to represent all values of the
     enumeration.  We must do this before the type of MINNODE and
     MAXNODE are transformed, since min_precision relies on the
     TREE_TYPE of the value it is passed.  */
  unsignedp = tree_int_cst_sgn (minnode) >= 0;
  lowprec = min_precision (minnode, unsignedp);
  highprec = min_precision (maxnode, unsignedp);
  precision = MAX (lowprec, highprec);

  /* Determine the underlying type of the enumeration.

       [dcl.enum]

       The underlying type of an enumeration is an integral type that
       can represent all the enumerator values defined in the
       enumeration.  It is implementation-defined which integral type is
       used as the underlying type for an enumeration except that the
       underlying type shall not be larger than int unless the value of
       an enumerator cannot fit in an int or unsigned int.  

     We use "int" or an "unsigned int" as the underlying type, even if
     a smaller integral type would work, unless the user has
     explicitly requested that we use the smallest possible type.  */
  for (itk = (flag_short_enums ? itk_char : itk_int); 
       itk != itk_none; 
       itk++)
    {
      underlying_type = integer_types[itk];
      if (TYPE_PRECISION (underlying_type) >= precision
	  && TREE_UNSIGNED (underlying_type) == unsignedp)
	break;
    }
  if (itk == itk_none)
    {
      /* DR 377

	 IF no integral type can represent all the enumerator values, the
	 enumeration is ill-formed.  */
      error ("no integral type can represent all of the enumerator values "
	     "for `%T'", enumtype);
      precision = TYPE_PRECISION (long_long_integer_type_node);
      underlying_type = integer_types[itk_unsigned_long_long];
    }

  /* Compute the minium and maximum values for the type.  

     [dcl.enum]

     For an enumeration where emin is the smallest enumerator and emax
     is the largest, the values of the enumeration are the values of the
     underlying type in the range bmin to bmax, where bmin and bmax are,
     respectively, the smallest and largest values of the smallest bit-
     field that can store emin and emax.  */

  /* The middle-end currently assumes that types with TYPE_PRECISION
     narrower than their underlying type are suitably zero or sign
     extended to fill their mode.  g++ doesn't make these guarantees.
     Until the middle-end can represent such paradoxical types, we
     set the TYPE_PRECISON to the width of the underlying type.  */
  TYPE_PRECISION (enumtype) = TYPE_PRECISION (underlying_type);

  set_min_and_max_values_for_integral_type (enumtype, precision, unsignedp);

  /* [dcl.enum]
     
     The value of sizeof() applied to an enumeration type, an object
     of an enumeration type, or an enumerator, is the value of sizeof()
     applied to the underlying type.  */
  TYPE_SIZE (enumtype) = TYPE_SIZE (underlying_type);
  TYPE_SIZE_UNIT (enumtype) = TYPE_SIZE_UNIT (underlying_type);
  TYPE_MODE (enumtype) = TYPE_MODE (underlying_type);
  TYPE_ALIGN (enumtype) = TYPE_ALIGN (underlying_type);
  TYPE_USER_ALIGN (enumtype) = TYPE_USER_ALIGN (underlying_type);
  TREE_UNSIGNED (enumtype) = TREE_UNSIGNED (underlying_type);

  /* Convert each of the enumerators to the type of the underlying
     type of the enumeration.  */
  for (values = TYPE_VALUES (enumtype); values; values = TREE_CHAIN (values))
    {
      decl = TREE_VALUE (values);
      value = perform_implicit_conversion (underlying_type,
					   DECL_INITIAL (decl));
      TREE_TYPE (value) = enumtype;
      DECL_INITIAL (decl) = value;
      TREE_VALUE (values) = value;
    }

  /* Fix up all variant types of this enum type.  */
  for (t = TYPE_MAIN_VARIANT (enumtype); t; t = TYPE_NEXT_VARIANT (t))
    {
      TYPE_VALUES (t) = TYPE_VALUES (enumtype);
      TYPE_MIN_VALUE (t) = TYPE_MIN_VALUE (enumtype);
      TYPE_MAX_VALUE (t) = TYPE_MAX_VALUE (enumtype);
      TYPE_SIZE (t) = TYPE_SIZE (enumtype);
      TYPE_SIZE_UNIT (t) = TYPE_SIZE_UNIT (enumtype);
      TYPE_MODE (t) = TYPE_MODE (enumtype);
      TYPE_PRECISION (t) = TYPE_PRECISION (enumtype);
      TYPE_ALIGN (t) = TYPE_ALIGN (enumtype);
      TYPE_USER_ALIGN (t) = TYPE_USER_ALIGN (enumtype);
      TREE_UNSIGNED (t) = TREE_UNSIGNED (enumtype);
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (enumtype, namespace_bindings_p ());
}

/* Build and install a CONST_DECL for an enumeration constant of the
   enumeration type ENUMTYPE whose NAME and VALUE (if any) are provided.
   Assignment of sequential values by default is handled here.  */

void
build_enumerator (tree name, tree value, tree enumtype)
{
  tree decl;
  tree context;
  tree type;

  /* Remove no-op casts from the value.  */
  if (value)
    STRIP_TYPE_NOPS (value);

  if (! processing_template_decl)
    {
      /* Validate and default VALUE.  */
      if (value != NULL_TREE)
	{
	  value = decl_constant_value (value);

	  if (TREE_CODE (value) == INTEGER_CST)
	    {
	      value = perform_integral_promotions (value);
	      constant_expression_warning (value);
	    }
	  else
	    {
	      error ("enumerator value for `%D' not integer constant", name);
	      value = NULL_TREE;
	    }
	}

      /* Default based on previous value.  */
      if (value == NULL_TREE)
	{
	  tree prev_value;

	  if (TYPE_VALUES (enumtype))
	    {
	      /* The next value is the previous value ...  */
	      prev_value = DECL_INITIAL (TREE_VALUE (TYPE_VALUES (enumtype)));
	      /* ... plus one.  */
	      value = cp_build_binary_op (PLUS_EXPR,
					  prev_value,
					  integer_one_node);

	      if (tree_int_cst_lt (value, prev_value))
		error ("overflow in enumeration values at `%D'", name);
	    }
	  else
	    value = integer_zero_node;
	}

      /* Remove no-op casts from the value.  */
      STRIP_TYPE_NOPS (value);
    }

  /* C++ associates enums with global, function, or class declarations.  */
  context = current_scope ();
  if (!context)
    context = current_namespace;

  /* Build the actual enumeration constant.  Note that the enumeration
    constants have the type of their initializers until the
    enumeration is complete:

      [ dcl.enum ]

      Following the closing brace of an enum-specifier, each enumer-
      ator has the type of its enumeration.  Prior to the closing
      brace, the type of each enumerator is the type of its
      initializing value.

    In finish_enum we will reset the type.  Of course, if we're
    processing a template, there may be no value.  */
  type = value ? TREE_TYPE (value) : NULL_TREE;

  if (context && context == current_class_type)
    /* This enum declaration is local to the class.  We need the full
       lang_decl so that we can record DECL_CLASS_CONTEXT, for example.  */
    decl = build_lang_decl (CONST_DECL, name, type);
  else
    /* It's a global enum, or it's local to a function.  (Note local to
      a function could mean local to a class method.  */
    decl = build_decl (CONST_DECL, name, type);

  DECL_CONTEXT (decl) = FROB_CONTEXT (context);
  TREE_CONSTANT (decl) = TREE_READONLY (decl) = 1;
  DECL_INITIAL (decl) = value;

  if (context && context == current_class_type)
    /* In something like `struct S { enum E { i = 7 }; };' we put `i'
       on the TYPE_FIELDS list for `S'.  (That's so that you can say
       things like `S::i' later.)  */
    finish_member_declaration (decl);
  else
    pushdecl (decl);

  /* Add this enumeration constant to the list for this type.  */
  TYPE_VALUES (enumtype) = tree_cons (name, decl, TYPE_VALUES (enumtype));
}


/* We're defining DECL.  Make sure that it's type is OK.  */

static void
check_function_type (tree decl, tree current_function_parms)
{
  tree fntype = TREE_TYPE (decl);
  tree return_type = complete_type (TREE_TYPE (fntype));

  /* In a function definition, arg types must be complete.  */
  require_complete_types_for_parms (current_function_parms);

  if (!COMPLETE_OR_VOID_TYPE_P (return_type))
    {
      error ("return type `%#T' is incomplete", TREE_TYPE (fntype));

      /* Make it return void instead, but don't change the
	 type of the DECL_RESULT, in case we have a named return value.  */
      if (TREE_CODE (fntype) == METHOD_TYPE)
	{
	  tree ctype = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fntype)));
	  TREE_TYPE (decl)
	    = build_method_type_directly (ctype,
					  void_type_node,
					  FUNCTION_ARG_CHAIN (decl));
	}
      else
	TREE_TYPE (decl)
	  = build_function_type (void_type_node,
				 TYPE_ARG_TYPES (TREE_TYPE (decl)));
      TREE_TYPE (decl)
	= build_exception_variant (fntype,
				   TYPE_RAISES_EXCEPTIONS (fntype));
    }
  else
    abstract_virtuals_error (decl, TREE_TYPE (fntype));
}

/* Create the FUNCTION_DECL for a function definition.
   DECLSPECS and DECLARATOR are the parts of the declaration;
   they describe the function's name and the type it returns,
   but twisted together in a fashion that parallels the syntax of C.

   FLAGS is a bitwise or of SF_PRE_PARSED (indicating that the
   DECLARATOR is really the DECL for the function we are about to
   process and that DECLSPECS should be ignored), SF_INCLASS_INLINE
   indicating that the function is an inline defined in-class.

   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns 1 on success.  If the DECLARATOR is not suitable for a function
   (it defines a datum instead), we return 0, which tells
   yyparse to report a parse error.

   For C++, we must first check whether that datum makes any sense.
   For example, "class A local_a(1,2);" means that variable local_a
   is an aggregate of type A, which should have a constructor
   applied to it with the argument list [1, 2].  */

int
start_function (tree declspecs, tree declarator, tree attrs, int flags)
{
  tree decl1;
  tree ctype = NULL_TREE;
  tree fntype;
  tree restype;
  int doing_friend = 0;
  struct cp_binding_level *bl;
  tree current_function_parms;

  /* Sanity check.  */
  my_friendly_assert (TREE_CODE (TREE_VALUE (void_list_node)) == VOID_TYPE, 160);
  my_friendly_assert (TREE_CHAIN (void_list_node) == NULL_TREE, 161);

  /* This should only be done once on the top most decl.  */
  if (have_extern_spec)
    {
      declspecs = tree_cons (NULL_TREE, get_identifier ("extern"), declspecs);
      have_extern_spec = false;
    }

  if (flags & SF_PRE_PARSED)
    {
      decl1 = declarator;

      fntype = TREE_TYPE (decl1);
      if (TREE_CODE (fntype) == METHOD_TYPE)
	ctype = TYPE_METHOD_BASETYPE (fntype);

      /* ISO C++ 11.4/5.  A friend function defined in a class is in
	 the (lexical) scope of the class in which it is defined.  */
      if (!ctype && DECL_FRIEND_P (decl1))
	{
	  ctype = DECL_FRIEND_CONTEXT (decl1);

	  /* CTYPE could be null here if we're dealing with a template;
	     for example, `inline friend float foo()' inside a template
	     will have no CTYPE set.  */
	  if (ctype && TREE_CODE (ctype) != RECORD_TYPE)
	    ctype = NULL_TREE;
	  else
	    doing_friend = 1;
	}
    }
  else
    {
      decl1 = grokdeclarator (declarator, declspecs, FUNCDEF, 1, &attrs);
      /* If the declarator is not suitable for a function definition,
	 cause a syntax error.  */
      if (decl1 == NULL_TREE || TREE_CODE (decl1) != FUNCTION_DECL)
	return 0;

      cplus_decl_attributes (&decl1, attrs, 0);

      /* If #pragma weak was used, mark the decl weak now.  */
      if (global_scope_p (current_binding_level))
	maybe_apply_pragma_weak (decl1);

      fntype = TREE_TYPE (decl1);

      restype = TREE_TYPE (fntype);

      if (TREE_CODE (fntype) == METHOD_TYPE)
	ctype = TYPE_METHOD_BASETYPE (fntype);
      else if (DECL_MAIN_P (decl1))
	{
	  /* If this doesn't return integer_type, or a typedef to
	     integer_type, complain.  */
	  if (!same_type_p (TREE_TYPE (TREE_TYPE (decl1)), integer_type_node))
	    {
	      if (pedantic || warn_return_type)
		pedwarn ("return type for `main' changed to `int'");
	      TREE_TYPE (decl1) = fntype = default_function_type;
	    }
	}
    }

  if (DECL_DECLARED_INLINE_P (decl1)
      && lookup_attribute ("noinline", attrs))
    warning ("%Jinline function '%D' given attribute noinline", decl1, decl1);

  if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (decl1))
    /* This is a constructor, we must ensure that any default args
       introduced by this definition are propagated to the clones
       now. The clones are used directly in overload resolution.  */
    adjust_clone_args (decl1);

  /* Sometimes we don't notice that a function is a static member, and
     build a METHOD_TYPE for it.  Fix that up now.  */
  if (ctype != NULL_TREE && DECL_STATIC_FUNCTION_P (decl1)
      && TREE_CODE (TREE_TYPE (decl1)) == METHOD_TYPE)
    {
      revert_static_member_fn (decl1);
      ctype = NULL_TREE;
    }

  /* Warn if function was previously implicitly declared
     (but not if we warned then).  */
  if (! warn_implicit
      && IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1)) != NULL_TREE)
    cp_warning_at ("`%D' implicitly declared before its definition", IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1)));

  /* Set up current_class_type, and enter the scope of the class, if
     appropriate.  */
  if (ctype)
    push_nested_class (ctype);
  else if (DECL_STATIC_FUNCTION_P (decl1))
    push_nested_class (DECL_CONTEXT (decl1));

  /* Now that we have entered the scope of the class, we must restore
     the bindings for any template parameters surrounding DECL1, if it
     is an inline member template.  (Order is important; consider the
     case where a template parameter has the same name as a field of
     the class.)  It is not until after this point that
     PROCESSING_TEMPLATE_DECL is guaranteed to be set up correctly.  */
  if (flags & SF_INCLASS_INLINE)
    maybe_begin_member_template_processing (decl1);

  /* Effective C++ rule 15.  */
  if (warn_ecpp
      && DECL_OVERLOADED_OPERATOR_P (decl1) == NOP_EXPR
      && TREE_CODE (TREE_TYPE (fntype)) == VOID_TYPE)
    warning ("`operator=' should return a reference to `*this'");

  /* Make the init_value nonzero so pushdecl knows this is not tentative.
     error_mark_node is replaced below (in poplevel) with the BLOCK.  */
  if (!DECL_INITIAL (decl1))
    DECL_INITIAL (decl1) = error_mark_node;

  /* This function exists in static storage.
     (This does not mean `static' in the C sense!)  */
  TREE_STATIC (decl1) = 1;

  /* We must call push_template_decl after current_class_type is set
     up.  (If we are processing inline definitions after exiting a
     class scope, current_class_type will be NULL_TREE until set above
     by push_nested_class.)  */
  if (processing_template_decl)
    decl1 = push_template_decl (decl1);

  /* We are now in the scope of the function being defined.  */
  current_function_decl = decl1;

  /* Save the parm names or decls from this function's declarator
     where store_parm_decls will find them.  */
  current_function_parms = DECL_ARGUMENTS (decl1);

  /* Make sure the parameter and return types are reasonable.  When
     you declare a function, these types can be incomplete, but they
     must be complete when you define the function.  */
  if (! processing_template_decl)
    check_function_type (decl1, current_function_parms);

  /* Build the return declaration for the function.  */
  restype = TREE_TYPE (fntype);
  /* Promote the value to int before returning it.  */
  if (c_promoting_integer_type_p (restype))
    restype = type_promotes_to (restype);
  if (DECL_RESULT (decl1) == NULL_TREE)
    {
      DECL_RESULT (decl1)
	= build_decl (RESULT_DECL, 0, TYPE_MAIN_VARIANT (restype));
      c_apply_type_quals_to_decl (cp_type_quals (restype),
				  DECL_RESULT (decl1));
    }

  /* Initialize RTL machinery.  We cannot do this until
     CURRENT_FUNCTION_DECL and DECL_RESULT are set up.  We do this
     even when processing a template; this is how we get
     CFUN set up, and our per-function variables initialized.
     FIXME factor out the non-RTL stuff.  */
  bl = current_binding_level;
  allocate_struct_function (decl1);
  current_binding_level = bl;

  /* Even though we're inside a function body, we still don't want to
     call expand_expr to calculate the size of a variable-sized array.
     We haven't necessarily assigned RTL to all variables yet, so it's
     not safe to try to expand expressions involving them.  */
  immediate_size_expand = 0;
  cfun->x_dont_save_pending_sizes_p = 1;

  /* Start the statement-tree, start the tree now.  */
  begin_stmt_tree (&DECL_SAVED_TREE (decl1));

  /* Let the user know we're compiling this function.  */
  announce_function (decl1);

  /* Record the decl so that the function name is defined.
     If we already have a decl for this name, and it is a FUNCTION_DECL,
     use the old decl.  */
  if (!processing_template_decl && !(flags & SF_PRE_PARSED))
    {
      /* A specialization is not used to guide overload resolution.  */
      if (!DECL_FUNCTION_MEMBER_P (decl1)
	  && !(DECL_USE_TEMPLATE (decl1) && 
	       PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (decl1))))
	{
	  tree olddecl = pushdecl (decl1);

	  if (olddecl == error_mark_node)
	    /* If something went wrong when registering the declaration,
	       use DECL1; we have to have a FUNCTION_DECL to use when
	       parsing the body of the function.  */
	    ;
	  else
	    /* Otherwise, OLDDECL is either a previous declaration of
	       the same function or DECL1 itself.  */
	    decl1 = olddecl;
	}
      else
	{
	  /* We need to set the DECL_CONTEXT.  */
	  if (!DECL_CONTEXT (decl1) && DECL_TEMPLATE_INFO (decl1))
	    DECL_CONTEXT (decl1) = DECL_CONTEXT (DECL_TI_TEMPLATE (decl1));
	  /* And make sure we have enough default args.  */
	  check_default_args (decl1);
	}
      fntype = TREE_TYPE (decl1);
    }

  /* Reset these in case the call to pushdecl changed them.  */
  current_function_decl = decl1;
  cfun->decl = decl1;

  /* If we are (erroneously) defining a function that we have already
     defined before, wipe out what we knew before.  */
  if (!DECL_PENDING_INLINE_P (decl1))
    DECL_SAVED_FUNCTION_DATA (decl1) = NULL;

  if (ctype && !doing_friend && !DECL_STATIC_FUNCTION_P (decl1))
    {
      /* We know that this was set up by `grokclassfn'.  We do not
	 wait until `store_parm_decls', since evil parse errors may
	 never get us to that point.  Here we keep the consistency
	 between `current_class_type' and `current_class_ptr'.  */
      tree t = DECL_ARGUMENTS (decl1);

      my_friendly_assert (t != NULL_TREE && TREE_CODE (t) == PARM_DECL,
			  162);
      my_friendly_assert (TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE,
			  19990811);

      cp_function_chain->x_current_class_ref
	= build_indirect_ref (t, NULL);
      cp_function_chain->x_current_class_ptr = t;

      /* Constructors and destructors need to know whether they're "in
	 charge" of initializing virtual base classes.  */
      t = TREE_CHAIN (t);
      if (DECL_HAS_IN_CHARGE_PARM_P (decl1))
	{
	  current_in_charge_parm = t;
	  t = TREE_CHAIN (t);
	}
      if (DECL_HAS_VTT_PARM_P (decl1))
	{
	  if (DECL_NAME (t) != vtt_parm_identifier)
	    abort ();
	  current_vtt_parm = t;
	}
    }

  if (DECL_INTERFACE_KNOWN (decl1))
    {
      tree ctx = decl_function_context (decl1);

      if (DECL_NOT_REALLY_EXTERN (decl1))
	DECL_EXTERNAL (decl1) = 0;

      if (ctx != NULL_TREE && DECL_DECLARED_INLINE_P (ctx)
	  && TREE_PUBLIC (ctx))
	/* This is a function in a local class in an extern inline
	   function.  */
	comdat_linkage (decl1);
    }
  /* If this function belongs to an interface, it is public.
     If it belongs to someone else's interface, it is also external.
     This only affects inlines and template instantiations.  */
  else if (interface_unknown == 0
	   && ! DECL_TEMPLATE_INSTANTIATION (decl1))
    {
      if (DECL_DECLARED_INLINE_P (decl1) 
	  || DECL_TEMPLATE_INSTANTIATION (decl1)
	  || processing_template_decl)
	{
	  DECL_EXTERNAL (decl1)
	    = (interface_only
	       || (DECL_DECLARED_INLINE_P (decl1) 
		   && ! flag_implement_inlines
		   && !DECL_VINDEX (decl1)));

	  /* For WIN32 we also want to put these in linkonce sections.  */
	  maybe_make_one_only (decl1);
	}
      else
	DECL_EXTERNAL (decl1) = 0;
      DECL_NOT_REALLY_EXTERN (decl1) = 0;
      DECL_INTERFACE_KNOWN (decl1) = 1;
    }
  else if (interface_unknown && interface_only
	   && ! DECL_TEMPLATE_INSTANTIATION (decl1))
    {
      /* If MULTIPLE_SYMBOL_SPACES is defined and we saw a #pragma
	 interface, we will have interface_only set but not
	 interface_known.  In that case, we don't want to use the normal
	 heuristics because someone will supply a #pragma implementation
	 elsewhere, and deducing it here would produce a conflict.  */
      comdat_linkage (decl1);
      DECL_EXTERNAL (decl1) = 0;
      DECL_INTERFACE_KNOWN (decl1) = 1;
      DECL_DEFER_OUTPUT (decl1) = 1;
    }
  else
    {
      /* This is a definition, not a reference.
	 So clear DECL_EXTERNAL.  */
      DECL_EXTERNAL (decl1) = 0;

      if ((DECL_DECLARED_INLINE_P (decl1) 
	   || DECL_TEMPLATE_INSTANTIATION (decl1))
	  && ! DECL_INTERFACE_KNOWN (decl1)
	  /* Don't try to defer nested functions for now.  */
	  && ! decl_function_context (decl1))
	DECL_DEFER_OUTPUT (decl1) = 1;
      else
	DECL_INTERFACE_KNOWN (decl1) = 1;
    }

  begin_scope (sk_function_parms, decl1);

  ++function_depth;

  if (DECL_DESTRUCTOR_P (decl1))
    {
      dtor_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
      DECL_CONTEXT (dtor_label) = current_function_decl;
    }

  start_fname_decls ();
  
  store_parm_decls (current_function_parms);

  return 1;
}

/* Store the parameter declarations into the current function declaration.
   This is called after parsing the parameter declarations, before
   digesting the body of the function.

   Also install to binding contour return value identifier, if any.  */

static void
store_parm_decls (tree current_function_parms)
{
  tree fndecl = current_function_decl;
  tree parm;

  /* This is a chain of any other decls that came in among the parm
     declarations.  If a parm is declared with  enum {foo, bar} x;
     then CONST_DECLs for foo and bar are put here.  */
  tree nonparms = NULL_TREE;

  if (current_function_parms)
    {
      /* This case is when the function was defined with an ANSI prototype.
	 The parms already have decls, so we need not do anything here
	 except record them as in effect
	 and complain if any redundant old-style parm decls were written.  */

      tree specparms = current_function_parms;
      tree next;

      /* Must clear this because it might contain TYPE_DECLs declared
	     at class level.  */
      current_binding_level->names = NULL;

      /* If we're doing semantic analysis, then we'll call pushdecl
	     for each of these.  We must do them in reverse order so that
	     they end in the correct forward order.  */
      specparms = nreverse (specparms);

      for (parm = specparms; parm; parm = next)
	{
	  next = TREE_CHAIN (parm);
	  if (TREE_CODE (parm) == PARM_DECL)
	    {
	      if (DECL_NAME (parm) == NULL_TREE
		  || TREE_CODE (parm) != VOID_TYPE)
		pushdecl (parm);
	      else
		error ("parameter `%D' declared void", parm);
	    }
	  else
	    {
	      /* If we find an enum constant or a type tag,
		 put it aside for the moment.  */
	      TREE_CHAIN (parm) = NULL_TREE;
	      nonparms = chainon (nonparms, parm);
	    }
	}

      /* Get the decls in their original chain order and record in the
	 function.  This is all and only the PARM_DECLs that were
	 pushed into scope by the loop above.  */
      DECL_ARGUMENTS (fndecl) = getdecls ();
    }
  else
    DECL_ARGUMENTS (fndecl) = NULL_TREE;

  /* Now store the final chain of decls for the arguments
     as the decl-chain of the current lexical scope.
     Put the enumerators in as well, at the front so that
     DECL_ARGUMENTS is not modified.  */
  current_binding_level->names = chainon (nonparms, DECL_ARGUMENTS (fndecl));

  /* Do the starting of the exception specifications, if we have any.  */
  if (flag_exceptions && !processing_template_decl
      && flag_enforce_eh_specs
      && TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)))
    current_eh_spec_block = begin_eh_spec_block ();
}


/* We have finished doing semantic analysis on DECL, but have not yet
   generated RTL for its body.  Save away our current state, so that
   when we want to generate RTL later we know what to do.  */

static void
save_function_data (tree decl)
{
  struct language_function *f;

  /* Save the language-specific per-function data so that we can
     get it back when we really expand this function.  */
  my_friendly_assert (!DECL_PENDING_INLINE_P (decl),
		      19990908);

  /* Make a copy.  */
  f = ggc_alloc (sizeof (struct language_function));
  memcpy (f, cp_function_chain, sizeof (struct language_function));
  DECL_SAVED_FUNCTION_DATA (decl) = f;

  /* Clear out the bits we don't need.  */
  f->base.x_stmt_tree.x_last_stmt = NULL_TREE;
  f->base.x_stmt_tree.x_last_expr_type = NULL_TREE;
  f->x_named_label_uses = NULL;
  f->bindings = NULL;
  f->x_local_names = NULL;

  /* If we've already decided that we cannot inline this function, we
     must remember that fact when we actually go to expand the
     function.  */
  if (current_function_cannot_inline)
    {
      f->cannot_inline = current_function_cannot_inline;
      DECL_INLINE (decl) = 0;
    }
}

/* Add a note to mark the beginning of the main body of the constructor.
   This is used to set up the data structures for the cleanup regions for
   fully-constructed bases and members.  */

static void
begin_constructor_body (void)
{
}

/* Add a note to mark the end of the main body of the constructor.  This is
   used to end the cleanup regions for fully-constructed bases and
   members.  */

static void
finish_constructor_body (void)
{
}

/* Do all the processing for the beginning of a destructor; set up the
   vtable pointers and cleanups for bases and members.  */

static void
begin_destructor_body (void)
{
  tree if_stmt;
  tree compound_stmt;

  /* If the dtor is empty, and we know there is not any possible
     way we could use any vtable entries, before they are possibly
     set by a base class dtor, we don't have to setup the vtables,
     as we know that any base class dtor will set up any vtables
     it needs.  We avoid MI, because one base class dtor can do a
     virtual dispatch to an overridden function that would need to
     have a non-related vtable set up, we cannot avoid setting up
     vtables in that case.  We could change this to see if there
     is just one vtable.

     ??? In the destructor for a class, the vtables are set
     appropriately for that class.  There will be no non-related
     vtables.  jason 2001-12-11.  */
  if_stmt = begin_if_stmt ();

  /* If it is not safe to avoid setting up the vtables, then
     someone will change the condition to be boolean_true_node.  
     (Actually, for now, we do not have code to set the condition
     appropriately, so we just assume that we always need to
     initialize the vtables.)  */
  finish_if_stmt_cond (boolean_true_node, if_stmt);

  compound_stmt = begin_compound_stmt (/*has_no_scope=*/false);

  /* Make all virtual function table pointers in non-virtual base
     classes point to CURRENT_CLASS_TYPE's virtual function
     tables.  */
  initialize_vtbl_ptrs (current_class_ptr);

  finish_compound_stmt (compound_stmt);
  finish_then_clause (if_stmt);
  finish_if_stmt ();

  /* And insert cleanups for our bases and members so that they
     will be properly destroyed if we throw.  */
  push_base_cleanups ();
}

/* At the end of every destructor we generate code to delete the object if
   necessary.  Do that now.  */

static void
finish_destructor_body (void)
{
  tree exprstmt;

  /* Any return from a destructor will end up here; that way all base
     and member cleanups will be run when the function returns.  */
  add_stmt (build_stmt (LABEL_STMT, dtor_label));

  /* In a virtual destructor, we must call delete.  */
  if (DECL_VIRTUAL_P (current_function_decl))
    {
      tree if_stmt;
      tree virtual_size = cxx_sizeof (current_class_type);

      /* [class.dtor]

      At the point of definition of a virtual destructor (including
      an implicit definition), non-placement operator delete shall
      be looked up in the scope of the destructor's class and if
      found shall be accessible and unambiguous.  */
      exprstmt = build_op_delete_call
	(DELETE_EXPR, current_class_ptr, virtual_size,
	 LOOKUP_NORMAL | LOOKUP_SPECULATIVELY, NULL_TREE);

      if_stmt = begin_if_stmt ();
      finish_if_stmt_cond (build (BIT_AND_EXPR, integer_type_node,
				  current_in_charge_parm,
				  integer_one_node),
			   if_stmt);
      finish_expr_stmt (exprstmt);
      finish_then_clause (if_stmt);
      finish_if_stmt ();
    }
}

/* Do the necessary processing for the beginning of a function body, which
   in this case includes member-initializers, but not the catch clauses of
   a function-try-block.  Currently, this means opening a binding level
   for the member-initializers (in a ctor) and member cleanups (in a dtor).
   In other functions, this isn't necessary, but it doesn't hurt.  */

tree
begin_function_body (void)
{
  tree stmt;

  if (processing_template_decl)
    /* Do nothing now.  */;
  else
    /* Always keep the BLOCK node associated with the outermost pair of
       curly braces of a function.  These are needed for correct
       operation of dwarfout.c.  */
    keep_next_level (true);

  stmt = begin_compound_stmt (/*has_no_scope=*/false);
  COMPOUND_STMT_BODY_BLOCK (stmt) = 1;

  if (processing_template_decl)
    /* Do nothing now.  */;
  else if (DECL_CONSTRUCTOR_P (current_function_decl))
    begin_constructor_body ();
  else if (DECL_DESTRUCTOR_P (current_function_decl))
    begin_destructor_body ();

  return stmt;
}

/* Do the processing for the end of a function body.  Currently, this means
   closing out the cleanups for fully-constructed bases and members, and in
   the case of the destructor, deleting the object if desired.  Again, this
   is only meaningful for [cd]tors, since they are the only functions where
   there is a significant distinction between the main body and any
   function catch clauses.  Handling, say, main() return semantics here
   would be wrong, as flowing off the end of a function catch clause for
   main() would also need to return 0.  */

void
finish_function_body (tree compstmt)
{
  /* Close the block.  */
  finish_compound_stmt (compstmt);

  if (processing_template_decl)
    /* Do nothing now.  */;
  else if (DECL_CONSTRUCTOR_P (current_function_decl))
    finish_constructor_body ();
  else if (DECL_DESTRUCTOR_P (current_function_decl))
    finish_destructor_body ();
}  

/* Finish up a function declaration and compile that function
   all the way to assembler language output.  The free the storage
   for the function definition.

   FLAGS is a bitwise or of the following values:
     2 - INCLASS_INLINE
       We just finished processing the body of an in-class inline
       function definition.  (This processing will have taken place
       after the class definition is complete.)  */

tree
finish_function (int flags)
{
  tree fndecl = current_function_decl;
  tree fntype, ctype = NULL_TREE;
  int inclass_inline = (flags & 2) != 0;
  int nested;

  /* When we get some parse errors, we can end up without a
     current_function_decl, so cope.  */
  if (fndecl == NULL_TREE)
    return error_mark_node;

  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fndecl)
      && DECL_VIRTUAL_P (fndecl)
      && !processing_template_decl)
    {
      tree fnclass = DECL_CONTEXT (fndecl);
      if (fndecl == CLASSTYPE_KEY_METHOD (fnclass))
	keyed_classes = tree_cons (NULL_TREE, fnclass, keyed_classes);
    }

  nested = function_depth > 1;
  fntype = TREE_TYPE (fndecl);

  /*  TREE_READONLY (fndecl) = 1;
      This caused &foo to be of type ptr-to-const-function
      which then got a warning when stored in a ptr-to-function variable.  */

  my_friendly_assert (building_stmt_tree (), 20000911);
  
  /* For a cloned function, we've already got all the code we need;
     there's no need to add any extra bits.  */
  if (!DECL_CLONED_FUNCTION_P (fndecl))
    {
      if (DECL_MAIN_P (current_function_decl))
	{
	  /* Make it so that `main' always returns 0 by default.  */
#if VMS_TARGET
	  finish_return_stmt (integer_one_node);
#else
	  finish_return_stmt (integer_zero_node);
#endif
	}

      /* Finish dealing with exception specifiers.  */
      if (flag_exceptions && !processing_template_decl
	  && flag_enforce_eh_specs
	  && TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)))
	finish_eh_spec_block (TYPE_RAISES_EXCEPTIONS
			      (TREE_TYPE (current_function_decl)),
			      current_eh_spec_block);
    }

  finish_fname_decls ();

  /* If we're saving up tree structure, tie off the function now.  */
  finish_stmt_tree (&DECL_SAVED_TREE (fndecl));

  /* If this function can't throw any exceptions, remember that.  */
  if (!processing_template_decl
      && !cp_function_chain->can_throw
      && !flag_non_call_exceptions)
    TREE_NOTHROW (fndecl) = 1;

  /* This must come after expand_function_end because cleanups might
     have declarations (from inline functions) that need to go into
     this function's blocks.  */
  
  /* If the current binding level isn't the outermost binding level
     for this function, either there is a bug, or we have experienced
     syntax errors and the statement tree is malformed.  */
  if (current_binding_level->kind != sk_function_parms)
    {
      /* Make sure we have already experienced errors.  */
      if (errorcount == 0)
	abort ();

      /* Throw away the broken statement tree and extra binding
         levels.  */
      DECL_SAVED_TREE (fndecl) = build_stmt (COMPOUND_STMT, NULL_TREE);

      while (current_binding_level->kind != sk_function_parms)
	{
	  if (current_binding_level->kind == sk_class)
	    pop_nested_class ();
	  else
	    poplevel (0, 0, 0);
	}
    }
  poplevel (1, 0, 1);

  /* Statements should always be full-expressions at the outermost set
     of curly braces for a function.  */
  my_friendly_assert (stmts_are_full_exprs_p (), 19990831);

  /* Set up the named return value optimization, if we can.  Here, we
     eliminate the copy from the nrv into the RESULT_DECL and any cleanup
     for the nrv.  genrtl_start_function and declare_return_variable
     handle making the nrv and RESULT_DECL share space.  */
  if (current_function_return_value)
    {
      tree r = current_function_return_value;
      tree outer;

      if (r != error_mark_node
	  /* This is only worth doing for fns that return in memory--and
	     simpler, since we don't have to worry about promoted modes.  */
	  && aggregate_value_p (TREE_TYPE (TREE_TYPE (fndecl)), fndecl)
	  /* Only allow this for variables declared in the outer scope of
	     the function so we know that their lifetime always ends with a
	     return; see g++.dg/opt/nrv6.C.  We could be more flexible if
	     we were to do this optimization in tree-ssa.  */
	  /* Skip the artificial function body block.  */
	  && (outer = BLOCK_SUBBLOCKS (BLOCK_SUBBLOCKS (DECL_INITIAL (fndecl))),
	      chain_member (r, BLOCK_VARS (outer))))
	{
	  
	  DECL_ALIGN (r) = DECL_ALIGN (DECL_RESULT (fndecl));
	  walk_tree_without_duplicates (&DECL_SAVED_TREE (fndecl),
					nullify_returns_r, r);
	}
      else
	/* Clear it so genrtl_start_function and declare_return_variable
	   know we're not optimizing.  */
	current_function_return_value = NULL_TREE;
    }

  /* Remember that we were in class scope.  */
  if (current_class_name)
    ctype = current_class_type;

  /* Must mark the RESULT_DECL as being in this function.  */
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  /* Set the BLOCK_SUPERCONTEXT of the outermost function scope to point
     to the FUNCTION_DECL node itself.  */
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Save away current state, if appropriate.  */
  if (!processing_template_decl)
    save_function_data (fndecl);

  /* If this function calls `setjmp' it cannot be inlined.  When
     `longjmp' is called it is not guaranteed to restore the value of
     local variables that have been modified since the call to
     `setjmp'.  So, if were to inline this function into some caller
     `c', then when we `longjmp', we might not restore all variables
     in `c'.  (It might seem, at first blush, that there's no way for
     this function to modify local variables in `c', but their
     addresses may have been stored somewhere accessible to this
     function.)  */
  if (!processing_template_decl && calls_setjmp_p (fndecl))
    DECL_UNINLINABLE (fndecl) = 1;

  /* Complain if there's just no return statement.  */
  if (warn_return_type
      && TREE_CODE (TREE_TYPE (fntype)) != VOID_TYPE
      && !dependent_type_p (TREE_TYPE (fntype))
      && !current_function_returns_value && !current_function_returns_null
      /* Don't complain if we abort or throw.  */
      && !current_function_returns_abnormally
      && !DECL_NAME (DECL_RESULT (fndecl))
      /* Normally, with -Wreturn-type, flow will complain.  Unless we're an
	 inline function, as we might never be compiled separately.  */
      && (DECL_INLINE (fndecl) || processing_template_decl))
    warning ("no return statement in function returning non-void");

  /* We're leaving the context of this function, so zap cfun.  It's still in
     DECL_SAVED_INSNS, and we'll restore it in tree_rest_of_compilation.  */
  cfun = NULL;
  current_function_decl = NULL;

  /* If this is an in-class inline definition, we may have to pop the
     bindings for the template parameters that we added in
     maybe_begin_member_template_processing when start_function was
     called.  */
  if (inclass_inline)
    maybe_end_member_template_processing ();

  /* Leave the scope of the class.  */
  if (ctype)
    pop_nested_class ();

  --function_depth;

  /* Clean up.  */
  if (! nested)
    /* Let the error reporting routines know that we're outside a
       function.  For a nested function, this value is used in
       cxx_pop_function_context and then reset via pop_function_context.  */
    current_function_decl = NULL_TREE;

  return fndecl;
}

/* Create the FUNCTION_DECL for a function definition.
   DECLSPECS and DECLARATOR are the parts of the declaration;
   they describe the return type and the name of the function,
   but twisted together in a fashion that parallels the syntax of C.

   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns a FUNCTION_DECL on success.

   If the DECLARATOR is not suitable for a function (it defines a datum
   instead), we return 0, which tells yyparse to report a parse error.

   May return void_type_node indicating that this method is actually
   a friend.  See grokfield for more details.

   Came here with a `.pushlevel' .

   DO NOT MAKE ANY CHANGES TO THIS CODE WITHOUT MAKING CORRESPONDING
   CHANGES TO CODE IN `grokfield'.  */

tree
start_method (tree declspecs, tree declarator, tree attrlist)
{
  tree fndecl = grokdeclarator (declarator, declspecs, MEMFUNCDEF, 0,
				&attrlist);

  if (fndecl == error_mark_node)
    return error_mark_node;

  if (fndecl == NULL || TREE_CODE (fndecl) != FUNCTION_DECL)
    {
      error ("invalid member function declaration");
      return error_mark_node;
    }

  if (attrlist)
    cplus_decl_attributes (&fndecl, attrlist, 0);

  /* Pass friends other than inline friend functions back.  */
  if (fndecl == void_type_node)
    return fndecl;

  if (DECL_IN_AGGR_P (fndecl))
    {
      if (IDENTIFIER_ERROR_LOCUS (DECL_ASSEMBLER_NAME (fndecl)) != current_class_type)
	{
	  if (DECL_CONTEXT (fndecl)
	      && TREE_CODE( DECL_CONTEXT (fndecl)) != NAMESPACE_DECL)
	    error ("`%D' is already defined in class `%T'", fndecl,
	              DECL_CONTEXT (fndecl));
	}
      return void_type_node;
    }

  check_template_shadow (fndecl);

  DECL_DECLARED_INLINE_P (fndecl) = 1;
  if (flag_default_inline)
    DECL_INLINE (fndecl) = 1;

  /* We process method specializations in finish_struct_1.  */
  if (processing_template_decl && !DECL_TEMPLATE_SPECIALIZATION (fndecl))
    {
      fndecl = push_template_decl (fndecl);
      if (fndecl == error_mark_node)
	return fndecl;
    }

  if (! DECL_FRIEND_P (fndecl))
    {
      if (TREE_CHAIN (fndecl))
	{
	  fndecl = copy_node (fndecl);
	  TREE_CHAIN (fndecl) = NULL_TREE;
	}
      grok_special_member_properties (fndecl);
    }

  cp_finish_decl (fndecl, NULL_TREE, NULL_TREE, 0);

  /* Make a place for the parms.  */
  begin_scope (sk_function_parms, fndecl);

  DECL_IN_AGGR_P (fndecl) = 1;
  return fndecl;
}

/* Go through the motions of finishing a function definition.
   We don't compile this method until after the whole class has
   been processed.

   FINISH_METHOD must return something that looks as though it
   came from GROKFIELD (since we are defining a method, after all).

   This is called after parsing the body of the function definition.
   STMTS is the chain of statements that makes up the function body.

   DECL is the ..._DECL that `start_method' provided.  */

tree
finish_method (tree decl)
{
  tree fndecl = decl;
  tree old_initial;

  tree link;

  if (decl == void_type_node)
    return decl;

  old_initial = DECL_INITIAL (fndecl);

  /* Undo the level for the parms (from start_method).
     This is like poplevel, but it causes nothing to be
     saved.  Saving information here confuses symbol-table
     output routines.  Besides, this information will
     be correctly output when this method is actually
     compiled.  */

  /* Clear out the meanings of the local variables of this level;
     also record in each decl which block it belongs to.  */

  for (link = current_binding_level->names; link; link = TREE_CHAIN (link))
    {
      if (DECL_NAME (link) != NULL_TREE)
	pop_binding (DECL_NAME (link), link);
      my_friendly_assert (TREE_CODE (link) != FUNCTION_DECL, 163);
      DECL_CONTEXT (link) = NULL_TREE;
    }

  poplevel (0, 0, 0);

  DECL_INITIAL (fndecl) = old_initial;

  /* We used to check if the context of FNDECL was different from
     current_class_type as another way to get inside here.  This didn't work
     for String.cc in libg++.  */
  if (DECL_FRIEND_P (fndecl))
    {
      CLASSTYPE_INLINE_FRIENDS (current_class_type)
	= tree_cons (NULL_TREE, fndecl, CLASSTYPE_INLINE_FRIENDS (current_class_type));
      decl = void_type_node;
    }

  return decl;
}


/* VAR is a VAR_DECL.  If its type is incomplete, remember VAR so that
   we can lay it out later, when and if its type becomes complete.  */

void
maybe_register_incomplete_var (tree var)
{
  my_friendly_assert (TREE_CODE (var) == VAR_DECL, 20020406);

  /* Keep track of variables with incomplete types.  */
  if (!processing_template_decl && TREE_TYPE (var) != error_mark_node 
      && DECL_EXTERNAL (var))
    {
      tree inner_type = TREE_TYPE (var);
      
      while (TREE_CODE (inner_type) == ARRAY_TYPE)
	inner_type = TREE_TYPE (inner_type);
      inner_type = TYPE_MAIN_VARIANT (inner_type);
      
      if ((!COMPLETE_TYPE_P (inner_type) && CLASS_TYPE_P (inner_type))
	  /* RTTI TD entries are created while defining the type_info.  */
	  || (TYPE_LANG_SPECIFIC (inner_type)
	      && TYPE_BEING_DEFINED (inner_type)))
	incomplete_vars = tree_cons (inner_type, var, incomplete_vars);
    }
}

/* Called when a class type (given by TYPE) is defined.  If there are
   any existing VAR_DECLs whose type hsa been completed by this
   declaration, update them now.  */

void
complete_vars (tree type)
{
  tree *list = &incomplete_vars;

  my_friendly_assert (CLASS_TYPE_P (type), 20020406);
  while (*list) 
    {
      if (same_type_p (type, TREE_PURPOSE (*list)))
	{
	  tree var = TREE_VALUE (*list);
	  /* Complete the type of the variable.  The VAR_DECL itself
	     will be laid out in expand_expr.  */
	  complete_type (TREE_TYPE (var));
	  /* Remove this entry from the list.  */
	  *list = TREE_CHAIN (*list);
	}
      else
	list = &TREE_CHAIN (*list);
    }
}

/* If DECL is of a type which needs a cleanup, build that cleanup
   here.  */

tree
cxx_maybe_build_cleanup (tree decl)
{
  tree type = TREE_TYPE (decl);

  if (type != error_mark_node && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
    {
      int flags = LOOKUP_NORMAL|LOOKUP_DESTRUCTOR;
      tree rval;

      if (TREE_CODE (type) == ARRAY_TYPE)
	rval = decl;
      else
	{
	  cxx_mark_addressable (decl);
	  rval = build_unary_op (ADDR_EXPR, decl, 0);
	}

      /* Optimize for space over speed here.  */
      if (! TYPE_USES_VIRTUAL_BASECLASSES (type)
	  || flag_expensive_optimizations)
	flags |= LOOKUP_NONVIRTUAL;

      rval = build_delete (TREE_TYPE (rval), rval,
			   sfk_complete_destructor, flags, 0);

      if (TYPE_USES_VIRTUAL_BASECLASSES (type)
	  && ! TYPE_HAS_DESTRUCTOR (type))
	rval = build_compound_expr (rval, build_vbase_delete (type, decl));

      return rval;
    }
  return NULL_TREE;
}

/* When a stmt has been parsed, this function is called.  */

void
finish_stmt (void)
{
  /* Always assume this statement was not an expression statement.  If
     it actually was an expression statement, its our callers
     responsibility to fix this up.  */
  last_expr_type = NULL_TREE;
}

/* DECL was originally constructed as a non-static member function,
   but turned out to be static.  Update it accordingly.  */

void
revert_static_member_fn (tree decl)
{
  tree tmp;
  tree function = TREE_TYPE (decl);
  tree args = TYPE_ARG_TYPES (function);

  if (cp_type_quals (TREE_TYPE (TREE_VALUE (args)))
      != TYPE_UNQUALIFIED)
    error ("static member function `%#D' declared with type qualifiers",
	      decl);

  args = TREE_CHAIN (args);
  tmp = build_function_type (TREE_TYPE (function), args);
  tmp = build_qualified_type (tmp, cp_type_quals (function));
  tmp = build_exception_variant (tmp,
				 TYPE_RAISES_EXCEPTIONS (function));
  TREE_TYPE (decl) = tmp;
  if (DECL_ARGUMENTS (decl))
    DECL_ARGUMENTS (decl) = TREE_CHAIN (DECL_ARGUMENTS (decl));
  DECL_STATIC_FUNCTION_P (decl) = 1;
}

/* Initialize the variables used during compilation of a C++
   function.  */

void
cxx_push_function_context (struct function * f)
{
  struct language_function *p
    = ggc_alloc_cleared (sizeof (struct language_function));
  f->language = p;

  /* Whenever we start a new function, we destroy temporaries in the
     usual way.  */
  current_stmt_tree ()->stmts_are_full_exprs_p = 1;

  if (f->decl)
    {
      tree fn = f->decl;

      if (DECL_SAVED_FUNCTION_DATA (fn))
	{
	  /* If we already parsed this function, and we're just expanding it
	     now, restore saved state.  */
	  *cp_function_chain = *DECL_SAVED_FUNCTION_DATA (fn);

	  /* If we decided that we didn't want to inline this function,
	     make sure the back-end knows that.  */
	  if (!current_function_cannot_inline)
	    current_function_cannot_inline = cp_function_chain->cannot_inline;

	  /* We don't need the saved data anymore.  Unless this is an inline
	     function; we need the named return value info for
	     cp_copy_res_decl_for_inlining.  */
	  if (! DECL_INLINE (fn))
	    DECL_SAVED_FUNCTION_DATA (fn) = NULL;
	}
    }
}

/* Free the language-specific parts of F, now that we've finished
   compiling the function.  */

void
cxx_pop_function_context (struct function * f)
{
  f->language = 0;
}

/* Return which tree structure is used by T, or TS_CP_GENERIC if T is
   one of the language-independent trees.  */

enum cp_tree_node_structure_enum
cp_tree_node_structure (union lang_tree_node * t)
{
  switch (TREE_CODE (&t->generic))
    {
    case DEFAULT_ARG:		return TS_CP_DEFAULT_ARG;
    case IDENTIFIER_NODE:	return TS_CP_IDENTIFIER;
    case OVERLOAD:		return TS_CP_OVERLOAD;
    case TEMPLATE_PARM_INDEX:	return TS_CP_TPI;
    case PTRMEM_CST:		return TS_CP_PTRMEM;
    case BASELINK:              return TS_CP_BASELINK;
    case WRAPPER:		return TS_CP_WRAPPER;
    default:			return TS_CP_GENERIC;
    }
}

/* Build the void_list_node (void_type_node having been created).  */
tree
build_void_list_node (void)
{
  tree t = build_tree_list (NULL_TREE, void_type_node);
  TREE_PARMLIST (t) = 1;
  return t;
}

static int
cp_missing_noreturn_ok_p (tree decl)
{
  /* A missing noreturn is ok for the `main' function.  */
  return DECL_MAIN_P (decl);
}

#include "gt-cp-decl.h"
#include "gtype-cp.h"
