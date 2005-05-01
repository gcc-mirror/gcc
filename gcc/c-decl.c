/* Process declarations and variables for C compiler.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Process declarations and symbol lookup for C front end.
   Also constructs types; the standard scalar types at initialization,
   and structure, union, array and enum types when they are declared.  */

/* ??? not all decl nodes are given the most useful possible
   line numbers.  For example, the CONST_DECLs for enum values.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "intl.h"
#include "tree.h"
#include "tree-inline.h"
#include "rtl.h"
#include "flags.h"
#include "function.h"
#include "output.h"
#include "expr.h"
#include "c-tree.h"
#include "toplev.h"
#include "ggc.h"
#include "tm_p.h"
#include "cpplib.h"
#include "target.h"
#include "debug.h"
#include "opts.h"
#include "timevar.h"
#include "c-common.h"
#include "c-pragma.h"
#include "cgraph.h"
#include "hashtab.h"
#include "libfuncs.h"
#include "except.h"
#include "langhooks-def.h"

/* In grokdeclarator, distinguish syntactic contexts of declarators.  */
enum decl_context
{ NORMAL,			/* Ordinary declaration */
  FUNCDEF,			/* Function definition */
  PARM,				/* Declaration of parm before function body */
  FIELD,			/* Declaration inside struct or union */
  TYPENAME};			/* Typename (inside cast or sizeof)  */


/* Nonzero if we have seen an invalid cross reference
   to a struct, union, or enum, but not yet printed the message.  */

tree pending_invalid_xref;
/* File and line to appear in the eventual error message.  */
location_t pending_invalid_xref_location;

/* While defining an enum type, this is 1 plus the last enumerator
   constant value.  Note that will do not have to save this or `enum_overflow'
   around nested function definition since such a definition could only
   occur in an enum value expression and we don't use these variables in
   that case.  */

static tree enum_next_value;

/* Nonzero means that there was overflow computing enum_next_value.  */

static int enum_overflow;

/* Parsing a function declarator leaves a list of parameter names
   or a chain of parameter decls here.  */

static tree last_function_parms;

/* ... and a chain of structure and enum types declared in the
   parmlist here.  */

static tree last_function_parm_tags;

/* ... and a chain of all non-parameter declarations (such as
   CONST_DECLs from enumerations) here.  */

static tree last_function_parm_others;

/* After parsing the declarator that starts a function definition,
   `start_function' puts the list of parameter names or chain of decls here
   for `store_parm_decls' to find.  */

static tree current_function_parms;

/* Similar, for last_function_parm_tags.  */

static tree current_function_parm_tags;

/* And for last_function_parm_others.  */

static tree current_function_parm_others;

/* Similar, for the file and line that the prototype came from if this is
   an old-style definition.  */

static location_t current_function_prototype_locus;

/* The current statement tree.  */

static GTY(()) struct stmt_tree_s c_stmt_tree;

/* The current scope statement stack.  */

static GTY(()) tree c_scope_stmt_stack;

/* State saving variables.  */
int c_in_iteration_stmt;
int c_in_case_stmt;

/* A list of external DECLs that appeared at block scope when there was
   some other global meaning for that identifier.  */
static GTY(()) tree truly_local_externals;

/* All the builtins; this is a subset of the entries of global_scope.  */

static GTY(()) tree first_builtin_decl;
static GTY(()) tree last_builtin_decl;

/* A DECL for the current file-scope context.  */

static GTY(()) tree current_file_decl;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

int current_function_returns_value;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

int current_function_returns_null;

/* Set to 0 at beginning of a function definition, set to 1 if
   a call to a noreturn function is seen.  */

int current_function_returns_abnormally;

/* Set to nonzero by `grokdeclarator' for a function
   whose return type is defaulted, if warnings for this are desired.  */

static int warn_about_return_type;

/* Nonzero when starting a function declared `extern inline'.  */

static int current_extern_inline;

/* Each c_scope structure describes the complete contents of one scope.
   Three scopes are distinguished specially: the innermost or current
   scope, the innermost function scope, and the outermost or file scope.

   Most declarations are recorded in the current scope.

   All normal label declarations are recorded in the innermost
   function scope, as are bindings of undeclared identifiers to
   error_mark_node.  (GCC permits nested functions as an extension,
   hence the 'innermost' qualifier.)  Explicitly declared labels
   (using the __label__ extension) appear in the current scope.

   Being in the global scope (current_scope == global_scope) causes
   special behavior in several places below.  Also, under some
   conditions the Objective-C front end records declarations in the
   global scope even though that isn't the current scope.

   The order of the names, parms, and blocks lists matters, and they
   are frequently appended to.  To avoid having to walk all the way to
   the end of the list on each insertion, or reverse the lists later,
   we maintain a pointer to the last list entry for each of the lists.

   The order of the tags, shadowed, and shadowed_tags
   lists does not matter, so we just prepend to these lists.  */

struct c_scope GTY(())
{
  /* The scope containing this one.  */
  struct c_scope *outer;

  /* The next outermost function scope.  */
  struct c_scope *outer_function;

  /* All variables, constants, functions, labels, and typedef names.  */
  tree names;
  tree names_last;

  /* All parameter declarations.  Used only in the outermost scope of
     a function.  */
  tree parms;
  tree parms_last;

  /* All structure, union, and enum type tags.  */
  tree tags;

  /* For each scope, a list of shadowed outer-scope definitions
     to be restored when this scope is popped.
     Each link is a TREE_LIST whose TREE_PURPOSE is an identifier and
     whose TREE_VALUE is its old definition (a kind of ..._DECL node).  */
  tree shadowed;

  /* For each scope, a list of shadowed outer-scope tag definitions
     to be restored when this scope is popped.
     Each link is a TREE_LIST whose TREE_PURPOSE is an identifier and
     whose TREE_VALUE is its old definition (a kind of ..._TYPE node).  */
  tree shadowed_tags;

  /* For each scope (except the global one), a chain of BLOCK nodes
     for all the scopes that were entered and exited one level down.  */
  tree blocks;
  tree blocks_last;

  /* True if we are currently filling this scope with parameter
     declarations.  */
  BOOL_BITFIELD parm_flag : 1;

  /* True if we already complained about forward parameter decls
     in this scope.  This prevents double warnings on
     foo (int a; int b; ...)  */
  BOOL_BITFIELD warned_forward_parm_decls : 1;

  /* True if this is the outermost block scope of a function body.
     This scope contains the parameters, the local variables declared
     in the outermost block, and all the labels (except those in
     nested functions, or declared at block scope with __label__).  */
  BOOL_BITFIELD function_body : 1;

  /* True means make a BLOCK for this scope no matter what.  */
  BOOL_BITFIELD keep : 1;
};

/* The scope currently in effect.  */

static GTY(()) struct c_scope *current_scope;

/* A chain of c_scope structures awaiting reuse.  */

static GTY((deletable (""))) struct c_scope *scope_freelist;

/* The innermost function scope.  Ordinary (not explicitly declared)
   labels, bindings to error_mark_node, and the lazily-created
   bindings of __func__ and its friends get this scope.  */

static GTY(()) struct c_scope *current_function_scope;

/* The outermost scope, corresponding to the C "file scope".  This is
   created when the compiler is started and exists through the entire run.  */

static GTY(()) struct c_scope *global_scope;

/* Append VAR to LIST in scope SCOPE.  */
#define SCOPE_LIST_APPEND(scope, list, decl) do {	\
  struct c_scope *s_ = (scope);				\
  tree d_ = (decl);					\
  if (s_->list##_last)					\
    TREE_CHAIN (s_->list##_last) = d_;			\
  else							\
    s_->list = d_;					\
  s_->list##_last = d_;					\
} while (0)

/* Concatenate FROM in scope FSCOPE onto TO in scope TSCOPE.  */
#define SCOPE_LIST_CONCAT(tscope, to, fscope, from) do {	\
  struct c_scope *t_ = (tscope);				\
  struct c_scope *f_ = (fscope);				\
  if (t_->to##_last)						\
    TREE_CHAIN (t_->to##_last) = f_->from;			\
  else								\
    t_->to = f_->from;						\
  t_->to##_last = f_->from##_last;				\
} while (0)

/* True means unconditionally make a BLOCK for the next scope pushed.  */

static bool keep_next_level_flag;

/* True means the next call to pushlevel will be the outermost scope
   of a function body, so do not push a new scope, merely cease
   expecting parameter decls.  */

static bool next_is_function_body;

/* Functions called automatically at the beginning and end of execution.  */

tree static_ctors, static_dtors;

/* Forward declarations.  */

static struct c_scope *make_scope (void);
static void pop_scope (void);
static tree make_label (tree, location_t);
static void bind_label (tree, tree, struct c_scope *);
static void implicit_decl_warning (tree);
static tree lookup_tag (enum tree_code, tree, int);
static tree lookup_name_current_level (tree);
static tree grokdeclarator (tree, tree, enum decl_context, int, tree *);
static tree grokparms (tree, int);
static void layout_array_type (tree);
static void store_parm_decls_newstyle (void);
static void store_parm_decls_oldstyle (void);
static tree c_make_fname_decl (tree, int);
static void c_expand_body_1 (tree, int);
static tree any_external_decl (tree);
static void record_external_decl (tree);
static void warn_if_shadowing (tree, tree);
static void check_bitfield_type_and_width (tree *, tree *, const char *);
static void clone_underlying_type (tree);
static bool flexible_array_type_p (tree);
static hashval_t link_hash_hash	(const void *);
static int link_hash_eq (const void *, const void *);

/* States indicating how grokdeclarator() should handle declspecs marked
   with __attribute__((deprecated)).  An object declared as
   __attribute__((deprecated)) suppresses warnings of uses of other
   deprecated items.  */

enum deprecated_states {
  DEPRECATED_NORMAL,
  DEPRECATED_SUPPRESS
};

static enum deprecated_states deprecated_state = DEPRECATED_NORMAL;

void
c_print_identifier (FILE *file, tree node, int indent)
{
  print_node (file, "symbol", IDENTIFIER_SYMBOL_VALUE (node), indent + 4);
  print_node (file, "tag", IDENTIFIER_TAG_VALUE (node), indent + 4);
  print_node (file, "label", IDENTIFIER_LABEL_VALUE (node), indent + 4);
  if (C_IS_RESERVED_WORD (node))
    {
      tree rid = ridpointers[C_RID_CODE (node)];
      indent_to (file, indent + 4);
      fprintf (file, "rid " HOST_PTR_PRINTF " \"%s\"",
	       (void *) rid, IDENTIFIER_POINTER (rid));
    }
}

/* Hook called at end of compilation to assume 1 elt
   for a file-scope tentative array defn that wasn't complete before.  */

void
c_finish_incomplete_decl (tree decl)
{
  if (TREE_CODE (decl) == VAR_DECL)
    {
      tree type = TREE_TYPE (decl);
      if (type != error_mark_node
	  && TREE_CODE (type) == ARRAY_TYPE
	  && ! DECL_EXTERNAL (decl)
	  && TYPE_DOMAIN (type) == 0)
	{
	  warning ("%Jarray '%D' assumed to have one element", decl, decl);

	  complete_array_type (type, NULL_TREE, 1);

	  layout_decl (decl, 0);
	}
    }
}

/* Reuse or create a struct for this scope.  */

static struct c_scope *
make_scope (void)
{
  struct c_scope *result;
  if (scope_freelist)
    {
      result = scope_freelist;
      scope_freelist = result->outer;
    }
  else
    result = ggc_alloc_cleared (sizeof (struct c_scope));

  return result;
}

/* Remove the topmost scope from the stack and add it to the
   free list, updating current_function_scope if necessary.  */

static void
pop_scope (void)
{
  struct c_scope *scope = current_scope;

  current_scope = scope->outer;
  if (scope->function_body)
    current_function_scope = scope->outer_function;

  memset (scope, 0, sizeof (struct c_scope));
  scope->outer = scope_freelist;
  scope_freelist = scope;
}

/* The Objective-C front-end often needs to determine the current scope.  */

void *
get_current_scope (void)
{
  return current_scope;
}

/* The following function is used only by Objective-C.  It needs to live here
   because it accesses the innards of c_scope.  */

void
objc_mark_locals_volatile (void *enclosing_blk)
{
  struct c_scope *scope;

  for (scope = current_scope;
       scope && scope != enclosing_blk;
       scope = scope->outer)
    {
      tree decl;

      for (decl = scope->names; decl; decl = TREE_CHAIN (decl))
	{
	  DECL_REGISTER (decl) = 0;
	  TREE_THIS_VOLATILE (decl) = 1;
	}
      /* Do not climb up past the current function.  */
      if (scope->function_body)
	break;
    }
}

/* Nonzero if we are currently in the global scope.  */

int
global_bindings_p (void)
{
  return current_scope == global_scope;
}

void
keep_next_level (void)
{
  keep_next_level_flag = true;
}

/* Identify this scope as currently being filled with parameters.  */

void
declare_parm_level (void)
{
  current_scope->parm_flag = true;
}

/* Nonzero if currently making parm declarations.  */

int
in_parm_level_p (void)
{
  return current_scope->parm_flag;
}

/* Enter a new scope.  The dummy parameter is for signature
   compatibility with lang_hooks.decls.pushlevel.  */

void
pushlevel (int dummy ATTRIBUTE_UNUSED)
{
  if (next_is_function_body)
    {
      /* This is the transition from the parameters to the top level
	 of the function body.  These are the same scope
	 (C99 6.2.1p4,6) so we do not push another scope structure.
	 next_is_function_body is set only by store_parm_decls, which
	 in turn is called when and only when we are about to
	 encounter the opening curly brace for the function body.

	 The outermost block of a function always gets a BLOCK node,
	 because the debugging output routines expect that each
	 function has at least one BLOCK.  */
      current_scope->parm_flag         = false;
      current_scope->function_body     = true;
      current_scope->keep              = true;
      current_scope->outer_function    = current_function_scope;
      current_function_scope           = current_scope;

      keep_next_level_flag = false;
      next_is_function_body = false;
    }
  else
    {
      struct c_scope *scope = make_scope ();

      scope->keep          = keep_next_level_flag;
      scope->outer         = current_scope;
      current_scope        = scope;
      keep_next_level_flag = false;
    }
}

/* Exit a scope.  Restore the state of the identifier-decl mappings
   that were in effect when this scope was entered.

   If KEEP is KEEP_YES (1), this scope had explicit declarations, so
   create a BLOCK node to record its declarations and subblocks for
   debugging output.  If KEEP is KEEP_MAYBE, do so only if the names
   or tags lists are nonempty.

   The second parameter is ignored; it is present only for
   signature compatibility with lang_hooks.decls.poplevel.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   even if current_scope->function_body is not set.  This is used
   by language-independent code that generates synthetic functions,
   and cannot set current_scope->function_body.

   FIXME: Eliminate the need for all arguments.  */

tree
poplevel (int keep, int dummy ATTRIBUTE_UNUSED, int functionbody)
{
  struct c_scope *scope = current_scope;
  tree block;
  tree decl;
  tree p;

  /* The following line does not use |= due to a bug in HP's C compiler.  */
  scope->function_body = scope->function_body | functionbody;

  if (keep == KEEP_MAYBE)
    keep = (scope->names || scope->tags);

  keep |= scope->keep;
  keep |= scope->function_body;

  /* If appropriate, create a BLOCK to record the decls for the life
     of this function.  */
  block = 0;
  if (keep)
    {
      block = make_node (BLOCK);
      BLOCK_VARS (block) = scope->names;
      BLOCK_SUBBLOCKS (block) = scope->blocks;
      TREE_USED (block) = 1;
    }

  /* In each subblock, record that this is its superior.  */
  for (p = scope->blocks; p; p = TREE_CHAIN (p))
    BLOCK_SUPERCONTEXT (p) = block;

  /* Clear out the variable bindings in this scope.

     Propagate TREE_ADDRESSABLE from nested functions to their
     containing functions.

     Issue warnings for unused variables and labels, and errors for
     undefined labels, if there are any.  */

  for (p = scope->names; p; p = TREE_CHAIN (p))
    {
      switch (TREE_CODE (p))
	{
	case LABEL_DECL:
	  if (TREE_USED (p) && !DECL_INITIAL (p))
	    {
	      error ("%Jlabel `%D' used but not defined", p, p);
	      DECL_INITIAL (p) = error_mark_node;
	    }
	  else if (!TREE_USED (p) && warn_unused_label)
	    {
	      if (DECL_INITIAL (p))
		warning ("%Jlabel `%D' defined but not used", p, p);
	      else
		warning ("%Jlabel `%D' declared but not defined", p, p);
	    }

	  IDENTIFIER_LABEL_VALUE (DECL_NAME (p)) = 0;
	  break;

	case FUNCTION_DECL:
	  if (! TREE_ASM_WRITTEN (p)
	      && DECL_INITIAL (p) != 0
	      && TREE_ADDRESSABLE (p)
	      && DECL_ABSTRACT_ORIGIN (p) != 0
	      && DECL_ABSTRACT_ORIGIN (p) != p)
	    TREE_ADDRESSABLE (DECL_ABSTRACT_ORIGIN (p)) = 1;
	  goto normal;

	case VAR_DECL:
	  /* Keep this in sync with stmt.c:warn_about_unused_variables.
	     No warnings when the global scope is popped because the
	     global scope isn't popped for the last translation unit,
	     so the warnings are done in c_write_global_declaration.  */
	  if (warn_unused_variable && scope != global_scope
	      && !TREE_USED (p)
	      && !DECL_IN_SYSTEM_HEADER (p)
	      && DECL_NAME (p)
	      && !DECL_ARTIFICIAL (p))
	    warning ("%Junused variable `%D'", p, p);
	  /* fall through */

	default:
	normal:
	  if (DECL_NAME (p))
	    {
	      if (DECL_EXTERNAL (p) && scope != global_scope)
		/* External decls stay in the symbol-value slot but are
		   inaccessible.  */
		C_DECL_INVISIBLE (p) = 1;
	      else
		IDENTIFIER_SYMBOL_VALUE (DECL_NAME (p)) = 0;
	    }
	  break;
	}
    }

  /* Clear out the parameter bindings in this scope, if any.
     Unused-parameter warnings are handled by function.c.  */
  for (p = scope->parms; p; p = TREE_CHAIN (p))
    if (DECL_NAME (p))
      IDENTIFIER_SYMBOL_VALUE (DECL_NAME (p)) = 0;

  /* Clear out the tag-meanings declared in this scope.

     Set the TYPE_CONTEXTs for all of the tagged types belonging to
     this scope so that they point to the appropriate construct, i.e.
     either to the current FUNCTION_DECL node, or else to the BLOCK
     node we just constructed.

     Note that for tagged types whose scope is just the formal
     parameter list for some function type specification, we can't
     properly set their TYPE_CONTEXTs here, because we don't have a
     pointer to the appropriate FUNCTION_TYPE node readily available
     to us.  For those cases, the TYPE_CONTEXTs of the relevant tagged
     type nodes get set in `grokdeclarator' as soon as we have created
     the FUNCTION_TYPE node which will represent the "scope" for these
     "parameter list local" tagged types.  */

  decl = scope->function_body ? current_function_decl : block;
  for (p = scope->tags; p; p = TREE_CHAIN (p))
    {
      if (TREE_PURPOSE (p))
	IDENTIFIER_TAG_VALUE (TREE_PURPOSE (p)) = 0;
      if (decl)
	TYPE_CONTEXT (TREE_VALUE (p)) = decl;
    }

  /* Restore all name- and label-meanings from outer scopes that were
     shadowed by this scope.  */
  for (p = scope->shadowed; p; p = TREE_CHAIN (p))
    if (TREE_VALUE (p) && TREE_CODE (TREE_VALUE (p)) == LABEL_DECL)
      IDENTIFIER_LABEL_VALUE (TREE_PURPOSE (p)) = TREE_VALUE (p);
    else
      IDENTIFIER_SYMBOL_VALUE (TREE_PURPOSE (p)) = TREE_VALUE (p);

  /* Restore all tag-meanings from outer scopes that were shadowed by
     this scope.  */
  for (p = scope->shadowed_tags; p; p = TREE_CHAIN (p))
    IDENTIFIER_TAG_VALUE (TREE_PURPOSE (p)) = TREE_VALUE (p);

  /* Dispose of the block that we just made inside some higher level.  */
  if (scope->function_body && current_function_decl)
    DECL_INITIAL (current_function_decl) = block;
  else if (scope->outer)
    {
      if (block)
	SCOPE_LIST_APPEND (scope->outer, blocks, block);
      /* If we did not make a block for the scope just exited, any
	 blocks made for inner scopes must be carried forward so they
	 will later become subblocks of something else.  */
      else if (scope->blocks)
	SCOPE_LIST_CONCAT (scope->outer, blocks, scope, blocks);
    }

  /* Pop the current scope, and free the structure for reuse.  */
  pop_scope ();

  return block;
}

/* Insert BLOCK at the end of the list of subblocks of the current
   scope.  This is used when a BIND_EXPR is expanded, to handle the
   BLOCK node inside the BIND_EXPR.  */

void
insert_block (tree block)
{
  TREE_USED (block) = 1;
  SCOPE_LIST_APPEND (current_scope, blocks, block);
}

/* Set the BLOCK node for the innermost scope (the one we are
   currently in).  The RTL expansion machinery requires us to provide
   this hook, but it is not useful in function-at-a-time mode.  */

void
set_block (tree block ATTRIBUTE_UNUSED)
{
}

/* Push a definition or a declaration of struct, union or enum tag "name".
   "type" should be the type node.
   We assume that the tag "name" is not already defined.

   Note that the definition may really be just a forward reference.
   In that case, the TYPE_SIZE will be zero.  */

void
pushtag (tree name, tree type)
{
  struct c_scope *b = current_scope;

  /* Record the identifier as the type's name if it has none.  */
  if (name)
    {
      if (TYPE_NAME (type) == 0)
	TYPE_NAME (type) = name;

      if (IDENTIFIER_TAG_VALUE (name))
	b->shadowed_tags = tree_cons (name, IDENTIFIER_TAG_VALUE (name),
				      b->shadowed_tags);
      IDENTIFIER_TAG_VALUE (name) = type;
    }

  b->tags = tree_cons (name, type, b->tags);

  /* Create a fake NULL-named TYPE_DECL node whose TREE_TYPE will be the
     tagged type we just added to the current scope.  This fake
     NULL-named TYPE_DECL node helps dwarfout.c to know when it needs
     to output a representation of a tagged type, and it also gives
     us a convenient place to record the "scope start" address for the
     tagged type.  */

  TYPE_STUB_DECL (type) = pushdecl (build_decl (TYPE_DECL, NULL_TREE, type));

  /* An approximation for now, so we can tell this is a function-scope tag.
     This will be updated in poplevel.  */
  TYPE_CONTEXT (type) = DECL_CONTEXT (TYPE_STUB_DECL (type));
}

/* Subroutine of compare_decls.  Allow harmless mismatches in return
   and argument types provided that the type modes match.  This function
   return a unified type given a suitable match, and 0 otherwise.  */

static tree
match_builtin_function_types (tree newtype, tree oldtype)
{
  tree newrettype, oldrettype;
  tree newargs, oldargs;
  tree trytype, tryargs;

  /* Accept the return type of the new declaration if same modes.  */
  oldrettype = TREE_TYPE (oldtype);
  newrettype = TREE_TYPE (newtype);

  if (TYPE_MODE (oldrettype) != TYPE_MODE (newrettype))
    return 0;

  oldargs = TYPE_ARG_TYPES (oldtype);
  newargs = TYPE_ARG_TYPES (newtype);
  tryargs = newargs;

  while (oldargs || newargs)
    {
      if (! oldargs
	  || ! newargs
	  || ! TREE_VALUE (oldargs)
	  || ! TREE_VALUE (newargs)
	  || TYPE_MODE (TREE_VALUE (oldargs))
	     != TYPE_MODE (TREE_VALUE (newargs)))
	return 0;

      oldargs = TREE_CHAIN (oldargs);
      newargs = TREE_CHAIN (newargs);
    }

  trytype = build_function_type (newrettype, tryargs);
  return build_type_attribute_variant (trytype, TYPE_ATTRIBUTES (oldtype));
}

/* Subroutine of diagnose_mismathed_decls.  Check for function type
   mismatch involving an empty arglist vs a nonempty one and give clearer
   diagnostics. */
static void
diagnose_arglist_conflict (tree newdecl, tree olddecl,
			   tree newtype, tree oldtype)
{
  tree t;

  if (TREE_CODE (olddecl) != FUNCTION_DECL
      || !comptypes (TREE_TYPE (oldtype), TREE_TYPE (newtype), COMPARE_STRICT)
      || !((TYPE_ARG_TYPES (oldtype) == 0 && DECL_INITIAL (olddecl) == 0)
	   ||
	   (TYPE_ARG_TYPES (newtype) == 0 && DECL_INITIAL (newdecl) == 0)))
    return;

  t = TYPE_ARG_TYPES (oldtype);
  if (t == 0)
    t = TYPE_ARG_TYPES (newtype);
  for (; t; t = TREE_CHAIN (t))
    {
      tree type = TREE_VALUE (t);

      if (TREE_CHAIN (t) == 0
	  && TYPE_MAIN_VARIANT (type) != void_type_node)
	{
	  inform ("a parameter list with an ellipsis can't match "
		  "an empty parameter name list declaration");
	  break;
	}

      if (c_type_promotes_to (type) != type)
	{
	  inform ("an argument type that has a default promotion can't match "
		  "an empty parameter name list declaration");
	  break;
	}
    }
}

/* Another subroutine of diagnose_mismatched_decls.  OLDDECL is an
   old-style function definition, NEWDECL is a prototype declaration.
   Diagnose inconsistencies in the argument list.  Returns TRUE if
   the prototype is compatible, FALSE if not.  */
static bool
validate_proto_after_old_defn (tree newdecl, tree newtype, tree oldtype)
{
  tree newargs, oldargs;
  int i;

  /* ??? Elsewhere TYPE_MAIN_VARIANT is not used in this context.  */
#define END_OF_ARGLIST(t) (TYPE_MAIN_VARIANT (t) == void_type_node)

  oldargs = TYPE_ACTUAL_ARG_TYPES (oldtype);
  newargs = TYPE_ARG_TYPES (newtype);
  i = 1;

  for (;;)
    {
      tree oldargtype = TREE_VALUE (oldargs);
      tree newargtype = TREE_VALUE (newargs);

      if (END_OF_ARGLIST (oldargtype) && END_OF_ARGLIST (newargtype))
	break;

      /* Reaching the end of just one list means the two decls don't
	 agree on the number of arguments.  */
      if (END_OF_ARGLIST (oldargtype))
	{
	  error ("%Jprototype for '%D' declares more arguments "
		 "than previous old-style definition", newdecl, newdecl);
	  return false;
	}
      else if (END_OF_ARGLIST (newargtype))
	{
	  error ("%Jprototype for '%D' declares fewer arguments "
		 "than previous old-style definition", newdecl, newdecl);
	  return false;
	}

      /* Type for passing arg must be consistent with that declared
	 for the arg.  */
      else if (! comptypes (oldargtype, newargtype, COMPARE_STRICT))
	{
	  error ("%Jprototype for '%D' declares arg %d with incompatible type",
		 newdecl, newdecl, i);
	  return false;
	}

      oldargs = TREE_CHAIN (oldargs);
      newargs = TREE_CHAIN (newargs);
      i++;
    }

  /* If we get here, no errors were found, but do issue a warning
     for this poor-style construct.  */
  warning ("%Jprototype for '%D' follows non-prototype definition",
	   newdecl, newdecl);
  return true;
#undef END_OF_ARGLIST
}

/* Subroutine of diagnose_mismatched_decls.  Report the location of DECL,
   first in a pair of mismatched declarations, using the diagnostic
   function DIAG.  */
static void
locate_old_decl (tree decl, void (*diag)(const char *, ...))
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_BUILT_IN (decl))
    ;
  else if (DECL_INITIAL (decl))
    diag (N_("%Jprevious definition of '%D' was here"), decl, decl);
  else if (C_DECL_IMPLICIT (decl))
    diag (N_("%Jprevious implicit declaration of '%D' was here"), decl, decl);
  else
    diag (N_("%Jprevious declaration of '%D' was here"), decl, decl);
}

/* Subroutine of duplicate_decls.  Compare NEWDECL to OLDDECL.
   Returns true if the caller should proceed to merge the two, false
   if OLDDECL should simply be discarded.  As a side effect, issues
   all necessary diagnostics for invalid or poor-style combinations.
   If it returns true, writes the types of NEWDECL and OLDDECL to
   *NEWTYPEP and *OLDTYPEP - these may have been adjusted from
   TREE_TYPE (NEWDECL, OLDDECL) respectively.  */

static bool
diagnose_mismatched_decls (tree newdecl, tree olddecl,
			   tree *newtypep, tree *oldtypep)
{
  tree newtype, oldtype;
  bool pedwarned = false;
  bool warned = false;

  /* If we have error_mark_node for either decl or type, just discard
     the previous decl - we're in an error cascade already.  */
  if (olddecl == error_mark_node || newdecl == error_mark_node)
    return false;
  *oldtypep = oldtype = TREE_TYPE (olddecl);
  *newtypep = newtype = TREE_TYPE (newdecl);
  if (oldtype == error_mark_node || newtype == error_mark_node)
    return false;

  /* Two different categories of symbol altogether.  This is an error
     unless OLDDECL is a builtin.  OLDDECL will be discarded in any case.  */
  if (TREE_CODE (olddecl) != TREE_CODE (newdecl))
    {
      if (TREE_CODE (olddecl) != FUNCTION_DECL
          || !DECL_BUILT_IN (olddecl) || !C_DECL_INVISIBLE (olddecl))
	{
	  error ("%J'%D' redeclared as different kind of symbol",
		 newdecl, newdecl);
	  locate_old_decl (olddecl, error);
	}
      else if (TREE_PUBLIC (newdecl))
	warning ("%Jbuilt-in function '%D' declared as non-function",
		 newdecl, newdecl);
      else if (warn_shadow)
	warning ("%Jshadowing built-in function '%D'",
		 newdecl, newdecl);
      return false;
    }

  if (!comptypes (oldtype, newtype, COMPARE_STRICT))
    {
      if (TREE_CODE (olddecl) == FUNCTION_DECL
	  && DECL_BUILT_IN (olddecl) && C_DECL_INVISIBLE (olddecl))
	{
	  /* Accept harmless mismatch in function types.
	     This is for the ffs and fprintf builtins.  */
	  tree trytype = match_builtin_function_types (newtype, oldtype);

	  if (trytype && comptypes (newtype, trytype, COMPARE_STRICT))
	    *oldtypep = oldtype = trytype;
	  else
	    {
	      /* If types don't match for a built-in, throw away the
		 built-in.  No point in calling locate_old_decl here, it
		 won't print anything. */
	      warning ("%Jconflicting types for built-in function '%D'",
		       newdecl, newdecl);
	      return false;
	    }
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_SOURCE_LINE (olddecl) == 0)
	{
	  /* A conflicting function declaration for a predeclared
	     function that isn't actually built in.  Objective C uses
	     these.  The new declaration silently overrides everything
	     but the volatility (i.e. noreturn) indication.  See also
	     below.  FIXME: Make Objective C use normal builtins.  */
	  TREE_THIS_VOLATILE (newdecl) |= TREE_THIS_VOLATILE (olddecl);
	  return false;
	}
      /* Permit void foo (...) to match int foo (...) if the latter is
	 the definition and implicit int was used.  See
	 c-torture/compile/920625-2.c.  */
      else if (TREE_CODE (newdecl) == FUNCTION_DECL && DECL_INITIAL (newdecl)
	       && TYPE_MAIN_VARIANT (TREE_TYPE (oldtype)) == void_type_node
	       && TYPE_MAIN_VARIANT (TREE_TYPE (newtype)) == integer_type_node
	       && C_FUNCTION_IMPLICIT_INT (newdecl))
	{
	  pedwarn ("%Jconflicting types for '%D'", newdecl, newdecl);
	  /* Make sure we keep void as the return type.  */
	  TREE_TYPE (newdecl) = *newtypep = newtype = oldtype;
	  C_FUNCTION_IMPLICIT_INT (newdecl) = 0;
	  pedwarned = true;
	}
      else
	{
	  error ("%Jconflicting types for '%D'", newdecl, newdecl);
	  diagnose_arglist_conflict (newdecl, olddecl, newtype, oldtype);
	  locate_old_decl (olddecl, error);
	  return false;
	}
    }

  /* Redeclaration of a type is a constraint violation (6.7.2.3p1),
     but silently ignore the redeclaration if either is in a system
     header.  (Conflicting redeclarations were handled above.)  */
  if (TREE_CODE (newdecl) == TYPE_DECL)
    {
      if (DECL_IN_SYSTEM_HEADER (newdecl) || DECL_IN_SYSTEM_HEADER (olddecl))
	return true;  /* allow OLDDECL to continue in use */
      
      error ("%Jredefinition of typedef '%D'", newdecl, newdecl);
      locate_old_decl (olddecl, error);
      return false;
    }

  /* Function declarations can either be 'static' or 'extern' (no
     qualifier is equivalent to 'extern' - C99 6.2.2p5) and therefore
     can never conflict with each other on account of linkage (6.2.2p4).
     Multiple definitions are not allowed (6.9p3,5) but GCC permits
     two definitions if one is 'extern inline' and one is not.  The non-
     extern-inline definition supersedes the extern-inline definition.  */
  else if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* If you declare a built-in function name as static, or
	 define the built-in with an old-style definition (so we
	 can't validate the argument list) the built-in definition is
	 overridden, but optionally warn this was a bad choice of name.  */
      if (DECL_BUILT_IN (olddecl)
	  && C_DECL_INVISIBLE (olddecl)
	  && (!TREE_PUBLIC (newdecl)
	      || (DECL_INITIAL (newdecl)
		  && !TYPE_ARG_TYPES (TREE_TYPE (newdecl)))))
	{
	  if (warn_shadow)
	    warning ("%Jshadowing built-in function '%D'", newdecl, newdecl);
	  /* Discard the old built-in function.  */
	  return false;
	}
      
      if (DECL_INITIAL (newdecl))
	{
	  if (DECL_INITIAL (olddecl)
	      && !(DECL_DECLARED_INLINE_P (olddecl)
		   && DECL_EXTERNAL (olddecl)
		   && !(DECL_DECLARED_INLINE_P (newdecl)
			&& DECL_EXTERNAL (newdecl))))
	    {
	      error ("%Jredefinition of '%D'", newdecl, newdecl);
	      locate_old_decl (olddecl, error);
	      return false;
	    }
	}
      /* If we have a prototype after an old-style function definition,
	 the argument types must be checked specially.  */
      else if (DECL_INITIAL (olddecl)
	       && !TYPE_ARG_TYPES (oldtype) && TYPE_ARG_TYPES (newtype)
	       && TYPE_ACTUAL_ARG_TYPES (oldtype)
	       && !validate_proto_after_old_defn (newdecl, newtype, oldtype))
	{
	  locate_old_decl (olddecl, error);
	  return false;
	}
      /* Mismatched non-static and static is considered poor style.
         We only diagnose static then non-static if -Wtraditional,
	 because it is the most convenient way to get some effects
	 (see e.g.  what unwind-dw2-fde-glibc.c does to the definition
	 of _Unwind_Find_FDE in unwind-dw2-fde.c).  Revisit?  */
      if (TREE_PUBLIC (olddecl) && !TREE_PUBLIC (newdecl))
	{
	  /* A static function declaration for a predeclared function
	     that isn't actually built in, silently overrides the
	     default.  Objective C uses these.  See also above.
	     FIXME: Make Objective C use normal builtins.  */
	  if (TREE_CODE (olddecl) == FUNCTION_DECL
	      && DECL_SOURCE_LINE (olddecl) == 0)
	    return false;
	  else
	    {
	      warning ("%Jstatic declaration of '%D' follows "
		       "non-static declaration", newdecl, newdecl);
	      warned = true;
	    }
	}
      else if (TREE_PUBLIC (newdecl) && !TREE_PUBLIC (olddecl)
	       && warn_traditional)
	{
	  warning ("%Jnon-static declaration of '%D' follows "
		   "static declaration", newdecl, newdecl);
	  warned = true;
	}
    }
  else if (TREE_CODE (newdecl) == VAR_DECL)
    {
      /* Only variables can be thread-local, and all declarations must
	 agree on this property.  */
      if (DECL_THREAD_LOCAL (newdecl) != DECL_THREAD_LOCAL (olddecl))
	{
	  if (DECL_THREAD_LOCAL (newdecl))
	    error ("%Jthread-local declaration of '%D' follows "
		   "non-thread-local declaration", newdecl, newdecl);
	  else
	    error ("%Jnon-thread-local declaration of '%D' follows "
		   "thread-local declaration", newdecl, newdecl);

	  locate_old_decl (olddecl, error);
	  return false;
	}

      /* Multiple initialized definitions are not allowed (6.9p3,5).  */
      if (DECL_INITIAL (newdecl) && DECL_INITIAL (olddecl))
	{
	  error ("%Jredefinition of '%D'", newdecl, newdecl);
	  locate_old_decl (olddecl, error);
	  return false;
	}

      /* Objects declared at file scope: if at least one is 'extern',
	 it's fine (6.2.2p4); otherwise the linkage must agree (6.2.2p7).  */
      if (DECL_FILE_SCOPE_P (newdecl))
	{
	  if (!DECL_EXTERNAL (newdecl)
	      && !DECL_EXTERNAL (olddecl)
	      && TREE_PUBLIC (newdecl) != TREE_PUBLIC (olddecl))
	    {
	      if (TREE_PUBLIC (newdecl))
		error ("%Jnon-static declaration of '%D' follows "
		       "static declaration", newdecl, newdecl);
	      else
		error ("%Jstatic declaration of '%D' follows "
		       "non-static declaration", newdecl, newdecl);

	      locate_old_decl (olddecl, error);
	      return false;
	    }
	}
      /* Two objects with the same name declared at the same block
	 scope must both be external references (6.7p3).  */
      else if (DECL_CONTEXT (newdecl) == DECL_CONTEXT (olddecl)
	       && (!DECL_EXTERNAL (newdecl) || !DECL_EXTERNAL (olddecl)))
	{
	  if (DECL_EXTERNAL (newdecl))
	    error ("%Jextern declaration of '%D' follows "
		   "declaration with no linkage", newdecl, newdecl);
	  else if (DECL_EXTERNAL (olddecl))
	    error ("%Jdeclaration of '%D' with no linkage follows "
		   "extern declaration", newdecl, newdecl);
	  else
	    error ("%Jredeclaration of '%D' with no linkage",
		   newdecl, newdecl);

	  locate_old_decl (olddecl, error);
	  return false;
	}
    }

  /* warnings */
  /* All decls must agree on a non-default visibility.  */
  if (DECL_VISIBILITY (newdecl) != VISIBILITY_DEFAULT
      && DECL_VISIBILITY (olddecl) != VISIBILITY_DEFAULT
      && DECL_VISIBILITY (newdecl) != DECL_VISIBILITY (olddecl))
    {
      warning ("%Jredeclaration of '%D' with different visibility "
	       "(old visibility preserved)", newdecl, newdecl);
      warned = true;
    }

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* Diagnose inline __attribute__ ((noinline)) which is silly.  */
      if (DECL_DECLARED_INLINE_P (newdecl)
	  && lookup_attribute ("noinline", DECL_ATTRIBUTES (olddecl)))
	{
	  warning ("%Jinline declaration of '%D' follows "
		   "declaration with attribute noinline", newdecl, newdecl);
	  warned = true;
	}
      else if (DECL_DECLARED_INLINE_P (olddecl)
	       && lookup_attribute ("noinline", DECL_ATTRIBUTES (newdecl)))
	{
	  warning ("%Jdeclaration of '%D' with attribute noinline follows "
		   "inline declaration ", newdecl, newdecl);
	  warned = true;
	}

      /* Inline declaration after use or definition.
	 ??? Should we still warn about this now we have unit-at-a-time
	 mode and can get it right?  */
      if (DECL_DECLARED_INLINE_P (newdecl) && !DECL_DECLARED_INLINE_P (olddecl))
	{
	  if (TREE_USED (olddecl))
	    {
	      warning ("%J'%D' declared inline after being called",
		       olddecl, olddecl);
	      warned = true;
	    }
	  else if (DECL_INITIAL (olddecl))
	    {
	      warning ("%J'%D' declared inline after its definition",
		       olddecl, olddecl);
	      warned = true;
	    }
	}
    }
  else /* PARM_DECL, VAR_DECL */
    {
      /* Redeclaration of a PARM_DECL is invalid unless this is the
	 real position of a forward-declared parameter (GCC extension).  */
      if (TREE_CODE (newdecl) == PARM_DECL
	  && (!TREE_ASM_WRITTEN (olddecl) || TREE_ASM_WRITTEN (newdecl)))
	{
	  error ("%Jredefinition of parameter '%D'", newdecl, newdecl);
	  locate_old_decl (olddecl, error);
	  return false;
	}

      /* These bits are only type qualifiers when applied to objects.  */
      if (TREE_THIS_VOLATILE (newdecl) != TREE_THIS_VOLATILE (olddecl))
	{
	  if (TREE_THIS_VOLATILE (newdecl))
	    pedwarn ("%Jvolatile declaration of '%D' follows "
		     "non-volatile declaration", newdecl, newdecl);
	  else
	    pedwarn ("%Jnon-volatile declaration of '%D' follows "
		     "volatile declaration", newdecl, newdecl);
	  pedwarned = true;
	}
      if (TREE_READONLY (newdecl) != TREE_READONLY (olddecl))
	{
	  if (TREE_READONLY (newdecl))
	    pedwarn ("%Jconst declaration of '%D' follows "
		     "non-const declaration", newdecl, newdecl);
	  else
	    pedwarn ("%Jnon-const declaration of '%D' follows "
		     "const declaration", newdecl, newdecl);
	  pedwarned = true;
	}
    }

  /* Optional warning for completely redundant decls.  */
  if (!warned && !pedwarned
      && warn_redundant_decls
      /* Don't warn about a function declaration followed by a
	 definition.  */
      && !(TREE_CODE (newdecl) == FUNCTION_DECL
	   && DECL_INITIAL (newdecl) && !DECL_INITIAL (olddecl))
      /* Don't warn about redundant redeclarations of builtins. */
      && !(TREE_CODE (newdecl) == FUNCTION_DECL
	   && !DECL_BUILT_IN (newdecl)
	   && DECL_BUILT_IN (olddecl)
	   && C_DECL_INVISIBLE (olddecl))
      /* Don't warn about an extern followed by a definition.  */
      && !(DECL_EXTERNAL (olddecl) && !DECL_EXTERNAL (newdecl))
      /* Don't warn about forward parameter decls.  */
      && !(TREE_CODE (newdecl) == PARM_DECL
	   && TREE_ASM_WRITTEN (olddecl) && !TREE_ASM_WRITTEN (newdecl)))
    {
      warning ("%Jredundant redeclaration of '%D'", newdecl, newdecl);
      warned = true;
    }

  /* Report location of previous decl/defn in a consistent manner.  */
  if (warned || pedwarned)
    locate_old_decl (olddecl, pedwarned ? pedwarn : warning);

  return true;
}

/* Subroutine of duplicate_decls.  NEWDECL has been found to be
   consistent with OLDDECL, but carries new information.  Merge the
   new information into OLDDECL.  This function issues no
   diagnostics.  */

static void
merge_decls (tree newdecl, tree olddecl, tree newtype, tree oldtype)
{
  int new_is_definition = (TREE_CODE (newdecl) == FUNCTION_DECL
			   && DECL_INITIAL (newdecl) != 0);

  /* For real parm decl following a forward decl, return 1 so old decl
     will be reused.  Only allow this to happen once.  */
  if (TREE_CODE (newdecl) == PARM_DECL
      && TREE_ASM_WRITTEN (olddecl) && ! TREE_ASM_WRITTEN (newdecl))
    {
      TREE_ASM_WRITTEN (olddecl) = 0;
      return;
    }

  DECL_ATTRIBUTES (newdecl)
    = (*targetm.merge_decl_attributes) (olddecl, newdecl);

  /* Merge the data types specified in the two decls.  */
  TREE_TYPE (newdecl)
    = TREE_TYPE (olddecl)
    = common_type (newtype, oldtype);

  /* Lay the type out, unless already done.  */
  if (oldtype != TREE_TYPE (newdecl))
    {
      if (TREE_TYPE (newdecl) != error_mark_node)
	layout_type (TREE_TYPE (newdecl));
      if (TREE_CODE (newdecl) != FUNCTION_DECL
	  && TREE_CODE (newdecl) != TYPE_DECL
	  && TREE_CODE (newdecl) != CONST_DECL)
	layout_decl (newdecl, 0);
    }
  else
    {
      /* Since the type is OLDDECL's, make OLDDECL's size go with.  */
      DECL_SIZE (newdecl) = DECL_SIZE (olddecl);
      DECL_SIZE_UNIT (newdecl) = DECL_SIZE_UNIT (olddecl);
      DECL_MODE (newdecl) = DECL_MODE (olddecl);
      if (TREE_CODE (olddecl) != FUNCTION_DECL)
	if (DECL_ALIGN (olddecl) > DECL_ALIGN (newdecl))
	  {
	    DECL_ALIGN (newdecl) = DECL_ALIGN (olddecl);
	    DECL_USER_ALIGN (newdecl) |= DECL_ALIGN (olddecl);
	  }
    }

  /* Keep the old rtl since we can safely use it.  */
  COPY_DECL_RTL (olddecl, newdecl);

  /* Merge the type qualifiers.  */
  if (TREE_READONLY (newdecl))
    TREE_READONLY (olddecl) = 1;

  if (TREE_THIS_VOLATILE (newdecl))
    {
      TREE_THIS_VOLATILE (olddecl) = 1;
      if (TREE_CODE (newdecl) == VAR_DECL)
	make_var_volatile (newdecl);
    }

  /* Keep source location of definition rather than declaration.  */
  if (DECL_INITIAL (newdecl) == 0 && DECL_INITIAL (olddecl) != 0)
    DECL_SOURCE_LOCATION (newdecl) = DECL_SOURCE_LOCATION (olddecl);

  /* Merge the unused-warning information.  */
  if (DECL_IN_SYSTEM_HEADER (olddecl))
    DECL_IN_SYSTEM_HEADER (newdecl) = 1;
  else if (DECL_IN_SYSTEM_HEADER (newdecl))
    DECL_IN_SYSTEM_HEADER (olddecl) = 1;

  /* Merge the initialization information.  */
   if (DECL_INITIAL (newdecl) == 0)
    DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);

  /* Merge the section attribute.
     We want to issue an error if the sections conflict but that must be
     done later in decl_attributes since we are called before attributes
     are assigned.  */
  if (DECL_SECTION_NAME (newdecl) == NULL_TREE)
    DECL_SECTION_NAME (newdecl) = DECL_SECTION_NAME (olddecl);

  /* Copy the assembler name.
     Currently, it can only be defined in the prototype.  */
  COPY_DECL_ASSEMBLER_NAME (olddecl, newdecl);

  /* If either declaration has a nondefault visibility, use it.  */
  if (DECL_VISIBILITY (olddecl) != VISIBILITY_DEFAULT)
    DECL_VISIBILITY (newdecl) = DECL_VISIBILITY (olddecl);

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      DECL_STATIC_CONSTRUCTOR(newdecl) |= DECL_STATIC_CONSTRUCTOR(olddecl);
      DECL_STATIC_DESTRUCTOR (newdecl) |= DECL_STATIC_DESTRUCTOR (olddecl);
      DECL_NO_LIMIT_STACK (newdecl) |= DECL_NO_LIMIT_STACK (olddecl);
      DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (newdecl)
	|= DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (olddecl);
      TREE_THIS_VOLATILE (newdecl) |= TREE_THIS_VOLATILE (olddecl);
      TREE_READONLY (newdecl) |= TREE_READONLY (olddecl);
      DECL_IS_MALLOC (newdecl) |= DECL_IS_MALLOC (olddecl);
      DECL_IS_PURE (newdecl) |= DECL_IS_PURE (olddecl);
    }

  /* Merge the storage class information.  */
  merge_weak (newdecl, olddecl);

  /* For functions, static overrides non-static.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      TREE_PUBLIC (newdecl) &= TREE_PUBLIC (olddecl);
      /* This is since we don't automatically
	 copy the attributes of NEWDECL into OLDDECL.  */
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
      /* If this clears `static', clear it in the identifier too.  */
      if (! TREE_PUBLIC (olddecl))
	TREE_PUBLIC (DECL_NAME (olddecl)) = 0;
    }
  if (DECL_EXTERNAL (newdecl))
    {
      TREE_STATIC (newdecl) = TREE_STATIC (olddecl);
      DECL_EXTERNAL (newdecl) = DECL_EXTERNAL (olddecl);

      /* An extern decl does not override previous storage class.  */
      TREE_PUBLIC (newdecl) = TREE_PUBLIC (olddecl);
      if (! DECL_EXTERNAL (newdecl))
	{
	  DECL_CONTEXT (newdecl) = DECL_CONTEXT (olddecl);
	  DECL_COMMON (newdecl) = DECL_COMMON (olddecl);
	}
    }
  else
    {
      TREE_STATIC (olddecl) = TREE_STATIC (newdecl);
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
    }

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* If we're redefining a function previously defined as extern
	 inline, make sure we emit debug info for the inline before we
	 throw it away, in case it was inlined into a function that hasn't
	 been written out yet.  */
      if (new_is_definition && DECL_INITIAL (olddecl))
	{
	  if (TREE_USED (olddecl)
	      /* In unit-at-a-time mode we never inline re-defined extern
	         inline functions.  */
	      && !flag_unit_at_a_time
	      && cgraph_function_possibly_inlined_p (olddecl))
	    (*debug_hooks->outlining_inline_function) (olddecl);

	  /* The new defn must not be inline.  */
	  DECL_INLINE (newdecl) = 0;
	  DECL_UNINLINABLE (newdecl) = 1;
	}
      else
	{
	  /* If either decl says `inline', this fn is inline,
	     unless its definition was passed already.  */
	  if (DECL_DECLARED_INLINE_P (newdecl)
	      || DECL_DECLARED_INLINE_P (olddecl))
	    DECL_DECLARED_INLINE_P (newdecl) = 1;

	  DECL_UNINLINABLE (newdecl) = DECL_UNINLINABLE (olddecl)
	    = (DECL_UNINLINABLE (newdecl) || DECL_UNINLINABLE (olddecl));
	}

      if (DECL_BUILT_IN (olddecl))
	{
	  /* If redeclaring a builtin function, it stays built in.  */
	  DECL_BUILT_IN_CLASS (newdecl) = DECL_BUILT_IN_CLASS (olddecl);
	  DECL_FUNCTION_CODE (newdecl) = DECL_FUNCTION_CODE (olddecl);
	}

      /* Also preserve various other info from the definition.  */
      if (! new_is_definition)
	{
	  DECL_RESULT (newdecl) = DECL_RESULT (olddecl);
	  DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
	  DECL_SAVED_INSNS (newdecl) = DECL_SAVED_INSNS (olddecl);
	  DECL_SAVED_TREE (newdecl) = DECL_SAVED_TREE (olddecl);
	  DECL_ARGUMENTS (newdecl) = DECL_ARGUMENTS (olddecl);

	  /* Set DECL_INLINE on the declaration if we've got a body
	     from which to instantiate.  */
	  if (DECL_INLINE (olddecl) && ! DECL_UNINLINABLE (newdecl))
	    {
	      DECL_INLINE (newdecl) = 1;
	      DECL_ABSTRACT_ORIGIN (newdecl)
		= DECL_ABSTRACT_ORIGIN (olddecl);
	    }
	}
      else
	{
	  /* If a previous declaration said inline, mark the
	     definition as inlinable.  */
	  if (DECL_DECLARED_INLINE_P (newdecl)
	      && ! DECL_UNINLINABLE (newdecl))
	    DECL_INLINE (newdecl) = 1;
	}
    }

  /* Copy most of the decl-specific fields of NEWDECL into OLDDECL.
     But preserve OLDDECL's DECL_UID and C_DECL_INVISIBLE.  */
  {
    unsigned olddecl_uid = DECL_UID (olddecl);
    unsigned olddecl_invisible = C_DECL_INVISIBLE (olddecl);

    memcpy ((char *) olddecl + sizeof (struct tree_common),
	    (char *) newdecl + sizeof (struct tree_common),
	    sizeof (struct tree_decl) - sizeof (struct tree_common));
    DECL_UID (olddecl) = olddecl_uid;
    C_DECL_INVISIBLE (olddecl) = olddecl_invisible;
  }

  /* If OLDDECL had its DECL_RTL instantiated, re-invoke make_decl_rtl
     so that encode_section_info has a chance to look at the new decl
     flags and attributes.  */
  if (DECL_RTL_SET_P (olddecl)
      && (TREE_CODE (olddecl) == FUNCTION_DECL
	  || (TREE_CODE (olddecl) == VAR_DECL
	      && TREE_STATIC (olddecl))))
    make_decl_rtl (olddecl, NULL);
}

/* Handle when a new declaration NEWDECL has the same name as an old
   one OLDDECL in the same binding contour.  Prints an error message
   if appropriate.

   If safely possible, alter OLDDECL to look like NEWDECL, and return
   true.  Otherwise, return false.  */

static bool
duplicate_decls (tree newdecl, tree olddecl)
{
  tree newtype, oldtype;

  if (!diagnose_mismatched_decls (newdecl, olddecl, &newtype, &oldtype))
    return false;

  merge_decls (newdecl, olddecl, newtype, oldtype);
  return true;
}
  

/* Return any external DECL associated with ID, whether or not it is
   currently in scope.  */

static tree
any_external_decl (tree id)
{
  tree decl = IDENTIFIER_SYMBOL_VALUE (id);
  tree t;

  if (decl == 0 || TREE_CODE (decl) == ERROR_MARK)
    return 0;
  else if (TREE_CODE (decl) != TYPE_DECL && DECL_EXTERNAL (decl))
    return decl;

  t = purpose_member (id, truly_local_externals);
  if (t)
    return TREE_VALUE (t);

  return 0;
}

/* Record an external decl DECL.  This only does something if a
   shadowing decl already exists.  */
static void
record_external_decl (tree decl)
{
  tree name = DECL_NAME (decl);
  if (!IDENTIFIER_SYMBOL_VALUE (name))
    return;

  truly_local_externals = tree_cons (name, decl, truly_local_externals);
}

/* Check whether decl-node X shadows an existing declaration.
   OLD is the old IDENTIFIER_SYMBOL_VALUE of the DECL_NAME of X,
   which might be a NULL_TREE.  */
static void
warn_if_shadowing (tree x, tree old)
{
  /* Nothing to shadow?  */
  if (old == 0
      /* Shadow warnings not wanted?  */
      || !warn_shadow
      /* No shadow warnings for internally generated vars.  */
      || DECL_SOURCE_LINE (x) == 0
      /* No shadow warnings for vars made for inlining.  */
      || DECL_FROM_INLINE (x)
      /* Don't warn about the parm names in function declarator
	 within a function declarator.
	 It would be nice to avoid warning in any function
	 declarator in a declaration, as opposed to a definition,
	 but there is no way to tell it's not a definition.  */
      || (TREE_CODE (x) == PARM_DECL && current_scope->outer->parm_flag)
      /* Shadow warnings only apply to local variables and parameters.  */
      || (TREE_CODE (x) != PARM_DECL && DECL_FILE_SCOPE_P (x)))
    return;

  if (TREE_CODE (old) == PARM_DECL)
    warning ("%Jdeclaration of '%D' shadows a parameter", x, x);
  else if (DECL_FILE_SCOPE_P (old))
    warning ("%Jdeclaration of '%D' shadows a global declaration", x, x);
  else
    warning ("%Jdeclaration of '%D' shadows a previous local", x, x);

  warning ("%Jshadowed declaration is here", old);
}


/* Subroutine of pushdecl.

   X is a TYPE_DECL for a typedef statement.  Create a brand new
   ..._TYPE node (which will be just a variant of the existing
   ..._TYPE node with identical properties) and then install X
   as the TYPE_NAME of this brand new (duplicate) ..._TYPE node.

   The whole point here is to end up with a situation where each
   and every ..._TYPE node the compiler creates will be uniquely
   associated with AT MOST one node representing a typedef name.
   This way, even though the compiler substitutes corresponding
   ..._TYPE nodes for TYPE_DECL (i.e. "typedef name") nodes very
   early on, later parts of the compiler can always do the reverse
   translation and get back the corresponding typedef name.  For
   example, given:

        typedef struct S MY_TYPE;
	MY_TYPE object;

   Later parts of the compiler might only know that `object' was of
   type `struct S' if it were not for code just below.  With this
   code however, later parts of the compiler see something like:

	struct S' == struct S
	typedef struct S' MY_TYPE;
	struct S' object;

    And they can then deduce (from the node for type struct S') that
    the original object declaration was:

		MY_TYPE object;

    Being able to do this is important for proper support of protoize,
    and also for generating precise symbolic debugging information
    which takes full account of the programmer's (typedef) vocabulary.

    Obviously, we don't want to generate a duplicate ..._TYPE node if
    the TYPE_DECL node that we are now processing really represents a
    standard built-in type.

    Since all standard types are effectively declared at line zero
    in the source file, we can easily check to see if we are working
    on a standard type by checking the current value of lineno.  */

static void
clone_underlying_type (tree x)
{
  if (DECL_SOURCE_LINE (x) == 0)
    {
      if (TYPE_NAME (TREE_TYPE (x)) == 0)
	TYPE_NAME (TREE_TYPE (x)) = x;
    }
  else if (TREE_TYPE (x) != error_mark_node
	   && DECL_ORIGINAL_TYPE (x) == NULL_TREE)
    {
      tree tt = TREE_TYPE (x);
      DECL_ORIGINAL_TYPE (x) = tt;
      tt = build_type_copy (tt);
      TYPE_NAME (tt) = x;
      TREE_USED (tt) = TREE_USED (x);
      TREE_TYPE (x) = tt;
    }
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */

tree
pushdecl (tree x)
{
  tree name = DECL_NAME (x);
  struct c_scope *scope = current_scope;

#ifdef ENABLE_CHECKING
  if (error_mark_node == 0)
    /* Called too early.  */
    abort ();
#endif

  /* Functions need the lang_decl data.  */
  if (TREE_CODE (x) == FUNCTION_DECL && ! DECL_LANG_SPECIFIC (x))
    DECL_LANG_SPECIFIC (x) = ggc_alloc_cleared (sizeof (struct lang_decl));

  /* A local extern declaration for a function doesn't constitute nesting.
     A local auto declaration does, since it's a forward decl
     for a nested function coming later.  */
  if (current_function_decl == NULL
      || ((TREE_CODE (x) == FUNCTION_DECL || TREE_CODE (x) == VAR_DECL)
	  && DECL_INITIAL (x) == 0 && DECL_EXTERNAL (x)))
    DECL_CONTEXT (x) = current_file_decl;
  else
    DECL_CONTEXT (x) = current_function_decl;

  if (name)
    {
      tree old;

      if (warn_nested_externs
	  && scope != global_scope
	  && DECL_EXTERNAL (x)
	  && !DECL_IN_SYSTEM_HEADER (x))
	warning ("nested extern declaration of `%s'",
		 IDENTIFIER_POINTER (name));

      old = lookup_name_current_level (name);
      if (old && duplicate_decls (x, old))
	{
	  /* For PARM_DECLs, old may be a forward declaration.
	     If so, we want to remove it from its old location
	     (in the variables chain) and rechain it in the
	     location given by the new declaration.  */
	  if (TREE_CODE (x) == PARM_DECL)
	    {
	      tree *p;
	      for (p = &scope->names; *p; p = &TREE_CHAIN (*p))
		if (*p == old)
		  {
		    *p = TREE_CHAIN (old);
		    SCOPE_LIST_APPEND (scope, parms, old);
		    break;
		  }
	    }
	  return old;
	}
      if (DECL_EXTERNAL (x) || scope == global_scope)
	{
	  /* Find and check against a previous, not-in-scope, external
	     decl for this identifier.  (C99 6.2.7p2: All declarations
	     that refer to the same object or function shall have
	     compatible type; otherwise, the behavior is undefined.)  */
 	  tree ext = any_external_decl (name);
	  if (ext)
	    {
	      if (duplicate_decls (x, ext))
		x = copy_node (ext);
	    }
	  else
	    record_external_decl (x);
	}

      if (TREE_CODE (x) == TYPE_DECL)
	clone_underlying_type (x);

      /* If storing a local value, there may already be one
	 (inherited).  If so, record it for restoration when this
	 scope ends.  Take care not to do this if we are replacing an
	 older decl in the same scope (i.e.  duplicate_decls returned
	 false, above).  */
      if (scope != global_scope)
	{
	  tree inherited_decl = lookup_name (name);
	  if (inherited_decl && inherited_decl != old)
	    {
	      warn_if_shadowing (x, inherited_decl);
	      scope->shadowed = tree_cons (name, inherited_decl,
					   scope->shadowed);
	    }
	}

      /* Install the new declaration in the requested scope.  */
      IDENTIFIER_SYMBOL_VALUE (name) = x;
      C_DECL_INVISIBLE (x) = 0;

      /* If x's type is incomplete because it's based on a
	 structure or union which has not yet been fully declared,
	 attach it to that structure or union type, so we can go
	 back and complete the variable declaration later, if the
	 structure or union gets fully declared.

	 If the input is erroneous, we can have error_mark in the type
	 slot (e.g. "f(void a, ...)") - that doesn't count as an
	 incomplete type.  */
      if (TREE_TYPE (x) != error_mark_node
	  && !COMPLETE_TYPE_P (TREE_TYPE (x)))
	{
	  tree element = TREE_TYPE (x);

	  while (TREE_CODE (element) == ARRAY_TYPE)
	    element = TREE_TYPE (element);
	  element = TYPE_MAIN_VARIANT (element);

	  if ((TREE_CODE (element) == RECORD_TYPE
	       || TREE_CODE (element) == UNION_TYPE)
	      && (TREE_CODE (x) != TYPE_DECL
		  || TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE)
	      && !COMPLETE_TYPE_P (element))
	    C_TYPE_INCOMPLETE_VARS (element)
	      = tree_cons (NULL_TREE, x, C_TYPE_INCOMPLETE_VARS (element));
	}
    }

  if (TREE_CODE (x) == PARM_DECL)
    SCOPE_LIST_APPEND (scope, parms, x);
  else
    SCOPE_LIST_APPEND (scope, names, x);

  return x;
}

/* Record X as belonging to the global scope (C99 "file scope").
   This is used only internally by the Objective-C front end,
   and is limited to its needs.  duplicate_decls is not called;
   if there is any preexisting decl for this identifier, it is an ICE.  */

tree
pushdecl_top_level (tree x)
{
  tree name;

  if (TREE_CODE (x) != VAR_DECL)
    abort ();

  name = DECL_NAME (x);

  if (IDENTIFIER_SYMBOL_VALUE (name))
    abort ();

  DECL_CONTEXT (x) = current_file_decl;
  IDENTIFIER_SYMBOL_VALUE (name) = x;

  SCOPE_LIST_APPEND (global_scope, names, x);
  return x;
}

/* Generate an implicit declaration for identifier FUNCTIONID as a
   function of type int ().  */

tree
implicitly_declare (tree functionid)
{
  tree decl = any_external_decl (functionid);

  if (decl)
    {
      /* Implicit declaration of a function already declared
	 (somehow) in a different scope, or as a built-in.
	 If this is the first time this has happened, warn;
	 then recycle the old declaration.  */
      if (!C_DECL_IMPLICIT (decl))
	{
	  implicit_decl_warning (DECL_NAME (decl));
	  if (! DECL_FILE_SCOPE_P (decl))
	    warning ("%Jprevious declaration of '%D'", decl, decl);
	  C_DECL_IMPLICIT (decl) = 1;
	}
      /* If this function is global, then it must already be in the
	 global scope, so there's no need to push it again.  */
      if (current_scope == global_scope)
	return decl;
      /* If this is a local declaration, make a copy; we can't have
	 the same DECL listed in two different scopes.  */
      return pushdecl (copy_node (decl));
    }

  /* Not seen before.  */
  decl = build_decl (FUNCTION_DECL, functionid, default_function_type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  C_DECL_IMPLICIT (decl) = 1;
  implicit_decl_warning (functionid);

  /* C89 says implicit declarations are in the innermost block.
     So we record the decl in the standard fashion.  */
  decl = pushdecl (decl);

  /* No need to call objc_check_decl here - it's a function type.  */
  rest_of_decl_compilation (decl, NULL, 0, 0);

  /* Write a record describing this implicit function declaration
     to the prototypes file (if requested).  */
  gen_aux_info_record (decl, 0, 1, 0);

  /* Possibly apply some default attributes to this implicit declaration.  */
  decl_attributes (&decl, NULL_TREE, 0);

  return decl;
}

static void
implicit_decl_warning (tree id)
{
  const char *name = IDENTIFIER_POINTER (id);
  if (mesg_implicit_function_declaration == 2)
    error ("implicit declaration of function `%s'", name);
  else if (mesg_implicit_function_declaration == 1)
    warning ("implicit declaration of function `%s'", name);
}

/* Issue an error message for a reference to an undeclared variable
   ID, including a reference to a builtin outside of function-call
   context.  Establish a binding of the identifier to error_mark_node
   in an appropriate scope, which will suppress further errors for the
   same identifier.  */
void
undeclared_variable (tree id)
{
  static bool already = false;
  struct c_scope *scope;

  if (current_function_decl == 0)
    {
      error ("`%s' undeclared here (not in a function)",
	     IDENTIFIER_POINTER (id));
      scope = current_scope;
    }
  else
    {
      error ("`%s' undeclared (first use in this function)",
	     IDENTIFIER_POINTER (id));

      if (! already)
	{
	  error ("(Each undeclared identifier is reported only once");
	  error ("for each function it appears in.)");
	  already = true;
	}

      scope = current_function_scope;
    }

  scope->shadowed = tree_cons (id, IDENTIFIER_SYMBOL_VALUE (id),
			       scope->shadowed);
  IDENTIFIER_SYMBOL_VALUE (id) = error_mark_node;
}

/* Subroutine of lookup_label, declare_label, define_label: construct a
   LABEL_DECL with all the proper frills.  */

static tree
make_label (tree name, location_t location)
{
  tree label = build_decl (LABEL_DECL, name, void_type_node);

  DECL_CONTEXT (label) = current_function_decl;
  DECL_MODE (label) = VOIDmode;
  DECL_SOURCE_LOCATION (label) = location;

  return label;
}

/* Another subroutine of lookup_label, declare_label, define_label:
   set up the binding of name to LABEL_DECL in the given SCOPE.  */

static void
bind_label (tree name, tree label, struct c_scope *scope)
{
  if (IDENTIFIER_LABEL_VALUE (name))
    scope->shadowed = tree_cons (name, IDENTIFIER_LABEL_VALUE (name),
				 scope->shadowed);
  IDENTIFIER_LABEL_VALUE (name) = label;

  SCOPE_LIST_APPEND (scope, names, label);
}

/* Get the LABEL_DECL corresponding to identifier NAME as a label.
   Create one if none exists so far for the current function.
   This is called when a label is used in a goto expression or
   has its address taken.  */

tree
lookup_label (tree name)
{
  tree label;

  if (current_function_decl == 0)
    {
      error ("label %s referenced outside of any function",
	     IDENTIFIER_POINTER (name));
      return 0;
    }

  /* Use a label already defined or ref'd with this name, but not if
     it is inherited from a containing function and wasn't declared
     using __label__.  */
  label = IDENTIFIER_LABEL_VALUE (name);
  if (label && (DECL_CONTEXT (label) == current_function_decl
		|| C_DECLARED_LABEL_FLAG (label)))
    {
      /* If the label has only been declared, update its apparent
	 location to point here, for better diagnostics if it
	 turns out not to have been defined.  */
      if (!TREE_USED (label))
	DECL_SOURCE_LOCATION (label) = input_location;
      return label;
    }

  /* No label binding for that identifier; make one.  */
  label = make_label (name, input_location);

  /* Ordinary labels go in the current function scope.  */
  bind_label (name, label, current_function_scope);
  return label;
}

/* Make a label named NAME in the current function, shadowing silently
   any that may be inherited from containing functions or containing
   scopes.  This is called for __label__ declarations.  */

/* Note that valid use, if the label being shadowed comes from another
   scope in the same function, requires calling declare_nonlocal_label
   right away.  (Is this still true?  -zw 2003-07-17)  */

tree
declare_label (tree name)
{
  tree label = IDENTIFIER_LABEL_VALUE (name);
  tree dup;

  /* Check to make sure that the label hasn't already been declared
     at this scope */
  for (dup = current_scope->names; dup; dup = TREE_CHAIN (dup))
    if (dup == label)
      {
	error ("duplicate label declaration `%s'", IDENTIFIER_POINTER (name));
	error ("%Jthis is a previous declaration", dup);

	/* Just use the previous declaration.  */
	return dup;
      }

  label = make_label (name, input_location);
  C_DECLARED_LABEL_FLAG (label) = 1;

  /* Declared labels go in the current scope.  */
  bind_label (name, label, current_scope);
  return label;
}

/* Define a label, specifying the location in the source file.
   Return the LABEL_DECL node for the label, if the definition is valid.
   Otherwise return 0.  */

tree
define_label (location_t location, tree name)
{
  tree label;

  /* Find any preexisting label with this name.  It is an error
     if that label has already been defined in this function, or
     if there is a containing function with a declared label with
     the same name.  */
  label = IDENTIFIER_LABEL_VALUE (name);

  if (label
      && ((DECL_CONTEXT (label) == current_function_decl
	   && DECL_INITIAL (label) != 0)
	  || (DECL_CONTEXT (label) != current_function_decl
	      && C_DECLARED_LABEL_FLAG (label))))
    {
      error ("%Hduplicate label `%D'", &location, label);
      if (DECL_INITIAL (label))
	error ("%J`%D' previously defined here", label, label);
      else
	error ("%J`%D' previously declared here", label, label);
      return 0;
    }
  else if (label && DECL_CONTEXT (label) == current_function_decl)
    {
      /* The label has been used or declared already in this function,
	 but not defined.  Update its location to point to this
	 definition.  */
      DECL_SOURCE_LOCATION (label) = location;
    }
  else
    {
      /* No label binding for that identifier; make one.  */
      label = make_label (name, location);

      /* Ordinary labels go in the current function scope.  */
      bind_label (name, label, current_function_scope);
    }

  if (warn_traditional && !in_system_header && lookup_name (name))
    warning ("%Htraditional C lacks a separate namespace for labels, "
             "identifier `%s' conflicts", &location,
	     IDENTIFIER_POINTER (name));

  /* Mark label as having been defined.  */
  DECL_INITIAL (label) = error_mark_node;
  return label;
}

/* Return the list of declarations of the current scope.  */

tree
getdecls (void)
{
  return current_scope->names;
}


/* Given NAME, an IDENTIFIER_NODE,
   return the structure (or union or enum) definition for that name.
   If THISLEVEL_ONLY is nonzero, searches only the current_scope.
   CODE says which kind of type the caller wants;
   it is RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE.
   If the wrong kind of type is found, an error is reported.  */

static tree
lookup_tag (enum tree_code code, tree name, int thislevel_only)
{
  tree tag = IDENTIFIER_TAG_VALUE (name);
  int thislevel = 0;

  if (!tag)
    return 0;

  /* We only care about whether it's in this level if
     thislevel_only was set or it might be a type clash.  */
  if (thislevel_only || TREE_CODE (tag) != code)
    {
      if (current_scope == global_scope
	  || purpose_member (name, current_scope->tags))
	thislevel = 1;
    }

  if (thislevel_only && !thislevel)
    return 0;

  if (TREE_CODE (tag) != code)
    {
      /* Definition isn't the kind we were looking for.  */
      pending_invalid_xref = name;
      pending_invalid_xref_location = input_location;

      /* If in the same binding level as a declaration as a tag
	 of a different type, this must not be allowed to
	 shadow that tag, so give the error immediately.
	 (For example, "struct foo; union foo;" is invalid.)  */
      if (thislevel)
	pending_xref_error ();
    }
  return tag;
}

/* Print an error message now
   for a recent invalid struct, union or enum cross reference.
   We don't print them immediately because they are not invalid
   when used in the `struct foo;' construct for shadowing.  */

void
pending_xref_error (void)
{
  if (pending_invalid_xref != 0)
    error ("%H`%s' defined as wrong kind of tag",
           &pending_invalid_xref_location,
           IDENTIFIER_POINTER (pending_invalid_xref));
  pending_invalid_xref = 0;
}


/* Look up NAME in the current scope and its superiors
   in the namespace of variables, functions and typedefs.
   Return a ..._DECL node of some kind representing its definition,
   or return 0 if it is undefined.  */

tree
lookup_name (tree name)
{
  tree decl = IDENTIFIER_SYMBOL_VALUE (name);
  if (decl == 0 || decl == error_mark_node)
    return decl;
  if (C_DECL_INVISIBLE (decl))
    return 0;
  return decl;
}

/* Similar to `lookup_name' but look only at the current scope.  */

static tree
lookup_name_current_level (tree name)
{
  tree decl = IDENTIFIER_SYMBOL_VALUE (name);

  if (decl == 0 || decl == error_mark_node || C_DECL_INVISIBLE (decl))
    return 0;

  if (current_scope == global_scope)
    return decl;

  /* Scan the current scope for a decl with name NAME.
     For PARM_DECLs, we have to look at both ->parms and ->names, since
     forward parameter declarations wind up on the ->names list.  */
  if (TREE_CODE (decl) == PARM_DECL
      && chain_member (decl, current_scope->parms))
    return decl;
  if (chain_member (decl, current_scope->names))
    return decl;

  return 0;
}

/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global scope.
   Make definitions for built-in primitive functions.  */

void
c_init_decl_processing (void)
{
  tree endlink;
  tree ptr_ftype_void, ptr_ftype_ptr;
  location_t save_loc = input_location;

  /* Adds some ggc roots, and reserved words for c-parse.in.  */
  c_parse_init ();

  current_function_decl = 0;

  /* Make the c_scope structure for global names.  */
  pushlevel (0);
  global_scope = current_scope;

  /* Declarations from c_common_nodes_and_builtins must not be associated
     with this input file, lest we get differences between using and not
     using preprocessed headers.  */
  input_location.file = "<internal>";
  input_location.line = 0;

  /* Make the DECL for the toplevel file scope.  */
  current_file_decl = build_decl (TRANSLATION_UNIT_DECL, NULL, NULL);

  build_common_tree_nodes (flag_signed_char);

  c_common_nodes_and_builtins ();

  /* In C, comparisons and TRUTH_* expressions have type int.  */
  truthvalue_type_node = integer_type_node;
  truthvalue_true_node = integer_one_node;
  truthvalue_false_node = integer_zero_node;

  /* Even in C99, which has a real boolean type.  */
  pushdecl (build_decl (TYPE_DECL, get_identifier ("_Bool"),
			boolean_type_node));

  endlink = void_list_node;
  ptr_ftype_void = build_function_type (ptr_type_node, endlink);
  ptr_ftype_ptr
    = build_function_type (ptr_type_node,
			   tree_cons (NULL_TREE, ptr_type_node, endlink));

  input_location = save_loc;

  pedantic_lvalues = pedantic;

  make_fname_decl = c_make_fname_decl;
  start_fname_decls ();

  first_builtin_decl = global_scope->names;
  last_builtin_decl = global_scope->names_last;
}

/* Create the VAR_DECL for __FUNCTION__ etc. ID is the name to give the
   decl, NAME is the initialization string and TYPE_DEP indicates whether
   NAME depended on the type of the function.  As we don't yet implement
   delayed emission of static data, we mark the decl as emitted
   so it is not placed in the output.  Anything using it must therefore pull
   out the STRING_CST initializer directly.  FIXME.  */

static tree
c_make_fname_decl (tree id, int type_dep)
{
  const char *name = fname_as_string (type_dep);
  tree decl, type, init;
  size_t length = strlen (name);

  type =  build_array_type
          (build_qualified_type (char_type_node, TYPE_QUAL_CONST),
	   build_index_type (size_int (length)));

  decl = build_decl (VAR_DECL, id, type);

  TREE_STATIC (decl) = 1;
  TREE_READONLY (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;

  init = build_string (length + 1, name);
  TREE_TYPE (init) = type;
  DECL_INITIAL (decl) = init;

  TREE_USED (decl) = 1;

  if (current_function_decl)
    {
      DECL_CONTEXT (decl) = current_function_decl;
      IDENTIFIER_SYMBOL_VALUE (id) = decl;
      SCOPE_LIST_APPEND (current_function_scope, names, decl);
    }

  finish_decl (decl, init, NULL_TREE);

  return decl;
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  If
   ATTRS is nonzero, use that for the function's attribute list.  */

tree
builtin_function (const char *name, tree type, int function_code,
		  enum built_in_class class, const char *library_name,
		  tree attrs)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));
  make_decl_rtl (decl, NULL);
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;

  /* Warn if a function in the namespace for users
     is used without an occasion to consider it declared.  */
  if (name[0] != '_' || name[1] != '_')
    C_DECL_INVISIBLE (decl) = 1;

  /* Possibly apply some default attributes to this built-in function.  */
  if (attrs)
    decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
  else
    decl_attributes (&decl, NULL_TREE, 0);

  return decl;
}

/* Called when a declaration is seen that contains no names to declare.
   If its type is a reference to a structure, union or enum inherited
   from a containing scope, shadow that tag name for the current scope
   with a forward reference.
   If its type defines a new named structure or union
   or defines an enum, it is valid but we need not do anything here.
   Otherwise, it is an error.  */

void
shadow_tag (tree declspecs)
{
  shadow_tag_warned (declspecs, 0);
}

void
shadow_tag_warned (tree declspecs, int warned)


     /* 1 => we have done a pedwarn.  2 => we have done a warning, but
	no pedwarn.  */
{
  int found_tag = 0;
  tree link;
  tree specs, attrs;

  pending_invalid_xref = 0;

  /* Remove the attributes from declspecs, since they will confuse the
     following code.  */
  split_specs_attrs (declspecs, &specs, &attrs);

  for (link = specs; link; link = TREE_CHAIN (link))
    {
      tree value = TREE_VALUE (link);
      enum tree_code code = TREE_CODE (value);

      if (code == RECORD_TYPE || code == UNION_TYPE || code == ENUMERAL_TYPE)
	/* Used to test also that TYPE_SIZE (value) != 0.
	   That caused warning for `struct foo;' at top level in the file.  */
	{
	  tree name = TYPE_NAME (value);
	  tree t;

	  found_tag++;

	  if (name == 0)
	    {
	      if (warned != 1 && code != ENUMERAL_TYPE)
		/* Empty unnamed enum OK */
		{
		  pedwarn ("unnamed struct/union that defines no instances");
		  warned = 1;
		}
	    }
	  else
	    {
	      t = lookup_tag (code, name, 1);

	      if (t == 0)
		{
		  t = make_node (code);
		  pushtag (name, t);
		}
	    }
	}
      else
	{
	  if (!warned && ! in_system_header)
	    {
	      warning ("useless keyword or type name in empty declaration");
	      warned = 2;
	    }
	}
    }

  if (found_tag > 1)
    error ("two types specified in one empty declaration");

  if (warned != 1)
    {
      if (found_tag == 0)
	pedwarn ("empty declaration");
    }
}

/* Construct an array declarator.  EXPR is the expression inside [], or
   NULL_TREE.  QUALS are the type qualifiers inside the [] (to be applied
   to the pointer to which a parameter array is converted).  STATIC_P is
   nonzero if "static" is inside the [], zero otherwise.  VLA_UNSPEC_P
   is nonzero is the array is [*], a VLA of unspecified length which is
   nevertheless a complete type (not currently implemented by GCC),
   zero otherwise.  The declarator is constructed as an ARRAY_REF
   (to be decoded by grokdeclarator), whose operand 0 is what's on the
   left of the [] (filled by in set_array_declarator_type) and operand 1
   is the expression inside; whose TREE_TYPE is the type qualifiers and
   which has TREE_STATIC set if "static" is used.  */

tree
build_array_declarator (tree expr, tree quals, int static_p, int vla_unspec_p)
{
  tree decl;
  decl = build_nt (ARRAY_REF, NULL_TREE, expr);
  TREE_TYPE (decl) = quals;
  TREE_STATIC (decl) = (static_p ? 1 : 0);
  if (pedantic && !flag_isoc99)
    {
      if (static_p || quals != NULL_TREE)
	pedwarn ("ISO C90 does not support `static' or type qualifiers in parameter array declarators");
      if (vla_unspec_p)
	pedwarn ("ISO C90 does not support `[*]' array declarators");
    }
  if (vla_unspec_p)
    warning ("GCC does not yet properly implement `[*]' array declarators");
  return decl;
}

/* Set the type of an array declarator.  DECL is the declarator, as
   constructed by build_array_declarator; TYPE is what appears on the left
   of the [] and goes in operand 0.  ABSTRACT_P is nonzero if it is an
   abstract declarator, zero otherwise; this is used to reject static and
   type qualifiers in abstract declarators, where they are not in the
   C99 grammar.  */

tree
set_array_declarator_type (tree decl, tree type, int abstract_p)
{
  TREE_OPERAND (decl, 0) = type;
  if (abstract_p && (TREE_TYPE (decl) != NULL_TREE || TREE_STATIC (decl)))
    error ("static or type qualifiers in abstract declarator");
  return decl;
}

/* Decode a "typename", such as "int **", returning a ..._TYPE node.  */

tree
groktypename (tree typename)
{
  tree specs, attrs;

  if (TREE_CODE (typename) != TREE_LIST)
    return typename;

  split_specs_attrs (TREE_PURPOSE (typename), &specs, &attrs);

  typename = grokdeclarator (TREE_VALUE (typename), specs, TYPENAME, 0,
			     NULL);

  /* Apply attributes.  */
  decl_attributes (&typename, attrs, 0);

  return typename;
}

/* Return a PARM_DECL node for a given pair of specs and declarator.  */

tree
groktypename_in_parm_context (tree typename)
{
  if (TREE_CODE (typename) != TREE_LIST)
    return typename;
  return grokdeclarator (TREE_VALUE (typename),
			 TREE_PURPOSE (typename),
			 PARM, 0, NULL);
}

/* Decode a declarator in an ordinary declaration or data definition.
   This is called as soon as the type information and variable name
   have been parsed, before parsing the initializer if any.
   Here we create the ..._DECL node, fill in its type,
   and put it on the list of decls for the current context.
   The ..._DECL node is returned as the value.

   Exception: for arrays where the length is not specified,
   the type is left null, to be filled in by `finish_decl'.

   Function definitions do not come here; they go to start_function
   instead.  However, external and forward declarations of functions
   do go through here.  Structure field declarations are done by
   grokfield and not through here.  */

tree
start_decl (tree declarator, tree declspecs, int initialized, tree attributes)
{
  tree decl;
  tree tem;

  /* An object declared as __attribute__((deprecated)) suppresses
     warnings of uses of other deprecated items.  */
  if (lookup_attribute ("deprecated", attributes))
    deprecated_state = DEPRECATED_SUPPRESS;

  decl = grokdeclarator (declarator, declspecs,
			 NORMAL, initialized, NULL);

  deprecated_state = DEPRECATED_NORMAL;

  if (warn_main > 0 && TREE_CODE (decl) != FUNCTION_DECL
      && MAIN_NAME_P (DECL_NAME (decl)))
    warning ("%J'%D' is usually a function", decl, decl);

  if (initialized)
    /* Is it valid for this decl to have an initializer at all?
       If not, set INITIALIZED to zero, which will indirectly
       tell `finish_decl' to ignore the initializer once it is parsed.  */
    switch (TREE_CODE (decl))
      {
      case TYPE_DECL:
	error ("typedef `%s' is initialized (use __typeof__ instead)",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
	initialized = 0;
	break;

      case FUNCTION_DECL:
	error ("function `%s' is initialized like a variable",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
	initialized = 0;
	break;

      case PARM_DECL:
	/* DECL_INITIAL in a PARM_DECL is really DECL_ARG_TYPE.  */
	error ("parameter `%s' is initialized",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
	initialized = 0;
	break;

      default:
	/* Don't allow initializations for incomplete types
	   except for arrays which might be completed by the initialization.  */

	/* This can happen if the array size is an undefined macro.  We already
	   gave a warning, so we don't need another one.  */
	if (TREE_TYPE (decl) == error_mark_node)
	  initialized = 0;
	else if (COMPLETE_TYPE_P (TREE_TYPE (decl)))
	  {
	    /* A complete type is ok if size is fixed.  */

	    if (TREE_CODE (TYPE_SIZE (TREE_TYPE (decl))) != INTEGER_CST
		|| C_DECL_VARIABLE_SIZE (decl))
	      {
		error ("variable-sized object may not be initialized");
		initialized = 0;
	      }
	  }
	else if (TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE)
	  {
	    error ("variable `%s' has initializer but incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
	else if (!COMPLETE_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
	  {
	    error ("elements of array `%s' have incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
      }

  if (initialized)
    {
      DECL_EXTERNAL (decl) = 0;
      if (current_scope == global_scope)
	TREE_STATIC (decl) = 1;

      /* Tell `pushdecl' this is an initialized decl
	 even though we don't yet have the initializer expression.
	 Also tell `finish_decl' it may store the real initializer.  */
      DECL_INITIAL (decl) = error_mark_node;
    }

  /* If this is a function declaration, write a record describing it to the
     prototypes file (if requested).  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    gen_aux_info_record (decl, 0, 0, TYPE_ARG_TYPES (TREE_TYPE (decl)) != 0);

  /* ANSI specifies that a tentative definition which is not merged with
     a non-tentative definition behaves exactly like a definition with an
     initializer equal to zero.  (Section 3.7.2)

     -fno-common gives strict ANSI behavior, though this tends to break
     a large body of code that grew up without this rule.

     Thread-local variables are never common, since there's no entrenched
     body of code to break, and it allows more efficient variable references
     in the presence of dynamic linking.  */

  if (TREE_CODE (decl) == VAR_DECL
      && !initialized
      && TREE_PUBLIC (decl)
      && !DECL_THREAD_LOCAL (decl)
      && !flag_no_common)
    DECL_COMMON (decl) = 1;

  /* Set attributes here so if duplicate decl, will have proper attributes.  */
  decl_attributes (&decl, attributes, 0);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && targetm.calls.promote_prototypes (TREE_TYPE (decl)))
    {
      tree ce = declarator;

      if (TREE_CODE (ce) == INDIRECT_REF)
	ce = TREE_OPERAND (declarator, 0);
      if (TREE_CODE (ce) == CALL_EXPR)
	{
	  tree args = TREE_PURPOSE (TREE_OPERAND (ce, 1));
	  for (; args; args = TREE_CHAIN (args))
	    {
	      tree type = TREE_TYPE (args);
	      if (INTEGRAL_TYPE_P (type)
		  && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
		DECL_ARG_TYPE (args) = integer_type_node;
	    }
	}
    }

  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_DECLARED_INLINE_P (decl)
      && DECL_UNINLINABLE (decl)
      && lookup_attribute ("noinline", DECL_ATTRIBUTES (decl)))
    warning ("%Jinline function '%D' given attribute noinline", decl, decl);

  /* Add this decl to the current scope.
     TEM may equal DECL or it may be a previous decl of the same name.  */
  tem = pushdecl (decl);

  /* For a local variable, define the RTL now.  */
  if (current_scope != global_scope
      /* But not if this is a duplicate decl
	 and we preserved the rtl from the previous one
	 (which may or may not happen).  */
      && !DECL_RTL_SET_P (tem)
      && DECL_FILE_SCOPE_P (tem))
    {
      if (TREE_TYPE (tem) != error_mark_node
	  && (COMPLETE_TYPE_P (TREE_TYPE (tem))
	      || (TREE_CODE (TREE_TYPE (tem)) == ARRAY_TYPE
		  && DECL_INITIAL (tem) != 0)))
	expand_decl (tem);
    }

  return tem;
}

/* Finish processing of a declaration;
   install its initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.  */

void
finish_decl (tree decl, tree init, tree asmspec_tree)
{
  tree type = TREE_TYPE (decl);
  int was_incomplete = (DECL_SIZE (decl) == 0);
  const char *asmspec = 0;

  /* If a name was specified, get the string.  */
  if (current_scope == global_scope)
    asmspec_tree = maybe_apply_renaming_pragma (decl, asmspec_tree);
  if (asmspec_tree)
    asmspec = TREE_STRING_POINTER (asmspec_tree);

  /* If `start_decl' didn't like having an initialization, ignore it now.  */
  if (init != 0 && DECL_INITIAL (decl) == 0)
    init = 0;

  /* Don't crash if parm is initialized.  */
  if (TREE_CODE (decl) == PARM_DECL)
    init = 0;

  if (init)
    store_init_value (decl, init);

  if (c_dialect_objc () && (TREE_CODE (decl) == VAR_DECL
		    || TREE_CODE (decl) == FUNCTION_DECL
		    || TREE_CODE (decl) == FIELD_DECL))
    objc_check_decl (decl);

  /* Deduce size of array from initialization, if not already known.  */
  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == 0
      && TREE_CODE (decl) != TYPE_DECL)
    {
      int do_default
	= (TREE_STATIC (decl)
	   /* Even if pedantic, an external linkage array
	      may have incomplete type at first.  */
	   ? pedantic && !TREE_PUBLIC (decl)
	   : !DECL_EXTERNAL (decl));
      int failure
	= complete_array_type (type, DECL_INITIAL (decl), do_default);

      /* Get the completed type made by complete_array_type.  */
      type = TREE_TYPE (decl);

      if (failure == 1)
	error ("%Jinitializer fails to determine size of '%D'", decl, decl);

      else if (failure == 2)
	{
	  if (do_default)
	    error ("%Jarray size missing in '%D'", decl, decl);
	  /* If a `static' var's size isn't known,
	     make it extern as well as static, so it does not get
	     allocated.
	     If it is not `static', then do not mark extern;
	     finish_incomplete_decl will give it a default size
	     and it will get allocated.  */
	  else if (!pedantic && TREE_STATIC (decl) && ! TREE_PUBLIC (decl))
	    DECL_EXTERNAL (decl) = 1;
	}

      /* TYPE_MAX_VALUE is always one less than the number of elements
	 in the array, because we start counting at zero.  Therefore,
	 warn only if the value is less than zero.  */
      else if (pedantic && TYPE_DOMAIN (type) != 0
	      && tree_int_cst_sgn (TYPE_MAX_VALUE (TYPE_DOMAIN (type))) < 0)
	error ("%Jzero or negative size array '%D'", decl, decl);

      layout_decl (decl, 0);
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SIZE (decl) == 0 && TREE_TYPE (decl) != error_mark_node
	  && COMPLETE_TYPE_P (TREE_TYPE (decl)))
	layout_decl (decl, 0);

      if (DECL_SIZE (decl) == 0
	  /* Don't give an error if we already gave one earlier.  */
	  && TREE_TYPE (decl) != error_mark_node
	  && (TREE_STATIC (decl)
	      ?
		/* A static variable with an incomplete type
		   is an error if it is initialized.
		   Also if it is not file scope.
		   Otherwise, let it through, but if it is not `extern'
		   then it may cause an error message later.  */
		(DECL_INITIAL (decl) != 0
		 || !DECL_FILE_SCOPE_P (decl))
	      :
		/* An automatic variable with an incomplete type
		   is an error.  */
		!DECL_EXTERNAL (decl)))
	{
	  error ("%Jstorage size of '%D' isn't known", decl, decl);
	  TREE_TYPE (decl) = error_mark_node;
	}

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
	  && DECL_SIZE (decl) != 0)
	{
	  if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
	    constant_expression_warning (DECL_SIZE (decl));
	  else
	    error ("%Jstorage size of '%D' isn't constant", decl, decl);
	}

      if (TREE_USED (type))
	TREE_USED (decl) = 1;
    }

  /* If this is a function and an assembler name is specified, reset DECL_RTL
     so we can give it its new name.  Also, update built_in_decls if it
     was a normal built-in.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && asmspec)
    {
      /* ASMSPEC is given, and not the name of a register.  Mark the
      name with a star so assemble_name won't munge it.  */
      char *starred = alloca (strlen (asmspec) + 2);
      starred[0] = '*';
      strcpy (starred + 1, asmspec);

      if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
	{
	  tree builtin = built_in_decls [DECL_FUNCTION_CODE (decl)];
	  SET_DECL_RTL (builtin, NULL_RTX);
	  SET_DECL_ASSEMBLER_NAME (builtin, get_identifier (starred));
#ifdef TARGET_MEM_FUNCTIONS
	  if (DECL_FUNCTION_CODE (decl) == BUILT_IN_MEMCPY)
	    init_block_move_fn (starred);
	  else if (DECL_FUNCTION_CODE (decl) == BUILT_IN_MEMSET)
	    init_block_clear_fn (starred);
#else
	  if (DECL_FUNCTION_CODE (decl) == BUILT_IN_BCOPY)
	    init_block_move_fn (starred);
	  else if (DECL_FUNCTION_CODE (decl) == BUILT_IN_BZERO)
	    init_block_clear_fn (starred);
#endif
	}
      SET_DECL_RTL (decl, NULL_RTX);
      change_decl_assembler_name (decl, get_identifier (starred));
    }

  /* If #pragma weak was used, mark the decl weak now.  */
  if (current_scope == global_scope)
    maybe_apply_pragma_weak (decl);

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* This is a no-op in c-lang.c or something real in objc-act.c.  */
      if (c_dialect_objc ())
	objc_check_decl (decl);

      if (DECL_FILE_SCOPE_P (decl))
	{
	  if (DECL_INITIAL (decl) == NULL_TREE
	      || DECL_INITIAL (decl) == error_mark_node)
	    /* Don't output anything
	       when a tentative file-scope definition is seen.
	       But at end of compilation, do output code for them.  */
	    DECL_DEFER_OUTPUT (decl) = 1;
	  rest_of_decl_compilation (decl, asmspec, true, 0);
	}
      else
	{
	  /* This is a local variable.  If there is an ASMSPEC, the
	     user has requested that we handle it specially.  */
	  if (asmspec)
	    {
	      /* In conjunction with an ASMSPEC, the `register'
		 keyword indicates that we should place the variable
		 in a particular register.  */
	      if (DECL_REGISTER (decl))
		DECL_C_HARD_REGISTER (decl) = 1;

	      /* If this is not a static variable, issue a warning.
		 It doesn't make any sense to give an ASMSPEC for an
		 ordinary, non-register local variable.  Historically,
		 GCC has accepted -- but ignored -- the ASMSPEC in
		 this case.  */
	      if (TREE_CODE (decl) == VAR_DECL
		  && !DECL_REGISTER (decl)
		  && !TREE_STATIC (decl))
		warning ("%Jignoring asm-specifier for non-static local "
                         "variable '%D'", decl, decl);
	      else
		change_decl_assembler_name (decl, get_identifier (asmspec));
	    }

	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    add_decl_stmt (decl);
	}

      if (!DECL_FILE_SCOPE_P (decl))
	{
	  /* Recompute the RTL of a local array now
	     if it used to be an incomplete type.  */
	  if (was_incomplete
	      && ! TREE_STATIC (decl) && ! DECL_EXTERNAL (decl))
	    {
	      /* If we used it already as memory, it must stay in memory.  */
	      TREE_ADDRESSABLE (decl) = TREE_USED (decl);
	      /* If it's still incomplete now, no init will save it.  */
	      if (DECL_SIZE (decl) == 0)
		DECL_INITIAL (decl) = 0;
	    }
	}
    }

  /* If this was marked 'used', be sure it will be output.  */
  if (lookup_attribute ("used", DECL_ATTRIBUTES (decl)))
    mark_referenced (DECL_ASSEMBLER_NAME (decl));

  if (TREE_CODE (decl) == TYPE_DECL)
    rest_of_decl_compilation (decl, NULL, DECL_FILE_SCOPE_P (decl), 0);

  /* At the end of a declaration, throw away any variable type sizes
     of types defined inside that declaration.  There is no use
     computing them in the following function definition.  */
  if (current_scope == global_scope)
    get_pending_sizes ();

  /* Install a cleanup (aka destructor) if one was given.  */
  if (TREE_CODE (decl) == VAR_DECL && !TREE_STATIC (decl))
    {
      tree attr = lookup_attribute ("cleanup", DECL_ATTRIBUTES (decl));
      if (attr)
	{
	  static bool eh_initialized_p;

	  tree cleanup_id = TREE_VALUE (TREE_VALUE (attr));
	  tree cleanup_decl = lookup_name (cleanup_id);
	  tree cleanup;

	  /* Build "cleanup(&decl)" for the destructor.  */
	  cleanup = build_unary_op (ADDR_EXPR, decl, 0);
	  cleanup = build_tree_list (NULL_TREE, cleanup);
	  cleanup = build_function_call (cleanup_decl, cleanup);

	  /* Don't warn about decl unused; the cleanup uses it.  */
	  TREE_USED (decl) = 1;

	  /* Initialize EH, if we've been told to do so.  */
	  if (flag_exceptions && !eh_initialized_p)
	    {
	      eh_initialized_p = true;
	      eh_personality_libfunc
		= init_one_libfunc (USING_SJLJ_EXCEPTIONS
				    ? "__gcc_personality_sj0"
				    : "__gcc_personality_v0");
	      using_eh_for_cleanups ();
	    }

	  add_stmt (build_stmt (CLEANUP_STMT, decl, cleanup));
	}
    }
}

/* Given a parsed parameter declaration, decode it into a PARM_DECL
   and push that on the current scope.  */

void
push_parm_decl (tree parm)
{
  tree decl;

  /* Don't attempt to expand sizes while parsing this decl.
     (We can get here with i_s_e 1 somehow from Objective-C.)  */
  int save_immediate_size_expand = immediate_size_expand;
  immediate_size_expand = 0;

  decl = grokdeclarator (TREE_VALUE (TREE_PURPOSE (parm)),
			 TREE_PURPOSE (TREE_PURPOSE (parm)),
			 PARM, 0, NULL);
  decl_attributes (&decl, TREE_VALUE (parm), 0);

  decl = pushdecl (decl);

  finish_decl (decl, NULL_TREE, NULL_TREE);

  immediate_size_expand = save_immediate_size_expand;
}

/* Mark all the parameter declarations to date as forward decls,
   shift them to the variables list, and reset the parameters list.
   Also diagnose use of this extension.  */

void
mark_forward_parm_decls (void)
{
  tree parm;

  if (pedantic && !current_scope->warned_forward_parm_decls)
    {
      pedwarn ("ISO C forbids forward parameter declarations");
      current_scope->warned_forward_parm_decls = true;
    }

  for (parm = current_scope->parms; parm; parm = TREE_CHAIN (parm))
    TREE_ASM_WRITTEN (parm) = 1;

  SCOPE_LIST_CONCAT (current_scope, names, current_scope, parms);
  current_scope->parms = 0;
  current_scope->parms_last = 0;
}

static GTY(()) int compound_literal_number;

/* Build a COMPOUND_LITERAL_EXPR.  TYPE is the type given in the compound
   literal, which may be an incomplete array type completed by the
   initializer; INIT is a CONSTRUCTOR that initializes the compound
   literal.  */

tree
build_compound_literal (tree type, tree init)
{
  /* We do not use start_decl here because we have a type, not a declarator;
     and do not use finish_decl because the decl should be stored inside
     the COMPOUND_LITERAL_EXPR rather than added elsewhere as a DECL_STMT.  */
  tree decl = build_decl (VAR_DECL, NULL_TREE, type);
  tree complit;
  tree stmt;
  DECL_EXTERNAL (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  TREE_STATIC (decl) = (current_scope == global_scope);
  DECL_CONTEXT (decl) = current_function_decl;
  TREE_USED (decl) = 1;
  TREE_TYPE (decl) = type;
  TREE_READONLY (decl) = TREE_READONLY (type);
  store_init_value (decl, init);

  if (TREE_CODE (type) == ARRAY_TYPE && !COMPLETE_TYPE_P (type))
    {
      int failure = complete_array_type (type, DECL_INITIAL (decl), 1);
      if (failure)
	abort ();
    }

  type = TREE_TYPE (decl);
  if (type == error_mark_node || !COMPLETE_TYPE_P (type))
    return error_mark_node;

  stmt = build_stmt (DECL_STMT, decl);
  complit = build1 (COMPOUND_LITERAL_EXPR, TREE_TYPE (decl), stmt);
  TREE_SIDE_EFFECTS (complit) = 1;

  layout_decl (decl, 0);

  if (TREE_STATIC (decl))
    {
      /* This decl needs a name for the assembler output.  We also need
	 a unique suffix to be added to the name.  */
      char *name;

      ASM_FORMAT_PRIVATE_NAME (name, "__compound_literal",
			       compound_literal_number);
      compound_literal_number++;
      DECL_NAME (decl) = get_identifier (name);
      DECL_DEFER_OUTPUT (decl) = 1;
      DECL_COMDAT (decl) = 1;
      DECL_ARTIFICIAL (decl) = 1;
      pushdecl (decl);
      rest_of_decl_compilation (decl, NULL, 1, 0);
    }

  return complit;
}

/* Make TYPE a complete type based on INITIAL_VALUE.
   Return 0 if successful, 1 if INITIAL_VALUE can't be deciphered,
   2 if there was no information (in which case assume 1 if DO_DEFAULT).  */

int
complete_array_type (tree type, tree initial_value, int do_default)
{
  tree maxindex = NULL_TREE;
  int value = 0;

  if (initial_value)
    {
      /* Note MAXINDEX  is really the maximum index,
	 one less than the size.  */
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
	  maxindex = build_int_2 (-1, -1);
	  for (; elts; elts = TREE_CHAIN (elts))
	    {
	      if (TREE_PURPOSE (elts))
		maxindex = TREE_PURPOSE (elts);
	      else
		maxindex = fold (build (PLUS_EXPR, integer_type_node,
					maxindex, integer_one_node));
	    }
	  maxindex = copy_node (maxindex);
	}
      else
	{
	  /* Make an error message unless that happened already.  */
	  if (initial_value != error_mark_node)
	    value = 1;

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
      TYPE_DOMAIN (type) = build_index_type (maxindex);
      if (!TREE_TYPE (maxindex))
	TREE_TYPE (maxindex) = TYPE_DOMAIN (type);
    }

  /* Lay out the type now that we can get the real answer.  */

  layout_type (type);

  return value;
}

/* Determine whether TYPE is a structure with a flexible array member,
   or a union containing such a structure (possibly recursively).  */

static bool
flexible_array_type_p (tree type)
{
  tree x;
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      x = TYPE_FIELDS (type);
      if (x == NULL_TREE)
	return false;
      while (TREE_CHAIN (x) != NULL_TREE)
	x = TREE_CHAIN (x);
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	  && TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
	  && TYPE_DOMAIN (TREE_TYPE (x)) != NULL_TREE
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (x))) == NULL_TREE)
	return true;
      return false;
    case UNION_TYPE:
      for (x = TYPE_FIELDS (type); x != NULL_TREE; x = TREE_CHAIN (x))
	{
	  if (flexible_array_type_p (TREE_TYPE (x)))
	    return true;
	}
      return false;
    default:
    return false;
  }
}

/* Performs sanity checks on the TYPE and WIDTH of the bit-field NAME,
   replacing with appropriate values if they are invalid.  */
static void
check_bitfield_type_and_width (tree *type, tree *width, const char *orig_name)
{
  tree type_mv;
  unsigned int max_width;
  unsigned HOST_WIDE_INT w;
  const char *name = orig_name ? orig_name: _("<anonymous>");

  /* Necessary?  */
  STRIP_NOPS (*width);

  /* Detect and ignore out of range field width and process valid
     field widths.  */
  if (TREE_CODE (*width) != INTEGER_CST)
    {
      error ("bit-field `%s' width not an integer constant", name);
      *width = integer_one_node;
    }
  else
    {
      constant_expression_warning (*width);
      if (tree_int_cst_sgn (*width) < 0)
	{
	  error ("negative width in bit-field `%s'", name);
	  *width = integer_one_node;
	}
      else if (integer_zerop (*width) && orig_name)
	{
	  error ("zero width for bit-field `%s'", name);
	  *width = integer_one_node;
	}
    }

  /* Detect invalid bit-field type.  */
  if (TREE_CODE (*type) != INTEGER_TYPE
      && TREE_CODE (*type) != BOOLEAN_TYPE
      && TREE_CODE (*type) != ENUMERAL_TYPE)
    {
      error ("bit-field `%s' has invalid type", name);
      *type = unsigned_type_node;
    }

  type_mv = TYPE_MAIN_VARIANT (*type);
  if (pedantic
      && type_mv != integer_type_node
      && type_mv != unsigned_type_node
      && type_mv != boolean_type_node)
    pedwarn ("type of bit-field `%s' is a GCC extension", name);

  if (type_mv == boolean_type_node)
    max_width = CHAR_TYPE_SIZE;
  else
    max_width = TYPE_PRECISION (*type);

  if (0 < compare_tree_int (*width, max_width))
    {
      error ("width of `%s' exceeds its type", name);
      w = max_width;
      *width = build_int_2 (w, 0);
    }
  else
    w = tree_low_cst (*width, 1);

  if (TREE_CODE (*type) == ENUMERAL_TYPE
      && (w < min_precision (TYPE_MIN_VALUE (*type), TREE_UNSIGNED (*type))
	  || w < min_precision (TYPE_MAX_VALUE (*type), TREE_UNSIGNED (*type))))
    warning ("`%s' is narrower than values of its type", name);
}

/* Given declspecs and a declarator,
   determine the name and type of the object declared
   and construct a ..._DECL node for it.
   (In one case we can return a ..._TYPE node instead.
    For invalid input we sometimes return 0.)

   DECLSPECS is a chain of tree_list nodes whose value fields
    are the storage classes and type specifiers.

   DECL_CONTEXT says which syntactic context this declaration is in:
     NORMAL for most contexts.  Make a VAR_DECL or FUNCTION_DECL or TYPE_DECL.
     FUNCDEF for a function definition.  Like NORMAL but a few different
      error messages in each case.  Return value may be zero meaning
      this definition is too screwy to try to parse.
     PARM for a parameter declaration (either within a function prototype
      or before a function body).  Make a PARM_DECL, or return void_type_node.
     TYPENAME if for a typename (in a cast or sizeof).
      Don't make a DECL node; just return the ..._TYPE node.
     FIELD for a struct or union field; make a FIELD_DECL.
   INITIALIZED is 1 if the decl has an initializer.
   WIDTH is non-NULL for bit-fields, and is a pointer to an INTEGER_CST node
   representing the width of the bit-field.

   In the TYPENAME case, DECLARATOR is really an absolute declarator.
   It may also be so in the PARM case, for a prototype where the
   argument type is specified but not the name.

   This function is where the complicated C meanings of `static'
   and `extern' are interpreted.  */

static tree
grokdeclarator (tree declarator, tree declspecs,
		enum decl_context decl_context, int initialized, tree *width)
{
  int specbits = 0;
  tree spec;
  tree type = NULL_TREE;
  int longlong = 0;
  int constp;
  int restrictp;
  int volatilep;
  int type_quals = TYPE_UNQUALIFIED;
  int inlinep;
  int explicit_int = 0;
  int explicit_char = 0;
  int defaulted_int = 0;
  tree typedef_decl = 0;
  const char *name, *orig_name;
  tree typedef_type = 0;
  int funcdef_flag = 0;
  enum tree_code innermost_code = ERROR_MARK;
  int size_varies = 0;
  tree decl_attr = NULL_TREE;
  tree array_ptr_quals = NULL_TREE;
  int array_parm_static = 0;
  tree returned_attrs = NULL_TREE;
  bool bitfield = width != NULL;
  tree element_type;

  if (decl_context == FUNCDEF)
    funcdef_flag = 1, decl_context = NORMAL;

  /* Look inside a declarator for the name being declared
     and get it as a string, for an error message.  */
  {
    tree decl = declarator;
    name = 0;

    while (decl)
      switch (TREE_CODE (decl))
	{
	case ARRAY_REF:
	case INDIRECT_REF:
	case CALL_EXPR:
	  innermost_code = TREE_CODE (decl);
	  decl = TREE_OPERAND (decl, 0);
	  break;

	case TREE_LIST:
	  decl = TREE_VALUE (decl);
	  break;

	case IDENTIFIER_NODE:
	  name = IDENTIFIER_POINTER (decl);
	  decl = 0;
	  break;

	default:
	  abort ();
	}
    orig_name = name;
    if (name == 0)
      name = "type name";
  }

  /* A function definition's declarator must have the form of
     a function declarator.  */

  if (funcdef_flag && innermost_code != CALL_EXPR)
    return 0;

  /* If this looks like a function definition, make it one,
     even if it occurs where parms are expected.
     Then store_parm_decls will reject it and not use it as a parm.  */
  if (decl_context == NORMAL && !funcdef_flag
      && current_scope->parm_flag)
    decl_context = PARM;

  /* Look through the decl specs and record which ones appear.
     Some typespecs are defined as built-in typenames.
     Others, the ones that are modifiers of other types,
     are represented by bits in SPECBITS: set the bits for
     the modifiers that appear.  Storage class keywords are also in SPECBITS.

     If there is a typedef name or a type, store the type in TYPE.
     This includes builtin typedefs such as `int'.

     Set EXPLICIT_INT or EXPLICIT_CHAR if the type is `int' or `char'
     and did not come from a user typedef.

     Set LONGLONG if `long' is mentioned twice.  */

  for (spec = declspecs; spec; spec = TREE_CHAIN (spec))
    {
      tree id = TREE_VALUE (spec);

      /* If the entire declaration is itself tagged as deprecated then
         suppress reports of deprecated items.  */
      if (id && TREE_DEPRECATED (id))
        {
	  if (deprecated_state != DEPRECATED_SUPPRESS)
	    warn_deprecated_use (id);
        }

      if (id == ridpointers[(int) RID_INT])
	explicit_int = 1;
      if (id == ridpointers[(int) RID_CHAR])
	explicit_char = 1;

      if (TREE_CODE (id) == IDENTIFIER_NODE && C_IS_RESERVED_WORD (id))
	{
	  enum rid i = C_RID_CODE (id);
	  if ((int) i <= (int) RID_LAST_MODIFIER)
	    {
	      if (i == RID_LONG && (specbits & (1 << (int) RID_LONG)))
		{
		  if (longlong)
		    error ("`long long long' is too long for GCC");
		  else
		    {
		      if (pedantic && !flag_isoc99 && ! in_system_header
			  && warn_long_long)
			pedwarn ("ISO C90 does not support `long long'");
		      longlong = 1;
		    }
		}
	      else if (specbits & (1 << (int) i))
		{
		  if (i == RID_CONST || i == RID_VOLATILE || i == RID_RESTRICT)
		    {
		      if (pedantic && !flag_isoc99)
			pedwarn ("duplicate `%s'", IDENTIFIER_POINTER (id));
		    }
		  else
		    error ("duplicate `%s'", IDENTIFIER_POINTER (id));
		}

	      /* Diagnose "__thread extern".  Recall that this list
		 is in the reverse order seen in the text.  */
	      if (i == RID_THREAD
		  && (specbits & (1 << (int) RID_EXTERN
				  | 1 << (int) RID_STATIC)))
		{
		  if (specbits & 1 << (int) RID_EXTERN)
		    error ("`__thread' before `extern'");
		  else
		    error ("`__thread' before `static'");
		}

	      specbits |= 1 << (int) i;
	      goto found;
	    }
	}
      if (type)
	error ("two or more data types in declaration of `%s'", name);
      /* Actual typedefs come to us as TYPE_DECL nodes.  */
      else if (TREE_CODE (id) == TYPE_DECL)
	{
	  if (TREE_TYPE (id) == error_mark_node)
	    ; /* Allow the type to default to int to avoid cascading errors.  */
	  else
	    {
	      type = TREE_TYPE (id);
	      decl_attr = DECL_ATTRIBUTES (id);
	      typedef_decl = id;
	    }
	}
      /* Built-in types come as identifiers.  */
      else if (TREE_CODE (id) == IDENTIFIER_NODE)
	{
	  tree t = lookup_name (id);
	  if (TREE_TYPE (t) == error_mark_node)
	    ;
	  else if (!t || TREE_CODE (t) != TYPE_DECL)
	    error ("`%s' fails to be a typedef or built in type",
		   IDENTIFIER_POINTER (id));
	  else
	    {
	      type = TREE_TYPE (t);
	      typedef_decl = t;
	    }
	}
      else if (TREE_CODE (id) != ERROR_MARK)
	type = id;

    found:
      ;
    }

  typedef_type = type;
  if (type)
    size_varies = C_TYPE_VARIABLE_SIZE (type);

  /* No type at all: default to `int', and set DEFAULTED_INT
     because it was not a user-defined typedef.  */

  if (type == 0)
    {
      if ((! (specbits & ((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
			  | (1 << (int) RID_SIGNED)
			  | (1 << (int) RID_UNSIGNED)
			  | (1 << (int) RID_COMPLEX))))
	  /* Don't warn about typedef foo = bar.  */
	  && ! (specbits & (1 << (int) RID_TYPEDEF) && initialized)
	  && ! in_system_header)
	{
	  /* Issue a warning if this is an ISO C 99 program or if -Wreturn-type
	     and this is a function, or if -Wimplicit; prefer the former
	     warning since it is more explicit.  */
	  if ((warn_implicit_int || warn_return_type || flag_isoc99)
	      && funcdef_flag)
	    warn_about_return_type = 1;
	  else if (warn_implicit_int || flag_isoc99)
	    pedwarn_c99 ("type defaults to `int' in declaration of `%s'",
			 name);
	}

      defaulted_int = 1;
      type = integer_type_node;
    }

  /* Now process the modifiers that were specified
     and check for invalid combinations.  */

  /* Long double is a special combination.  */

  if ((specbits & 1 << (int) RID_LONG) && ! longlong
      && TYPE_MAIN_VARIANT (type) == double_type_node)
    {
      specbits &= ~(1 << (int) RID_LONG);
      type = long_double_type_node;
    }

  /* Check all other uses of type modifiers.  */

  if (specbits & ((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
		  | (1 << (int) RID_UNSIGNED) | (1 << (int) RID_SIGNED)))
    {
      int ok = 0;

      if ((specbits & 1 << (int) RID_LONG)
	  && (specbits & 1 << (int) RID_SHORT))
	error ("both long and short specified for `%s'", name);
      else if (((specbits & 1 << (int) RID_LONG)
		|| (specbits & 1 << (int) RID_SHORT))
	       && explicit_char)
	error ("long or short specified with char for `%s'", name);
      else if (((specbits & 1 << (int) RID_LONG)
		|| (specbits & 1 << (int) RID_SHORT))
	       && TREE_CODE (type) == REAL_TYPE)
	{
	  static int already = 0;

	  error ("long or short specified with floating type for `%s'", name);
	  if (! already && ! pedantic)
	    {
	      error ("the only valid combination is `long double'");
	      already = 1;
	    }
	}
      else if ((specbits & 1 << (int) RID_SIGNED)
	       && (specbits & 1 << (int) RID_UNSIGNED))
	error ("both signed and unsigned specified for `%s'", name);
      else if (TREE_CODE (type) != INTEGER_TYPE)
	error ("long, short, signed or unsigned invalid for `%s'", name);
      else
	{
	  ok = 1;
	  if (!explicit_int && !defaulted_int && !explicit_char)
	    {
	      error ("long, short, signed or unsigned used invalidly for `%s'",
		     name);
	      ok = 0;
	    }
	}

      /* Discard the type modifiers if they are invalid.  */
      if (! ok)
	{
	  specbits &= ~((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
			| (1 << (int) RID_UNSIGNED) | (1 << (int) RID_SIGNED));
	  longlong = 0;
	}
    }

  if ((specbits & (1 << (int) RID_COMPLEX))
      && TREE_CODE (type) != INTEGER_TYPE && TREE_CODE (type) != REAL_TYPE)
    {
      error ("complex invalid for `%s'", name);
      specbits &= ~(1 << (int) RID_COMPLEX);
    }

  /* Decide whether an integer type is signed or not.
     Optionally treat bit-fields as signed by default.  */
  if (specbits & 1 << (int) RID_UNSIGNED
      || (bitfield && ! flag_signed_bitfields
	  && (explicit_int || defaulted_int || explicit_char
	      /* A typedef for plain `int' without `signed'
		 can be controlled just like plain `int'.  */
	      || ! (typedef_decl != 0
		    && C_TYPEDEF_EXPLICITLY_SIGNED (typedef_decl)))
	  && TREE_CODE (type) != ENUMERAL_TYPE
	  && !(specbits & 1 << (int) RID_SIGNED)))
    {
      if (longlong)
	type = long_long_unsigned_type_node;
      else if (specbits & 1 << (int) RID_LONG)
	type = long_unsigned_type_node;
      else if (specbits & 1 << (int) RID_SHORT)
	type = short_unsigned_type_node;
      else if (type == char_type_node)
	type = unsigned_char_type_node;
      else if (typedef_decl)
	type = c_common_unsigned_type (type);
      else
	type = unsigned_type_node;
    }
  else if ((specbits & 1 << (int) RID_SIGNED)
	   && type == char_type_node)
    type = signed_char_type_node;
  else if (longlong)
    type = long_long_integer_type_node;
  else if (specbits & 1 << (int) RID_LONG)
    type = long_integer_type_node;
  else if (specbits & 1 << (int) RID_SHORT)
    type = short_integer_type_node;

  if (specbits & 1 << (int) RID_COMPLEX)
    {
      if (pedantic && !flag_isoc99)
	pedwarn ("ISO C90 does not support complex types");
      /* If we just have "complex", it is equivalent to
	 "complex double", but if any modifiers at all are specified it is
	 the complex form of TYPE.  E.g, "complex short" is
	 "complex short int".  */

      if (defaulted_int && ! longlong
	  && ! (specbits & ((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
			    | (1 << (int) RID_SIGNED)
			    | (1 << (int) RID_UNSIGNED))))
	{
	  if (pedantic)
	    pedwarn ("ISO C does not support plain `complex' meaning `double complex'");
	  type = complex_double_type_node;
	}
      else if (type == integer_type_node)
	{
	  if (pedantic)
	    pedwarn ("ISO C does not support complex integer types");
	  type = complex_integer_type_node;
	}
      else if (type == float_type_node)
	type = complex_float_type_node;
      else if (type == double_type_node)
	type = complex_double_type_node;
      else if (type == long_double_type_node)
	type = complex_long_double_type_node;
      else
	{
	  if (pedantic)
	    pedwarn ("ISO C does not support complex integer types");
	  type = build_complex_type (type);
	}
    }

  /* Figure out the type qualifiers for the declaration.  There are
     two ways a declaration can become qualified.  One is something
     like `const int i' where the `const' is explicit.  Another is
     something like `typedef const int CI; CI i' where the type of the
     declaration contains the `const'.  A third possibility is that
     there is a type qualifier on the element type of a typedefed
     array type, in which case we should extract that qualifier so
     that c_apply_type_quals_to_decls receives the full list of
     qualifiers to work with (C90 is not entirely clear about whether
     duplicate qualifiers should be diagnosed in this case, but it
     seems most appropriate to do so).  */
  element_type = strip_array_types (type);
  constp = !! (specbits & 1 << (int) RID_CONST) + TYPE_READONLY (element_type);
  restrictp
    = !! (specbits & 1 << (int) RID_RESTRICT) + TYPE_RESTRICT (element_type);
  volatilep
    = !! (specbits & 1 << (int) RID_VOLATILE) + TYPE_VOLATILE (element_type);
  inlinep = !! (specbits & (1 << (int) RID_INLINE));
  if (pedantic && !flag_isoc99)
    {
      if (constp > 1)
	pedwarn ("duplicate `const'");
      if (restrictp > 1)
	pedwarn ("duplicate `restrict'");
      if (volatilep > 1)
	pedwarn ("duplicate `volatile'");
    }
  if (! flag_gen_aux_info && (TYPE_QUALS (type)))
    type = TYPE_MAIN_VARIANT (type);
  type_quals = ((constp ? TYPE_QUAL_CONST : 0)
		| (restrictp ? TYPE_QUAL_RESTRICT : 0)
		| (volatilep ? TYPE_QUAL_VOLATILE : 0));

  /* Warn if two storage classes are given. Default to `auto'.  */

  {
    int nclasses = 0;

    if (specbits & 1 << (int) RID_AUTO) nclasses++;
    if (specbits & 1 << (int) RID_STATIC) nclasses++;
    if (specbits & 1 << (int) RID_EXTERN) nclasses++;
    if (specbits & 1 << (int) RID_REGISTER) nclasses++;
    if (specbits & 1 << (int) RID_TYPEDEF) nclasses++;

    /* "static __thread" and "extern __thread" are allowed.  */
    if ((specbits & (1 << (int) RID_THREAD
		     | 1 << (int) RID_STATIC
		     | 1 << (int) RID_EXTERN)) == (1 << (int) RID_THREAD))
      nclasses++;

    /* Warn about storage classes that are invalid for certain
       kinds of declarations (parameters, typenames, etc.).  */

    if (nclasses > 1)
      error ("multiple storage classes in declaration of `%s'", name);
    else if (funcdef_flag
	     && (specbits
		 & ((1 << (int) RID_REGISTER)
		    | (1 << (int) RID_AUTO)
		    | (1 << (int) RID_TYPEDEF)
		    | (1 << (int) RID_THREAD))))
      {
	if (specbits & 1 << (int) RID_AUTO
	    && (pedantic || current_scope == global_scope))
	  pedwarn ("function definition declared `auto'");
	if (specbits & 1 << (int) RID_REGISTER)
	  error ("function definition declared `register'");
	if (specbits & 1 << (int) RID_TYPEDEF)
	  error ("function definition declared `typedef'");
	if (specbits & 1 << (int) RID_THREAD)
	  error ("function definition declared `__thread'");
	specbits &= ~((1 << (int) RID_TYPEDEF) | (1 << (int) RID_REGISTER)
		      | (1 << (int) RID_AUTO) | (1 << (int) RID_THREAD));
      }
    else if (decl_context != NORMAL && nclasses > 0)
      {
	if (decl_context == PARM && specbits & 1 << (int) RID_REGISTER)
	  ;
	else
	  {
	    switch (decl_context)
	      {
	      case FIELD:
		error ("storage class specified for structure field `%s'",
		       name);
		break;
	      case PARM:
		error ("storage class specified for parameter `%s'", name);
		break;
	      default:
		error ("storage class specified for typename");
		break;
	      }
	    specbits &= ~((1 << (int) RID_TYPEDEF) | (1 << (int) RID_REGISTER)
			  | (1 << (int) RID_AUTO) | (1 << (int) RID_STATIC)
			  | (1 << (int) RID_EXTERN) | (1 << (int) RID_THREAD));
	  }
      }
    else if (specbits & 1 << (int) RID_EXTERN && initialized && ! funcdef_flag)
      {
	/* `extern' with initialization is invalid if not at file scope.  */
	if (current_scope == global_scope)
	  warning ("`%s' initialized and declared `extern'", name);
	else
	  error ("`%s' has both `extern' and initializer", name);
      }
    else if (current_scope == global_scope)
      {
	if (specbits & 1 << (int) RID_AUTO)
	  error ("file-scope declaration of `%s' specifies `auto'", name);
      }
    else
      {
	if (specbits & 1 << (int) RID_EXTERN && funcdef_flag)
	  error ("nested function `%s' declared `extern'", name);
	else if ((specbits & (1 << (int) RID_THREAD
			       | 1 << (int) RID_EXTERN
			       | 1 << (int) RID_STATIC))
		 == (1 << (int) RID_THREAD))
	  {
	    error ("function-scope `%s' implicitly auto and declared `__thread'",
		   name);
	    specbits &= ~(1 << (int) RID_THREAD);
	  }
      }
  }

  /* Now figure out the structure of the declarator proper.
     Descend through it, creating more complex types, until we reach
     the declared identifier (or NULL_TREE, in an absolute declarator).  */

  while (declarator && TREE_CODE (declarator) != IDENTIFIER_NODE)
    {
      if (type == error_mark_node)
	{
	  declarator = TREE_OPERAND (declarator, 0);
	  continue;
	}

      /* Each level of DECLARATOR is either an ARRAY_REF (for ...[..]),
	 an INDIRECT_REF (for *...),
	 a CALL_EXPR (for ...(...)),
	 a TREE_LIST (for nested attributes),
	 an identifier (for the name being declared)
	 or a null pointer (for the place in an absolute declarator
	 where the name was omitted).
	 For the last two cases, we have just exited the loop.

	 At this point, TYPE is the type of elements of an array,
	 or for a function to return, or for a pointer to point to.
	 After this sequence of ifs, TYPE is the type of the
	 array or function or pointer, and DECLARATOR has had its
	 outermost layer removed.  */

      if (array_ptr_quals != NULL_TREE || array_parm_static)
	{
	  /* Only the innermost declarator (making a parameter be of
	     array type which is converted to pointer type)
	     may have static or type qualifiers.  */
	  error ("static or type qualifiers in non-parameter array declarator");
	  array_ptr_quals = NULL_TREE;
	  array_parm_static = 0;
	}

      if (TREE_CODE (declarator) == TREE_LIST)
	{
	  /* We encode a declarator with embedded attributes using
	     a TREE_LIST.  */
	  tree attrs = TREE_PURPOSE (declarator);
	  tree inner_decl;
	  int attr_flags = 0;
	  declarator = TREE_VALUE (declarator);
	  inner_decl = declarator;
	  while (inner_decl != NULL_TREE
		 && TREE_CODE (inner_decl) == TREE_LIST)
	    inner_decl = TREE_VALUE (inner_decl);
	  if (inner_decl == NULL_TREE
	      || TREE_CODE (inner_decl) == IDENTIFIER_NODE)
	    attr_flags |= (int) ATTR_FLAG_DECL_NEXT;
	  else if (TREE_CODE (inner_decl) == CALL_EXPR)
	    attr_flags |= (int) ATTR_FLAG_FUNCTION_NEXT;
	  else if (TREE_CODE (inner_decl) == ARRAY_REF)
	    attr_flags |= (int) ATTR_FLAG_ARRAY_NEXT;
	  returned_attrs = decl_attributes (&type,
					    chainon (returned_attrs, attrs),
					    attr_flags);
	}
      else if (TREE_CODE (declarator) == ARRAY_REF)
	{
	  tree itype = NULL_TREE;
	  tree size = TREE_OPERAND (declarator, 1);
	  /* The index is a signed object `sizetype' bits wide.  */
	  tree index_type = c_common_signed_type (sizetype);

	  array_ptr_quals = TREE_TYPE (declarator);
	  array_parm_static = TREE_STATIC (declarator);

	  declarator = TREE_OPERAND (declarator, 0);

	  /* Check for some types that there cannot be arrays of.  */

	  if (VOID_TYPE_P (type))
	    {
	      error ("declaration of `%s' as array of voids", name);
	      type = error_mark_node;
	    }

	  if (TREE_CODE (type) == FUNCTION_TYPE)
	    {
	      error ("declaration of `%s' as array of functions", name);
	      type = error_mark_node;
	    }

	  if (pedantic && !in_system_header && flexible_array_type_p (type))
	    pedwarn ("invalid use of structure with flexible array member");

	  if (size == error_mark_node)
	    type = error_mark_node;

	  if (type == error_mark_node)
	    continue;

	  /* If size was specified, set ITYPE to a range-type for that size.
	     Otherwise, ITYPE remains null.  finish_decl may figure it out
	     from an initial value.  */

	  if (size)
	    {
	      /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
	      STRIP_TYPE_NOPS (size);

	      if (! INTEGRAL_TYPE_P (TREE_TYPE (size)))
		{
		  error ("size of array `%s' has non-integer type", name);
		  size = integer_one_node;
		}

	      if (pedantic && integer_zerop (size))
		pedwarn ("ISO C forbids zero-size array `%s'", name);

	      if (TREE_CODE (size) == INTEGER_CST)
		{
		  constant_expression_warning (size);
		  if (tree_int_cst_sgn (size) < 0)
		    {
		      error ("size of array `%s' is negative", name);
		      size = integer_one_node;
		    }
		}
	      else
		{
		  /* Make sure the array size remains visibly nonconstant
		     even if it is (eg) a const variable with known value.  */
		  size_varies = 1;

		  if (!flag_isoc99 && pedantic)
		    {
		      if (TREE_CONSTANT (size))
			pedwarn ("ISO C90 forbids array `%s' whose size can't be evaluated",
				 name);
		      else
			pedwarn ("ISO C90 forbids variable-size array `%s'",
				 name);
		    }
		}

	      if (integer_zerop (size))
		{
		  /* A zero-length array cannot be represented with an
		     unsigned index type, which is what we'll get with
		     build_index_type.  Create an open-ended range instead.  */
		  itype = build_range_type (sizetype, size, NULL_TREE);
		}
	      else
		{
		  /* Compute the maximum valid index, that is, size - 1.
		     Do the calculation in index_type, so that if it is
		     a variable the computations will be done in the
		     proper mode.  */
	          itype = fold (build (MINUS_EXPR, index_type,
				       convert (index_type, size),
				       convert (index_type, size_one_node)));

	          /* If that overflowed, the array is too big.
		     ??? While a size of INT_MAX+1 technically shouldn't
		     cause an overflow (because we subtract 1), the overflow
		     is recorded during the conversion to index_type, before
		     the subtraction.  Handling this case seems like an
		     unnecessary complication.  */
		  if (TREE_OVERFLOW (itype))
		    {
		      error ("size of array `%s' is too large", name);
		      type = error_mark_node;
		      continue;
		    }

		  if (size_varies)
		    {
		      /* We must be able to distinguish the
			 SAVE_EXPR_CONTEXT for the variably-sized type
			 so that we can set it correctly in
			 set_save_expr_context.  The convention is
			 that all SAVE_EXPRs that need to be reset
			 have NULL_TREE for their SAVE_EXPR_CONTEXT.  */
		      tree cfd = current_function_decl;
		      if (decl_context == PARM)
			current_function_decl = NULL_TREE;
		      itype = variable_size (itype);
		      if (decl_context == PARM)
			current_function_decl = cfd;
		    }
		  itype = build_index_type (itype);
		}
	    }
	  else if (decl_context == FIELD)
	    {
	      if (pedantic && !flag_isoc99 && !in_system_header)
		pedwarn ("ISO C90 does not support flexible array members");

	      /* ISO C99 Flexible array members are effectively identical
		 to GCC's zero-length array extension.  */
	      itype = build_range_type (sizetype, size_zero_node, NULL_TREE);
	    }

	  /* If pedantic, complain about arrays of incomplete types.  */

	  if (pedantic && !COMPLETE_TYPE_P (type))
	    pedwarn ("array type has incomplete element type");

	  /* Build the array type itself, then merge any constancy or
	     volatility into the target type.  We must do it in this order
	     to ensure that the TYPE_MAIN_VARIANT field of the array type
	     is set correctly.  */

	  type = build_array_type (type, itype);
	  if (type_quals)
	    type = c_build_qualified_type (type, type_quals);

	  if (size_varies)
	    C_TYPE_VARIABLE_SIZE (type) = 1;

	  /* The GCC extension for zero-length arrays differs from
	     ISO flexible array members in that sizeof yields zero.  */
	  if (size && integer_zerop (size))
	    {
	      layout_type (type);
	      TYPE_SIZE (type) = bitsize_zero_node;
	      TYPE_SIZE_UNIT (type) = size_zero_node;
	    }
	  if (decl_context != PARM
	      && (array_ptr_quals != NULL_TREE || array_parm_static))
	    {
	      error ("static or type qualifiers in non-parameter array declarator");
	      array_ptr_quals = NULL_TREE;
	      array_parm_static = 0;
	    }
	}
      else if (TREE_CODE (declarator) == CALL_EXPR)
	{
	  /* Say it's a definition only for the declarator closest to
	     the identifier, apart possibly from some attributes.  */
	  bool really_funcdef = false;
	  tree arg_types;
	  if (funcdef_flag)
	    {
	      tree t = TREE_OPERAND (declarator, 0);
	      while (TREE_CODE (t) == TREE_LIST)
		t = TREE_VALUE (t);
	      really_funcdef = (TREE_CODE (t) == IDENTIFIER_NODE);
	    }

	  /* Declaring a function type.
	     Make sure we have a valid type for the function to return.  */
	  if (type == error_mark_node)
	    continue;

	  size_varies = 0;

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

	  /* Construct the function type and go to the next
	     inner layer of declarator.  */

	  arg_types = grokparms (TREE_OPERAND (declarator, 1),
				 really_funcdef);
	  /* Type qualifiers before the return type of the function
	     qualify the return type, not the function type.  */
	  if (type_quals)
	    {
	      /* Type qualifiers on a function return type are normally
		 permitted by the standard but have no effect, so give a
		 warning at -Wextra.  Qualifiers on a void return type have
		 meaning as a GNU extension, and are banned on function
		 definitions in ISO C.  FIXME: strictly we shouldn't
		 pedwarn for qualified void return types except on function
		 definitions, but not doing so could lead to the undesirable
		 state of a "volatile void" function return type not being
		 warned about, and a use of the function being compiled
		 with GNU semantics, with no diagnostics under -pedantic.  */
	      if (VOID_TYPE_P (type) && pedantic && !in_system_header)
		pedwarn ("ISO C forbids qualified void function return type");
	      else if (extra_warnings
		       && !(VOID_TYPE_P (type)
			    && type_quals == TYPE_QUAL_VOLATILE))
		warning ("type qualifiers ignored on function return type");

	      type = c_build_qualified_type (type, type_quals);
	    }
	  type_quals = TYPE_UNQUALIFIED;

	  type = build_function_type (type, arg_types);
	  declarator = TREE_OPERAND (declarator, 0);

	  /* Set the TYPE_CONTEXTs for each tagged type which is local to
	     the formal parameter list of this FUNCTION_TYPE to point to
	     the FUNCTION_TYPE node itself.  */

	  {
	    tree link;

	    for (link = last_function_parm_tags;
		 link;
		 link = TREE_CHAIN (link))
	      TYPE_CONTEXT (TREE_VALUE (link)) = type;
	  }
	}
      else if (TREE_CODE (declarator) == INDIRECT_REF)
	{
	  /* Merge any constancy or volatility into the target type
	     for the pointer.  */

	  if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	      && type_quals)
	    pedwarn ("ISO C forbids qualified function types");
	  if (type_quals)
	    type = c_build_qualified_type (type, type_quals);
	  type_quals = TYPE_UNQUALIFIED;
	  size_varies = 0;

	  type = build_pointer_type (type);

	  /* Process a list of type modifier keywords
	     (such as const or volatile) that were given inside the `*'.  */

	  if (TREE_TYPE (declarator))
	    {
	      tree typemodlist;
	      int erred = 0;

	      constp = 0;
	      volatilep = 0;
	      restrictp = 0;
	      for (typemodlist = TREE_TYPE (declarator); typemodlist;
		   typemodlist = TREE_CHAIN (typemodlist))
		{
		  tree qualifier = TREE_VALUE (typemodlist);

		  if (C_IS_RESERVED_WORD (qualifier))
		    {
		      if (C_RID_CODE (qualifier) == RID_CONST)
			constp++;
		      else if (C_RID_CODE (qualifier) == RID_VOLATILE)
			volatilep++;
		      else if (C_RID_CODE (qualifier) == RID_RESTRICT)
			restrictp++;
		      else
			erred++;
		    }
		  else
		    erred++;
		}

	      if (erred)
		error ("invalid type modifier within pointer declarator");
	      if (pedantic && !flag_isoc99)
		{
		  if (constp > 1)
		    pedwarn ("duplicate `const'");
		  if (volatilep > 1)
		    pedwarn ("duplicate `volatile'");
		  if (restrictp > 1)
		    pedwarn ("duplicate `restrict'");
		}

	      type_quals = ((constp ? TYPE_QUAL_CONST : 0)
			    | (restrictp ? TYPE_QUAL_RESTRICT : 0)
			    | (volatilep ? TYPE_QUAL_VOLATILE : 0));
	    }

	  declarator = TREE_OPERAND (declarator, 0);
	}
      else
	abort ();

    }

  /* Now TYPE has the actual type.  */

  /* Check the type and width of a bit-field.  */
  if (bitfield)
    check_bitfield_type_and_width (&type, width, orig_name);

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

  /* If this is declaring a typedef name, return a TYPE_DECL.  */

  if (specbits & (1 << (int) RID_TYPEDEF))
    {
      tree decl;
      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters */
      if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	  && type_quals)
	pedwarn ("ISO C forbids qualified function types");
      if (type_quals)
	type = c_build_qualified_type (type, type_quals);
      decl = build_decl (TYPE_DECL, declarator, type);
      if ((specbits & (1 << (int) RID_SIGNED))
	  || (typedef_decl && C_TYPEDEF_EXPLICITLY_SIGNED (typedef_decl)))
	C_TYPEDEF_EXPLICITLY_SIGNED (decl) = 1;
      decl_attributes (&decl, returned_attrs, 0);
      return decl;
    }

  /* Detect the case of an array type of unspecified size
     which came, as such, direct from a typedef name.
     We must copy the type, so that each identifier gets
     a distinct type, so that each identifier's size can be
     controlled separately by its own initializer.  */

  if (type != 0 && typedef_type != 0
      && TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type) == 0
      && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (typedef_type))
    {
      type = build_array_type (TREE_TYPE (type), 0);
      if (size_varies)
	C_TYPE_VARIABLE_SIZE (type) = 1;
    }

  /* If this is a type name (such as, in a cast or sizeof),
     compute the type and return it now.  */

  if (decl_context == TYPENAME)
    {
      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters */
      if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	  && type_quals)
	pedwarn ("ISO C forbids const or volatile function types");
      if (type_quals)
	type = c_build_qualified_type (type, type_quals);
      decl_attributes (&type, returned_attrs, 0);
      return type;
    }

  /* Aside from typedefs and type names (handle above),
     `void' at top level (not within pointer)
     is allowed only in public variables.
     We don't complain about parms either, but that is because
     a better error message can be made later.  */

  if (VOID_TYPE_P (type) && decl_context != PARM
      && ! ((decl_context != FIELD && TREE_CODE (type) != FUNCTION_TYPE)
	    && ((specbits & (1 << (int) RID_EXTERN))
		|| (current_scope == global_scope
		    && !(specbits
			 & ((1 << (int) RID_STATIC) | (1 << (int) RID_REGISTER)))))))
    {
      error ("variable or field `%s' declared void", name);
      type = integer_type_node;
    }

  /* Now create the decl, which may be a VAR_DECL, a PARM_DECL
     or a FUNCTION_DECL, depending on DECL_CONTEXT and TYPE.  */

  {
    tree decl;

    if (decl_context == PARM)
      {
	tree type_as_written;
	tree promoted_type;

	/* A parameter declared as an array of T is really a pointer to T.
	   One declared as a function is really a pointer to a function.  */

	if (TREE_CODE (type) == ARRAY_TYPE)
	  {
	    /* Transfer const-ness of array into that of type pointed to.  */
	    type = TREE_TYPE (type);
	    if (type_quals)
	      type = c_build_qualified_type (type, type_quals);
	    type = build_pointer_type (type);
	    type_quals = TYPE_UNQUALIFIED;
	    if (array_ptr_quals)
	      {
		tree new_ptr_quals, new_ptr_attrs;
		int erred = 0;
		split_specs_attrs (array_ptr_quals, &new_ptr_quals, &new_ptr_attrs);
		/* We don't yet implement attributes in this context.  */
		if (new_ptr_attrs != NULL_TREE)
		  warning ("attributes in parameter array declarator ignored");

		constp = 0;
		volatilep = 0;
		restrictp = 0;
		for (; new_ptr_quals; new_ptr_quals = TREE_CHAIN (new_ptr_quals))
		  {
		    tree qualifier = TREE_VALUE (new_ptr_quals);

		    if (C_IS_RESERVED_WORD (qualifier))
		      {
			if (C_RID_CODE (qualifier) == RID_CONST)
			  constp++;
			else if (C_RID_CODE (qualifier) == RID_VOLATILE)
			  volatilep++;
			else if (C_RID_CODE (qualifier) == RID_RESTRICT)
			  restrictp++;
			else
			  erred++;
		      }
		    else
		      erred++;
		  }

		if (erred)
		  error ("invalid type modifier within array declarator");

		type_quals = ((constp ? TYPE_QUAL_CONST : 0)
			      | (restrictp ? TYPE_QUAL_RESTRICT : 0)
			      | (volatilep ? TYPE_QUAL_VOLATILE : 0));
	      }
	    size_varies = 0;
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    if (pedantic && type_quals)
	      pedwarn ("ISO C forbids qualified function types");
	    if (type_quals)
	      type = c_build_qualified_type (type, type_quals);
	    type = build_pointer_type (type);
	    type_quals = TYPE_UNQUALIFIED;
	  }
	else if (type_quals)
	  type = c_build_qualified_type (type, type_quals);

	type_as_written = type;

	decl = build_decl (PARM_DECL, declarator, type);
	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;

	/* Compute the type actually passed in the parmlist,
	   for the case where there is no prototype.
	   (For example, shorts and chars are passed as ints.)
	   When there is a prototype, this is overridden later.  */

	if (type == error_mark_node)
	  promoted_type = type;
	else
	  promoted_type = c_type_promotes_to (type);

	DECL_ARG_TYPE (decl) = promoted_type;
	DECL_ARG_TYPE_AS_WRITTEN (decl) = type_as_written;
      }
    else if (decl_context == FIELD)
      {
	/* Structure field.  It may not be a function.  */

	if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    error ("field `%s' declared as a function", name);
	    type = build_pointer_type (type);
	  }
	else if (TREE_CODE (type) != ERROR_MARK
	         && !COMPLETE_OR_UNBOUND_ARRAY_TYPE_P (type))
	  {
	    error ("field `%s' has incomplete type", name);
	    type = error_mark_node;
	  }
	/* Move type qualifiers down to element of an array.  */
	if (TREE_CODE (type) == ARRAY_TYPE && type_quals)
	  type = build_array_type (c_build_qualified_type (TREE_TYPE (type),
							   type_quals),
				   TYPE_DOMAIN (type));
	decl = build_decl (FIELD_DECL, declarator, type);
	DECL_NONADDRESSABLE_P (decl) = bitfield;

	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;
      }
    else if (TREE_CODE (type) == FUNCTION_TYPE)
      {
	/* Every function declaration is "external"
	   except for those which are inside a function body
	   in which `auto' is used.
	   That is a case not specified by ANSI C,
	   and we use it for forward declarations for nested functions.  */
	int extern_ref = (!(specbits & (1 << (int) RID_AUTO))
			  || current_scope == global_scope);

	if (specbits & (1 << (int) RID_AUTO)
	    && (pedantic || current_scope == global_scope))
	  pedwarn ("invalid storage class for function `%s'", name);
	if (specbits & (1 << (int) RID_REGISTER))
	  error ("invalid storage class for function `%s'", name);
	if (specbits & (1 << (int) RID_THREAD))
	  error ("invalid storage class for function `%s'", name);
	/* Function declaration not at file scope.
	   Storage classes other than `extern' are not allowed
	   and `extern' makes no difference.  */
	if (current_scope != global_scope
	    && (specbits & ((1 << (int) RID_STATIC) | (1 << (int) RID_INLINE)))
	    && pedantic)
	  pedwarn ("invalid storage class for function `%s'", name);

	decl = build_decl (FUNCTION_DECL, declarator, type);
	decl = build_decl_attribute_variant (decl, decl_attr);

	DECL_LANG_SPECIFIC (decl)
	  = ggc_alloc_cleared (sizeof (struct lang_decl));

	if (pedantic && type_quals && ! DECL_IN_SYSTEM_HEADER (decl))
	  pedwarn ("ISO C forbids qualified function types");

	/* GNU C interprets a `volatile void' return type to indicate
	   that the function does not return.  */
	if ((type_quals & TYPE_QUAL_VOLATILE)
	    && !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
	  warning ("`noreturn' function returns non-void value");

	if (extern_ref)
	  DECL_EXTERNAL (decl) = 1;
	/* Record absence of global scope for `static' or `auto'.  */
	TREE_PUBLIC (decl)
	  = !(specbits & ((1 << (int) RID_STATIC) | (1 << (int) RID_AUTO)));

	if (defaulted_int)
	  C_FUNCTION_IMPLICIT_INT (decl) = 1;

	/* Record presence of `inline', if it is reasonable.  */
	if (MAIN_NAME_P (declarator))
	  {
	    if (inlinep)
	      warning ("cannot inline function `main'");
	  }
	else if (inlinep)
	  {
	    /* Record that the function is declared `inline'.  */
	    DECL_DECLARED_INLINE_P (decl) = 1;

	    /* Do not mark bare declarations as DECL_INLINE.  Doing so
	       in the presence of multiple declarations can result in
	       the abstract origin pointing between the declarations,
	       which will confuse dwarf2out.  */
	    if (initialized)
	      {
		DECL_INLINE (decl) = 1;
		if (specbits & (1 << (int) RID_EXTERN))
		  current_extern_inline = 1;
	      }
	  }
	/* If -finline-functions, assume it can be inlined.  This does
	   two things: let the function be deferred until it is actually
	   needed, and let dwarf2 know that the function is inlinable.  */
	else if (flag_inline_trees == 2 && initialized)
	  DECL_INLINE (decl) = 1;
      }
    else
      {
	/* It's a variable.  */
	/* An uninitialized decl with `extern' is a reference.  */
	int extern_ref = !initialized && (specbits & (1 << (int) RID_EXTERN));

	/* Move type qualifiers down to element of an array.  */
	if (TREE_CODE (type) == ARRAY_TYPE && type_quals)
	  {
	    int saved_align = TYPE_ALIGN(type);
	    type = build_array_type (c_build_qualified_type (TREE_TYPE (type),
							     type_quals),
				     TYPE_DOMAIN (type));
	    TYPE_ALIGN (type) = saved_align;
	  }
	else if (type_quals)
	  type = c_build_qualified_type (type, type_quals);

	/* It is invalid to create an `extern' declaration for a
	   variable if there is a global declaration that is
	   `static' and the global declaration is not visible.  */
	if (extern_ref && current_scope != global_scope)
	  {
	    tree global_decl;

	    global_decl = identifier_global_value (declarator);
	    if (global_decl
		&& TREE_CODE (global_decl) == VAR_DECL
		&& lookup_name (declarator) != global_decl
		&& !TREE_PUBLIC (global_decl))
	      error ("variable previously declared `static' redeclared "
		     "`extern'");
	  }

	decl = build_decl (VAR_DECL, declarator, type);
	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;

	if (inlinep)
	  pedwarn ("%Jvariable '%D' declared `inline'", decl, decl);

	DECL_EXTERNAL (decl) = extern_ref;

	/* At file scope, the presence of a `static' or `register' storage
	   class specifier, or the absence of all storage class specifiers
	   makes this declaration a definition (perhaps tentative).  Also,
	   the absence of both `static' and `register' makes it public.  */
	if (current_scope == global_scope)
	  {
	    TREE_PUBLIC (decl) = !(specbits & ((1 << (int) RID_STATIC)
					       | (1 << (int) RID_REGISTER)));
	    TREE_STATIC (decl) = !extern_ref;
	  }
	/* Not at file scope, only `static' makes a static definition.  */
	else
	  {
	    TREE_STATIC (decl) = (specbits & (1 << (int) RID_STATIC)) != 0;
	    TREE_PUBLIC (decl) = extern_ref;
	  }

	if (specbits & 1 << (int) RID_THREAD)
	  {
	    if (targetm.have_tls)
	      DECL_THREAD_LOCAL (decl) = 1;
	    else
	      /* A mere warning is sure to result in improper semantics
		 at runtime.  Don't bother to allow this to compile.  */
	      error ("thread-local storage not supported for this target");
	  }
      }

    /* Record `register' declaration for warnings on &
       and in case doing stupid register allocation.  */

    if (specbits & (1 << (int) RID_REGISTER))
      DECL_REGISTER (decl) = 1;

    /* Record constancy and volatility.  */
    c_apply_type_quals_to_decl (type_quals, decl);

    /* If a type has volatile components, it should be stored in memory.
       Otherwise, the fact that those components are volatile
       will be ignored, and would even crash the compiler.  */
    if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (decl)))
      c_mark_addressable (decl);

#ifdef ENABLE_CHECKING
  /* This is the earliest point at which we might know the assembler
     name of a variable.  Thus, if it's known before this, die horribly.  */
  if (DECL_ASSEMBLER_NAME_SET_P (decl))
    abort ();
#endif

    decl_attributes (&decl, returned_attrs, 0);

    return decl;
  }
}

/* Decode the parameter-list info for a function type or function definition.
   The argument is the value returned by `get_parm_info' (or made in parse.y
   if there is an identifier list instead of a parameter decl list).
   These two functions are separate because when a function returns
   or receives functions then each is called multiple times but the order
   of calls is different.  The last call to `grokparms' is always the one
   that contains the formal parameter names of a function definition.

   Store in `last_function_parms' a chain of the decls of parms.
   Also store in `last_function_parm_tags' a chain of the struct, union,
   and enum tags declared among the parms.

   Return a list of arg types to use in the FUNCTION_TYPE for this function.

   FUNCDEF_FLAG is nonzero for a function definition, 0 for
   a mere declaration.  A nonempty identifier-list gets an error message
   when FUNCDEF_FLAG is zero.  */

static tree
grokparms (tree parms_info, int funcdef_flag)
{
  tree first_parm = TREE_CHAIN (parms_info);

  last_function_parms = TREE_PURPOSE (parms_info);
  last_function_parm_tags = TREE_VALUE (parms_info);
  last_function_parm_others = TREE_TYPE (parms_info);

  if (warn_strict_prototypes && first_parm == 0 && !funcdef_flag
      && !in_system_header)
    warning ("function declaration isn't a prototype");

  if (first_parm != 0
      && TREE_CODE (TREE_VALUE (first_parm)) == IDENTIFIER_NODE)
    {
      if (! funcdef_flag)
	pedwarn ("parameter names (without types) in function declaration");

      last_function_parms = first_parm;
      return 0;
    }
  else
    {
      tree parm;
      tree typelt;
      /* If the arg types are incomplete in a declaration,
	 they must include undefined tags.
	 These tags can never be defined in the scope of the declaration,
	 so the types can never be completed,
	 and no call can be compiled successfully.  */

      for (parm = last_function_parms, typelt = first_parm;
	   parm;
	   parm = TREE_CHAIN (parm))
	/* Skip over any enumeration constants declared here.  */
	if (TREE_CODE (parm) == PARM_DECL)
	  {
	    /* Barf if the parameter itself has an incomplete type.  */
	    tree type = TREE_VALUE (typelt);
	    if (type == error_mark_node)
	      continue;
	    if (!COMPLETE_TYPE_P (type))
	      {
		if (funcdef_flag && DECL_NAME (parm) != 0)
		  error ("parameter `%s' has incomplete type",
			 IDENTIFIER_POINTER (DECL_NAME (parm)));
		else
		  warning ("parameter has incomplete type");
		if (funcdef_flag)
		  {
		    TREE_VALUE (typelt) = error_mark_node;
		    TREE_TYPE (parm) = error_mark_node;
		  }
	      }
	    typelt = TREE_CHAIN (typelt);
	  }

      return first_parm;
    }
}

/* Return a tree_list node with info on a parameter list just parsed.
   The TREE_PURPOSE is a list of decls of those parms.
   The TREE_VALUE is a list of structure, union and enum tags defined.
   The TREE_CHAIN is a list of argument types to go in the FUNCTION_TYPE.
   The TREE_TYPE is a list of non-parameter decls which appeared with the
   parameters.
   This tree_list node is later fed to `grokparms'.

   VOID_AT_END nonzero means append `void' to the end of the type-list.
   Zero means the parmlist ended with an ellipsis so don't append `void'.  */

tree
get_parm_info (int void_at_end)
{
  tree decl, type, list;
  tree types = 0;
  tree *last_type = &types;
  tree tags = current_scope->tags;
  tree parms = current_scope->parms;
  tree others = current_scope->names;
  static bool explained_incomplete_types = false;
  bool gave_void_only_once_err = false;

  /* Just "void" (and no ellipsis) is special.  There are really no parms.
     But if the "void" is qualified (by "const" or "volatile"), or has a
     storage class specifier ("register"), then the behavior is undefined;
     issue an error.  Typedefs for "void" are OK (see DR#157).  */
  if (void_at_end && parms != 0
      && TREE_CHAIN (parms) == 0
      && VOID_TYPE_P (TREE_TYPE (parms))
      && !DECL_NAME (parms))
    {
      if (TREE_THIS_VOLATILE (parms)
	  || TREE_READONLY (parms)
	  || DECL_REGISTER (parms))
	error ("\"void\" as only parameter may not be qualified");

      return tree_cons (0, 0, tree_cons (0, void_type_node, 0));
    }

  /* Sanity check all of the parameter declarations.  */
  for (decl = parms; decl; decl = TREE_CHAIN (decl))
    {
      if (TREE_CODE (decl) != PARM_DECL)
	abort ();
      if (TREE_ASM_WRITTEN (decl))
	abort ();

      /* Since there is a prototype, args are passed in their
	 declared types.  The back end may override this.  */
      type = TREE_TYPE (decl);
      DECL_ARG_TYPE (decl) = type;

      /* Check for (..., void, ...) and issue an error.  */
      if (VOID_TYPE_P (type) && !DECL_NAME (decl) && !gave_void_only_once_err)
	{
	  error ("\"void\" must be the only parameter");
	  gave_void_only_once_err = true;
	}

      type = build_tree_list (0, type);
      *last_type = type;
      last_type = &TREE_CHAIN (type);
    }

  /* Check the list of non-parameter decls for any forward parm decls
     that never got real decls.  */
  for (decl = others; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == PARM_DECL)
      {
	if (!TREE_ASM_WRITTEN (decl))
	  abort ();

	  error ("%Jparameter \"%D\" has just a forward declaration",
		 decl, decl);
      }

  /* Warn about any struct, union or enum tags defined within this
     list.  The scope of such types is limited to this declaration,
     which is rarely if ever desirable (it's impossible to call such
     a function with type-correct arguments).  */
  for (decl = tags; decl; decl = TREE_CHAIN (decl))
    {
      enum tree_code code = TREE_CODE (TREE_VALUE (decl));
      const char *keyword;
      /* An anonymous union parm type is meaningful as a GNU extension.
	 So don't warn for that.  */
      if (code == UNION_TYPE && TREE_PURPOSE (decl) == 0 && !pedantic)
	continue;

      /* The keyword should not be translated.  */
      switch (code)
	{
	case RECORD_TYPE:   keyword = "struct"; break;
	case UNION_TYPE:    keyword = "union";  break;
	case ENUMERAL_TYPE: keyword = "enum";   break;
	default: abort ();
	}

      if (TREE_PURPOSE (decl))
	/* The first %s will be one of 'struct', 'union', or 'enum'.  */
	warning ("\"%s %s\" declared inside parameter list",
		 keyword, IDENTIFIER_POINTER (TREE_PURPOSE (decl)));
      else
	/* The %s will be one of 'struct', 'union', or 'enum'.  */
	warning ("anonymous %s declared inside parameter list", keyword);

      if (! explained_incomplete_types)
	{
	  warning ("its scope is only this definition or declaration,"
		   " which is probably not what you want");
	  explained_incomplete_types = true;
	}
    }


  if (void_at_end)
    {
      type = build_tree_list (0, void_type_node);
      *last_type = type;
    }

  list = tree_cons (parms, tags, types);
  TREE_TYPE (list) = others;
  return list;
}

/* Get the struct, enum or union (CODE says which) with tag NAME.
   Define the tag as a forward-reference if it is not defined.  */

tree
xref_tag (enum tree_code code, tree name)
{
  /* If a cross reference is requested, look up the type
     already defined for this tag and return it.  */

  tree ref = lookup_tag (code, name, 0);
  /* If this is the right type of tag, return what we found.
     (This reference will be shadowed by shadow_tag later if appropriate.)
     If this is the wrong type of tag, do not return it.  If it was the
     wrong type in the same scope, we will have had an error
     message already; if in a different scope and declaring
     a name, pending_xref_error will give an error message; but if in a
     different scope and not declaring a name, this tag should
     shadow the previous declaration of a different type of tag, and
     this would not work properly if we return the reference found.
     (For example, with "struct foo" in an outer scope, "union foo;"
     must shadow that tag with a new one of union type.)  */
  if (ref && TREE_CODE (ref) == code)
    return ref;

  /* If no such tag is yet defined, create a forward-reference node
     and record it as the "definition".
     When a real declaration of this type is found,
     the forward-reference will be altered into a real type.  */

  ref = make_node (code);
  if (code == ENUMERAL_TYPE)
    {
      /* Give the type a default layout like unsigned int
	 to avoid crashing if it does not get defined.  */
      TYPE_MODE (ref) = TYPE_MODE (unsigned_type_node);
      TYPE_ALIGN (ref) = TYPE_ALIGN (unsigned_type_node);
      TYPE_USER_ALIGN (ref) = 0;
      TREE_UNSIGNED (ref) = 1;
      TYPE_PRECISION (ref) = TYPE_PRECISION (unsigned_type_node);
      TYPE_MIN_VALUE (ref) = TYPE_MIN_VALUE (unsigned_type_node);
      TYPE_MAX_VALUE (ref) = TYPE_MAX_VALUE (unsigned_type_node);
    }

  pushtag (name, ref);

  return ref;
}

/* Make sure that the tag NAME is defined *in the current scope*
   at least as a forward reference.
   CODE says which kind of tag NAME ought to be.  */

tree
start_struct (enum tree_code code, tree name)
{
  /* If there is already a tag defined at this scope
     (as a forward reference), just return it.  */

  tree ref = 0;

  if (name != 0)
    ref = lookup_tag (code, name, 1);
  if (ref && TREE_CODE (ref) == code)
    {
      if (TYPE_FIELDS (ref))
        {
	  if (code == UNION_TYPE)
	    error ("redefinition of `union %s'", IDENTIFIER_POINTER (name));
          else
	    error ("redefinition of `struct %s'", IDENTIFIER_POINTER (name));
	}
    }
  else
    {
      /* Otherwise create a forward-reference just so the tag is in scope.  */

      ref = make_node (code);
      pushtag (name, ref);
    }

  C_TYPE_BEING_DEFINED (ref) = 1;
  TYPE_PACKED (ref) = flag_pack_struct;
  return ref;
}

/* Process the specs, declarator (NULL if omitted) and width (NULL if omitted)
   of a structure component, returning a FIELD_DECL node.
   WIDTH is non-NULL for bit-fields only, and is an INTEGER_CST node.

   This is done during the parsing of the struct declaration.
   The FIELD_DECL nodes are chained together and the lot of them
   are ultimately passed to `build_struct' to make the RECORD_TYPE node.  */

tree
grokfield (tree declarator, tree declspecs, tree width)
{
  tree value;

  if (declarator == NULL_TREE && width == NULL_TREE)
    {
      /* This is an unnamed decl.

	 If we have something of the form "union { list } ;" then this
	 is the anonymous union extension.  Similarly for struct.

	 If this is something of the form "struct foo;", then
	   If MS extensions are enabled, this is handled as an
	     anonymous struct.
	   Otherwise this is a forward declaration of a structure tag.

	 If this is something of the form "foo;" and foo is a TYPE_DECL, then
	   If MS extensions are enabled and foo names a structure, then
	     again this is an anonymous struct.
	   Otherwise this is an error.

	 Oh what a horrid tangled web we weave.  I wonder if MS consciously
	 took this from Plan 9 or if it was an accident of implementation
	 that took root before someone noticed the bug...  */

      tree type = TREE_VALUE (declspecs);

      if (flag_ms_extensions && TREE_CODE (type) == TYPE_DECL)
	type = TREE_TYPE (type);
      if (TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE)
	{
	  if (flag_ms_extensions)
	    ; /* ok */
	  else if (flag_iso)
	    goto warn_unnamed_field;
	  else if (TYPE_NAME (type) == NULL)
	    ; /* ok */
	  else
	    goto warn_unnamed_field;
	}
      else
	{
	warn_unnamed_field:
	  warning ("declaration does not declare anything");
	  return NULL_TREE;
	}
    }

  value = grokdeclarator (declarator, declspecs, FIELD, 0,
			  width ? &width : NULL);

  finish_decl (value, NULL_TREE, NULL_TREE);
  DECL_INITIAL (value) = width;

  return value;
}

/* Generate an error for any duplicate field names in FIELDLIST.  Munge
   the list such that this does not present a problem later.  */

static void
detect_field_duplicates (tree fieldlist)
{
  tree x, y;
  int timeout = 10;

  /* First, see if there are more than "a few" fields.
     This is trivially true if there are zero or one fields.  */
  if (!fieldlist)
    return;
  x = TREE_CHAIN (fieldlist);
  if (!x)
    return;
  do {
    timeout--;
    x = TREE_CHAIN (x);
  } while (timeout > 0 && x);

  /* If there were "few" fields, avoid the overhead of allocating
     a hash table.  Instead just do the nested traversal thing.  */
  if (timeout > 0)
    {
      for (x = TREE_CHAIN (fieldlist); x ; x = TREE_CHAIN (x))
	if (DECL_NAME (x))
	  {
	    for (y = fieldlist; y != x; y = TREE_CHAIN (y))
	      if (DECL_NAME (y) == DECL_NAME (x))
		{
		  error ("%Jduplicate member '%D'", x, x);
		  DECL_NAME (x) = NULL_TREE;
		}
	  }
    }
  else
    {
      htab_t htab = htab_create (37, htab_hash_pointer, htab_eq_pointer, NULL);
      void **slot;

      for (x = fieldlist; x ; x = TREE_CHAIN (x))
	if ((y = DECL_NAME (x)) != 0)
	  {
	    slot = htab_find_slot (htab, y, INSERT);
	    if (*slot)
	      {
		error ("%Jduplicate member '%D'", x, x);
		DECL_NAME (x) = NULL_TREE;
	      }
	    *slot = y;
	  }

      htab_delete (htab);
    }
}

/* Fill in the fields of a RECORD_TYPE or UNION_TYPE node, T.
   FIELDLIST is a chain of FIELD_DECL nodes for the fields.
   ATTRIBUTES are attributes to be applied to the structure.  */

tree
finish_struct (tree t, tree fieldlist, tree attributes)
{
  tree x;
  int toplevel = global_scope == current_scope;
  int saw_named_field;

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */

  TYPE_SIZE (t) = 0;

  decl_attributes (&t, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);

  /* Nameless union parm types are useful as GCC extension.  */
  if (! (TREE_CODE (t) == UNION_TYPE && TYPE_NAME (t) == 0) && !pedantic)
    /* Otherwise, warn about any struct or union def. in parmlist.  */
    if (in_parm_level_p ())
      {
	if (pedantic)
	  pedwarn ("%s defined inside parms",
		   TREE_CODE (t) == UNION_TYPE ? _("union") : _("structure"));
	else
	  warning ("%s defined inside parms",
		   TREE_CODE (t) == UNION_TYPE ? _("union") : _("structure"));
      }

  if (pedantic)
    {
      for (x = fieldlist; x; x = TREE_CHAIN (x))
	if (DECL_NAME (x) != 0)
	  break;

      if (x == 0)
	pedwarn ("%s has no %s",
		 TREE_CODE (t) == UNION_TYPE ? _("union") : _("struct"),
		 fieldlist ? _("named members") : _("members"));
    }

  /* Install struct as DECL_CONTEXT of each field decl.
     Also process specified field sizes,m which is found in the DECL_INITIAL.
     Store 0 there, except for ": 0" fields (so we can find them
     and delete them, below).  */

  saw_named_field = 0;
  for (x = fieldlist; x; x = TREE_CHAIN (x))
    {
      DECL_CONTEXT (x) = t;
      DECL_PACKED (x) |= TYPE_PACKED (t);

      /* If any field is const, the structure type is pseudo-const.  */
      if (TREE_READONLY (x))
	C_TYPE_FIELDS_READONLY (t) = 1;
      else
	{
	  /* A field that is pseudo-const makes the structure likewise.  */
	  tree t1 = TREE_TYPE (x);
	  while (TREE_CODE (t1) == ARRAY_TYPE)
	    t1 = TREE_TYPE (t1);
	  if ((TREE_CODE (t1) == RECORD_TYPE || TREE_CODE (t1) == UNION_TYPE)
	      && C_TYPE_FIELDS_READONLY (t1))
	    C_TYPE_FIELDS_READONLY (t) = 1;
	}

      /* Any field that is volatile means variables of this type must be
	 treated in some ways as volatile.  */
      if (TREE_THIS_VOLATILE (x))
	C_TYPE_FIELDS_VOLATILE (t) = 1;

      /* Any field of nominal variable size implies structure is too.  */
      if (C_DECL_VARIABLE_SIZE (x))
	C_TYPE_VARIABLE_SIZE (t) = 1;

      /* Detect invalid nested redefinition.  */
      if (TREE_TYPE (x) == t)
	error ("nested redefinition of `%s'",
	       IDENTIFIER_POINTER (TYPE_NAME (t)));

      if (DECL_INITIAL (x))
	{
	  unsigned HOST_WIDE_INT width = tree_low_cst (DECL_INITIAL (x), 1);
	  DECL_SIZE (x) = bitsize_int (width);
	  DECL_BIT_FIELD (x) = 1;
	  SET_DECL_C_BIT_FIELD (x);
	}

      DECL_INITIAL (x) = 0;

      /* Detect flexible array member in an invalid context.  */
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	  && TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
	  && TYPE_DOMAIN (TREE_TYPE (x)) != NULL_TREE
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (x))) == NULL_TREE)
	{
	  if (TREE_CODE (t) == UNION_TYPE)
	    {
	      error ("%Jflexible array member in union", x);
	      TREE_TYPE (x) = error_mark_node;
	    }
	  else if (TREE_CHAIN (x) != NULL_TREE)
	    {
	      error ("%Jflexible array member not at end of struct", x);
	      TREE_TYPE (x) = error_mark_node;
	    }
	  else if (! saw_named_field)
	    {
	      error ("%Jflexible array member in otherwise empty struct", x);
	      TREE_TYPE (x) = error_mark_node;
	    }
	}

      if (pedantic && !in_system_header && TREE_CODE (t) == RECORD_TYPE
	  && flexible_array_type_p (TREE_TYPE (x)))
	pedwarn ("%Jinvalid use of structure with flexible array member", x);

      if (DECL_NAME (x))
	saw_named_field = 1;
    }

  detect_field_duplicates (fieldlist);

  /* Now we have the nearly final fieldlist.  Record it,
     then lay out the structure or union (including the fields).  */

  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);

  /* Delete all zero-width bit-fields from the fieldlist.  */
  {
    tree *fieldlistp = &fieldlist;
    while (*fieldlistp)
      if (TREE_CODE (*fieldlistp) == FIELD_DECL && DECL_INITIAL (*fieldlistp))
	*fieldlistp = TREE_CHAIN (*fieldlistp);
      else
	fieldlistp = &TREE_CHAIN (*fieldlistp);
  }

  /* Now we have the truly final field list.
     Store it in this type and in the variants.  */

  TYPE_FIELDS (t) = fieldlist;

  /* If there are lots of fields, sort so we can look through them fast.
     We arbitrarily consider 16 or more elts to be "a lot".  */

  {
    int len = 0;

    for (x = fieldlist; x; x = TREE_CHAIN (x))
      {
        if (len > 15 || DECL_NAME (x) == NULL)
          break;
        len += 1;
      }

    if (len > 15)
      {
        tree *field_array;
        struct lang_type *space;
        struct sorted_fields_type *space2;

        len += list_length (x);

        /* Use the same allocation policy here that make_node uses, to
          ensure that this lives as long as the rest of the struct decl.
          All decls in an inline function need to be saved.  */

        space = ggc_alloc (sizeof (struct lang_type));
        space2 = ggc_alloc (sizeof (struct sorted_fields_type) + len * sizeof (tree));

        len = 0;
	space->s = space2;
	field_array = &space2->elts[0];
        for (x = fieldlist; x; x = TREE_CHAIN (x))
          {
            field_array[len++] = x;

            /* If there is anonymous struct or union, break out of the loop.  */
            if (DECL_NAME (x) == NULL)
              break;
          }
        /* Found no anonymous struct/union.  Add the TYPE_LANG_SPECIFIC.  */
        if (x == NULL)
          {
            TYPE_LANG_SPECIFIC (t) = space;
            TYPE_LANG_SPECIFIC (t)->s->len = len;
            field_array = TYPE_LANG_SPECIFIC (t)->s->elts;
            qsort (field_array, len, sizeof (tree), field_decl_cmp);
          }
      }
  }

  for (x = TYPE_MAIN_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
    {
      TYPE_FIELDS (x) = TYPE_FIELDS (t);
      TYPE_LANG_SPECIFIC (x) = TYPE_LANG_SPECIFIC (t);
      TYPE_ALIGN (x) = TYPE_ALIGN (t);
      TYPE_USER_ALIGN (x) = TYPE_USER_ALIGN (t);
    }

  /* If this was supposed to be a transparent union, but we can't
     make it one, warn and turn off the flag.  */
  if (TREE_CODE (t) == UNION_TYPE
      && TYPE_TRANSPARENT_UNION (t)
      && (!TYPE_FIELDS (t) || TYPE_MODE (t) != DECL_MODE (TYPE_FIELDS (t))))
    {
      TYPE_TRANSPARENT_UNION (t) = 0;
      warning ("union cannot be made transparent");
    }

  /* If this structure or union completes the type of any previous
     variable declaration, lay it out and output its rtl.  */
  for (x = C_TYPE_INCOMPLETE_VARS (TYPE_MAIN_VARIANT (t));
       x;
       x = TREE_CHAIN (x))
    {
      tree decl = TREE_VALUE (x);
      if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	layout_array_type (TREE_TYPE (decl));
      if (TREE_CODE (decl) != TYPE_DECL)
	{
	  layout_decl (decl, 0);
	  if (c_dialect_objc ())
	    objc_check_decl (decl);
	  rest_of_decl_compilation (decl, NULL, toplevel, 0);
	  if (! toplevel)
	    expand_decl (decl);
	}
    }
  C_TYPE_INCOMPLETE_VARS (TYPE_MAIN_VARIANT (t)) = 0;

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, toplevel);

  return t;
}

/* Lay out the type T, and its element type, and so on.  */

static void
layout_array_type (tree t)
{
  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    layout_array_type (TREE_TYPE (t));
  layout_type (t);
}

/* Begin compiling the definition of an enumeration type.
   NAME is its name (or null if anonymous).
   Returns the type object, as yet incomplete.
   Also records info about it so that build_enumerator
   may be used to declare the individual values as they are read.  */

tree
start_enum (tree name)
{
  tree enumtype = 0;

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

  if (name != 0)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, 1);

  if (enumtype == 0 || TREE_CODE (enumtype) != ENUMERAL_TYPE)
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (name, enumtype);
    }

  C_TYPE_BEING_DEFINED (enumtype) = 1;

  if (TYPE_VALUES (enumtype) != 0)
    {
      /* This enum is a named one that has been declared already.  */
      error ("redeclaration of `enum %s'", IDENTIFIER_POINTER (name));

      /* Completely replace its old definition.
	 The old enumerators remain defined, however.  */
      TYPE_VALUES (enumtype) = 0;
    }

  enum_next_value = integer_zero_node;
  enum_overflow = 0;

  if (flag_short_enums)
    TYPE_PACKED (enumtype) = 1;

  return enumtype;
}

/* After processing and defining all the values of an enumeration type,
   install their decls in the enumeration type and finish it off.
   ENUMTYPE is the type object, VALUES a list of decl-value pairs,
   and ATTRIBUTES are the specified attributes.
   Returns ENUMTYPE.  */

tree
finish_enum (tree enumtype, tree values, tree attributes)
{
  tree pair, tem;
  tree minnode = 0, maxnode = 0, enum_value_type;
  int precision, unsign;
  int toplevel = (global_scope == current_scope);

  if (in_parm_level_p ())
    warning ("enum defined inside parms");

  decl_attributes (&enumtype, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);

  /* Calculate the maximum value of any enumerator in this type.  */

  if (values == error_mark_node)
    minnode = maxnode = integer_zero_node;
  else
    {
      minnode = maxnode = TREE_VALUE (values);
      for (pair = TREE_CHAIN (values); pair; pair = TREE_CHAIN (pair))
	{
	  tree value = TREE_VALUE (pair);
	  if (tree_int_cst_lt (maxnode, value))
	    maxnode = value;
	  if (tree_int_cst_lt (value, minnode))
	    minnode = value;
	}
    }

  /* Construct the final type of this enumeration.  It is the same
     as one of the integral types - the narrowest one that fits, except
     that normally we only go as narrow as int - and signed iff any of
     the values are negative.  */
  unsign = (tree_int_cst_sgn (minnode) >= 0);
  precision = MAX (min_precision (minnode, unsign),
		   min_precision (maxnode, unsign));
  if (TYPE_PACKED (enumtype) || precision > TYPE_PRECISION (integer_type_node))
    {
      tree narrowest = c_common_type_for_size (precision, unsign);
      if (narrowest == 0)
	{
	  warning ("enumeration values exceed range of largest integer");
	  narrowest = long_long_integer_type_node;
	}

      precision = TYPE_PRECISION (narrowest);
    }
  else
    precision = TYPE_PRECISION (integer_type_node);

  if (precision == TYPE_PRECISION (integer_type_node))
    enum_value_type = c_common_type_for_size (precision, 0);
  else
    enum_value_type = enumtype;

  TYPE_MIN_VALUE (enumtype) = minnode;
  TYPE_MAX_VALUE (enumtype) = maxnode;
  TREE_UNSIGNED (enumtype) = unsign;
  TYPE_SIZE (enumtype) = 0;

  /* If the precision of the type was specific with an attribute and it
     was too small, give an error.  Otherwise, use it.  */
  if (TYPE_PRECISION (enumtype))
    {
      if (precision > TYPE_PRECISION (enumtype))
	error ("specified mode too small for enumeral values");
    }
  else
    TYPE_PRECISION (enumtype) = precision;

  layout_type (enumtype);

  if (values != error_mark_node)
    {
      /* Change the type of the enumerators to be the enum type.  We
	 need to do this irrespective of the size of the enum, for
	 proper type checking.  Replace the DECL_INITIALs of the
	 enumerators, and the value slots of the list, with copies
	 that have the enum type; they cannot be modified in place
	 because they may be shared (e.g.  integer_zero_node) Finally,
	 change the purpose slots to point to the names of the decls.  */
      for (pair = values; pair; pair = TREE_CHAIN (pair))
	{
	  tree enu = TREE_PURPOSE (pair);

	  TREE_TYPE (enu) = enumtype;

	  /* The ISO C Standard mandates enumerators to have type int,
	     even though the underlying type of an enum type is
	     unspecified.  Here we convert any enumerators that fit in
	     an int to type int, to avoid promotions to unsigned types
	     when comparing integers with enumerators that fit in the
	     int range.  When -pedantic is given, build_enumerator()
	     would have already taken care of those that don't fit.  */
	  if (int_fits_type_p (DECL_INITIAL (enu), enum_value_type))
	    DECL_INITIAL (enu) = convert (enum_value_type, DECL_INITIAL (enu));
	  else
	    DECL_INITIAL (enu) = convert (enumtype, DECL_INITIAL (enu));

	  TREE_PURPOSE (pair) = DECL_NAME (enu);
	  TREE_VALUE (pair) = DECL_INITIAL (enu);
	}

      TYPE_VALUES (enumtype) = values;
    }

  /* Fix up all variant types of this enum type.  */
  for (tem = TYPE_MAIN_VARIANT (enumtype); tem; tem = TYPE_NEXT_VARIANT (tem))
    {
      if (tem == enumtype)
	continue;
      TYPE_VALUES (tem) = TYPE_VALUES (enumtype);
      TYPE_MIN_VALUE (tem) = TYPE_MIN_VALUE (enumtype);
      TYPE_MAX_VALUE (tem) = TYPE_MAX_VALUE (enumtype);
      TYPE_SIZE (tem) = TYPE_SIZE (enumtype);
      TYPE_SIZE_UNIT (tem) = TYPE_SIZE_UNIT (enumtype);
      TYPE_MODE (tem) = TYPE_MODE (enumtype);
      TYPE_PRECISION (tem) = TYPE_PRECISION (enumtype);
      TYPE_ALIGN (tem) = TYPE_ALIGN (enumtype);
      TYPE_USER_ALIGN (tem) = TYPE_USER_ALIGN (enumtype);
      TREE_UNSIGNED (tem) = TREE_UNSIGNED (enumtype);
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (enumtype, toplevel);

  return enumtype;
}

/* Build and install a CONST_DECL for one value of the
   current enumeration type (one that was begun with start_enum).
   Return a tree-list containing the CONST_DECL and its value.
   Assignment of sequential values by default is handled here.  */

tree
build_enumerator (tree name, tree value)
{
  tree decl, type;

  /* Validate and default VALUE.  */

  /* Remove no-op casts from the value.  */
  if (value)
    STRIP_TYPE_NOPS (value);

  if (value != 0)
    {
      if (TREE_CODE (value) == INTEGER_CST)
	{
	  value = default_conversion (value);
	  constant_expression_warning (value);
	}
      else
	{
	  error ("enumerator value for `%s' not integer constant",
		 IDENTIFIER_POINTER (name));
	  value = 0;
	}
    }

  /* Default based on previous value.  */
  /* It should no longer be possible to have NON_LVALUE_EXPR
     in the default.  */
  if (value == 0)
    {
      value = enum_next_value;
      if (enum_overflow)
	error ("overflow in enumeration values");
    }

  if (pedantic && ! int_fits_type_p (value, integer_type_node))
    {
      pedwarn ("ISO C restricts enumerator values to range of `int'");
      value = convert (integer_type_node, value);
    }

  /* Set basis for default for next value.  */
  enum_next_value = build_binary_op (PLUS_EXPR, value, integer_one_node, 0);
  enum_overflow = tree_int_cst_lt (enum_next_value, value);

  /* Now create a declaration for the enum value name.  */

  type = TREE_TYPE (value);
  type = c_common_type_for_size (MAX (TYPE_PRECISION (type),
				      TYPE_PRECISION (integer_type_node)),
				 (TYPE_PRECISION (type)
				  >= TYPE_PRECISION (integer_type_node)
				  && TREE_UNSIGNED (type)));

  decl = build_decl (CONST_DECL, name, type);
  DECL_INITIAL (decl) = convert (type, value);
  pushdecl (decl);

  return tree_cons (decl, value, NULL_TREE);
}


/* Create the FUNCTION_DECL for a function definition.
   DECLSPECS, DECLARATOR and ATTRIBUTES are the parts of
   the declaration; they describe the function's name and the type it returns,
   but twisted together in a fashion that parallels the syntax of C.

   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns 1 on success.  If the DECLARATOR is not suitable for a function
   (it defines a datum instead), we return 0, which tells
   yyparse to report a parse error.  */

int
start_function (tree declspecs, tree declarator, tree attributes)
{
  tree decl1, old_decl;
  tree restype;
  int old_immediate_size_expand = immediate_size_expand;

  current_function_returns_value = 0;  /* Assume, until we see it does.  */
  current_function_returns_null = 0;
  current_function_returns_abnormally = 0;
  warn_about_return_type = 0;
  current_extern_inline = 0;
  c_in_iteration_stmt = 0;
  c_in_case_stmt = 0;

  /* Don't expand any sizes in the return type of the function.  */
  immediate_size_expand = 0;

  decl1 = grokdeclarator (declarator, declspecs, FUNCDEF, 1, NULL);

  /* If the declarator is not suitable for a function definition,
     cause a syntax error.  */
  if (decl1 == 0)
    {
      immediate_size_expand = old_immediate_size_expand;
      return 0;
    }

  decl_attributes (&decl1, attributes, 0);

  if (DECL_DECLARED_INLINE_P (decl1)
      && DECL_UNINLINABLE (decl1)
      && lookup_attribute ("noinline", DECL_ATTRIBUTES (decl1)))
    warning ("%Jinline function '%D' given attribute noinline", decl1, decl1);

  announce_function (decl1);

  if (!COMPLETE_OR_VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl1))))
    {
      error ("return type is an incomplete type");
      /* Make it return void instead.  */
      TREE_TYPE (decl1)
	= build_function_type (void_type_node,
			       TYPE_ARG_TYPES (TREE_TYPE (decl1)));
    }

  if (warn_about_return_type)
    pedwarn_c99 ("return type defaults to `int'");

  /* Save the parm names or decls from this function's declarator
     where store_parm_decls will find them.  */
  current_function_parms = last_function_parms;
  current_function_parm_tags = last_function_parm_tags;
  current_function_parm_others = last_function_parm_others;

  /* Make the init_value nonzero so pushdecl knows this is not tentative.
     error_mark_node is replaced below (in poplevel) with the BLOCK.  */
  DECL_INITIAL (decl1) = error_mark_node;

  /* If this definition isn't a prototype and we had a prototype declaration
     before, copy the arg type info from that prototype.
     But not if what we had before was a builtin function.  */
  old_decl = lookup_name_current_level (DECL_NAME (decl1));
  if (old_decl != 0 && TREE_CODE (TREE_TYPE (old_decl)) == FUNCTION_TYPE
      && !DECL_BUILT_IN (old_decl)
      && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (decl1)))
	  == TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (old_decl))))
      && TYPE_ARG_TYPES (TREE_TYPE (decl1)) == 0)
    {
      TREE_TYPE (decl1) = TREE_TYPE (old_decl);
      current_function_prototype_locus = DECL_SOURCE_LOCATION (old_decl);
    }

  /* Optionally warn of old-fashioned def with no previous prototype.  */
  if (warn_strict_prototypes
      && TYPE_ARG_TYPES (TREE_TYPE (decl1)) == 0
      && C_DECL_ISNT_PROTOTYPE (old_decl))
    warning ("function declaration isn't a prototype");
  /* Optionally warn of any global def with no previous prototype.  */
  else if (warn_missing_prototypes
	   && TREE_PUBLIC (decl1)
	   && ! MAIN_NAME_P (DECL_NAME (decl1))
	   && C_DECL_ISNT_PROTOTYPE (old_decl))
    warning ("%Jno previous prototype for '%D'", decl1, decl1);
  /* Optionally warn of any def with no previous prototype
     if the function has already been used.  */
  else if (warn_missing_prototypes
	   && old_decl != 0 && TREE_USED (old_decl)
	   && TYPE_ARG_TYPES (TREE_TYPE (old_decl)) == 0)
    warning ("%J'%D' was used with no prototype before its definition",
	     decl1, decl1);
  /* Optionally warn of any global def with no previous declaration.  */
  else if (warn_missing_declarations
	   && TREE_PUBLIC (decl1)
	   && old_decl == 0
	   && ! MAIN_NAME_P (DECL_NAME (decl1)))
    warning ("%Jno previous declaration for '%D'", decl1, decl1);
  /* Optionally warn of any def with no previous declaration
     if the function has already been used.  */
  else if (warn_missing_declarations
	   && old_decl != 0 && TREE_USED (old_decl)
	   && C_DECL_IMPLICIT (old_decl))
    warning ("%J`%D' was used with no declaration before its definition",
	     decl1, decl1);

  /* This is a definition, not a reference.
     So normally clear DECL_EXTERNAL.
     However, `extern inline' acts like a declaration
     except for defining how to inline.  So set DECL_EXTERNAL in that case.  */
  DECL_EXTERNAL (decl1) = current_extern_inline;

  /* This function exists in static storage.
     (This does not mean `static' in the C sense!)  */
  TREE_STATIC (decl1) = 1;

  /* A nested function is not global.  */
  if (current_function_decl != 0)
    TREE_PUBLIC (decl1) = 0;

#ifdef ENABLE_CHECKING
  /* This is the earliest point at which we might know the assembler
     name of the function.  Thus, if it's set before this, die horribly.  */
  if (DECL_ASSEMBLER_NAME_SET_P (decl1))
    abort ();
#endif

  /* If #pragma weak was used, mark the decl weak now.  */
  if (current_scope == global_scope)
    maybe_apply_pragma_weak (decl1);

  /* Warn for unlikely, improbable, or stupid declarations of `main'.  */
  if (warn_main > 0 && MAIN_NAME_P (DECL_NAME (decl1)))
    {
      tree args;
      int argct = 0;

      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (decl1)))
	  != integer_type_node)
	pedwarn ("%Jreturn type of '%D' is not `int'", decl1, decl1);

      for (args = TYPE_ARG_TYPES (TREE_TYPE (decl1)); args;
	   args = TREE_CHAIN (args))
	{
	  tree type = args ? TREE_VALUE (args) : 0;

	  if (type == void_type_node)
	    break;

	  ++argct;
	  switch (argct)
	    {
	    case 1:
	      if (TYPE_MAIN_VARIANT (type) != integer_type_node)
		pedwarn ("%Jfirst argument of '%D' should be `int'",
			 decl1, decl1);
	      break;

	    case 2:
	      if (TREE_CODE (type) != POINTER_TYPE
		  || TREE_CODE (TREE_TYPE (type)) != POINTER_TYPE
		  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type)))
		      != char_type_node))
		pedwarn ("%Jsecond argument of '%D' should be 'char **'",
                         decl1, decl1);
	      break;

	    case 3:
	      if (TREE_CODE (type) != POINTER_TYPE
		  || TREE_CODE (TREE_TYPE (type)) != POINTER_TYPE
		  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type)))
		      != char_type_node))
		pedwarn ("%Jthird argument of '%D' should probably be "
                         "'char **'", decl1, decl1);
	      break;
	    }
	}

      /* It is intentional that this message does not mention the third
	 argument because it's only mentioned in an appendix of the
	 standard.  */
      if (argct > 0 && (argct < 2 || argct > 3))
	pedwarn ("%J'%D' takes only zero or two arguments", decl1, decl1);

      if (! TREE_PUBLIC (decl1))
	pedwarn ("%J'%D' is normally a non-static function", decl1, decl1);
    }

  /* Record the decl so that the function name is defined.
     If we already have a decl for this name, and it is a FUNCTION_DECL,
     use the old decl.  */

  current_function_decl = pushdecl (decl1);

  pushlevel (0);
  declare_parm_level ();

  make_decl_rtl (current_function_decl, NULL);

  restype = TREE_TYPE (TREE_TYPE (current_function_decl));
  /* Promote the value to int before returning it.  */
  if (c_promoting_integer_type_p (restype))
    {
      /* It retains unsignedness if not really getting wider.  */
      if (TREE_UNSIGNED (restype)
	  && (TYPE_PRECISION (restype)
		  == TYPE_PRECISION (integer_type_node)))
	restype = unsigned_type_node;
      else
	restype = integer_type_node;
    }
  DECL_RESULT (current_function_decl)
    = build_decl (RESULT_DECL, NULL_TREE, restype);

  /* If this fcn was already referenced via a block-scope `extern' decl
     (or an implicit decl), propagate certain information about the usage.  */
  if (TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (current_function_decl)))
    TREE_ADDRESSABLE (current_function_decl) = 1;

  immediate_size_expand = old_immediate_size_expand;

  start_fname_decls ();

  return 1;
}

/* Subroutine of store_parm_decls which handles new-style function
   definitions (prototype format). The parms already have decls, so we
   need only record them as in effect and complain if any redundant
   old-style parm decls were written.  */
static void
store_parm_decls_newstyle (void)
{
  tree decl, last;
  tree fndecl = current_function_decl;
  tree parms = current_function_parms;
  tree tags = current_function_parm_tags;
  tree others = current_function_parm_others;

  if (current_scope->parms || current_scope->names || current_scope->tags)
    {
      error ("%Jold-style parameter declarations in prototyped "
	     "function definition", fndecl);

      /* Get rid of the old-style declarations.  */
      poplevel (0, 0, 0);
      pushlevel (0);
    }

  /* Now make all the parameter declarations visible in the function body.
     We can bypass most of the grunt work of pushdecl.  */
  for (last = 0, decl = parms; decl; last = decl, decl = TREE_CHAIN (decl))
    {
      DECL_CONTEXT (decl) = current_function_decl;
      if (DECL_NAME (decl) == 0)
	error ("%Jparameter name omitted", decl);
      else
	{
	  if (IDENTIFIER_SYMBOL_VALUE (DECL_NAME (decl)))
	    current_scope->shadowed
	      = tree_cons (DECL_NAME (decl),
			   IDENTIFIER_SYMBOL_VALUE (DECL_NAME (decl)),
			   current_scope->shadowed);
	  IDENTIFIER_SYMBOL_VALUE (DECL_NAME (decl)) = decl;
	}
    }
  current_scope->parms = parms;
  current_scope->parms_last = last;

  /* Record the parameter list in the function declaration.  */
  DECL_ARGUMENTS (fndecl) = parms;

  /* Now make all the ancillary declarations visible, likewise.  */
  for (last = 0, decl = others; decl; last = decl, decl = TREE_CHAIN (decl))
    {
      DECL_CONTEXT (decl) = current_function_decl;
      if (DECL_NAME (decl)
	  && TYPE_MAIN_VARIANT (TREE_TYPE (decl)) != void_type_node)
	{
	  if (IDENTIFIER_SYMBOL_VALUE (DECL_NAME (decl)))
	    current_scope->shadowed
	      = tree_cons (DECL_NAME (decl),
			   IDENTIFIER_SYMBOL_VALUE (DECL_NAME (decl)),
			   current_scope->shadowed);
	  IDENTIFIER_SYMBOL_VALUE (DECL_NAME (decl)) = decl;
	}
    }
  current_scope->names = others;
  current_scope->names_last = last;

  /* And all the tag declarations.  */
  for (decl = tags; decl; decl = TREE_CHAIN (decl))
    if (TREE_PURPOSE (decl))
      {
	if (IDENTIFIER_TAG_VALUE (TREE_PURPOSE (decl)))
	  current_scope->shadowed_tags
	    = tree_cons (TREE_PURPOSE (decl),
			 IDENTIFIER_SYMBOL_VALUE (TREE_PURPOSE (decl)),
			 current_scope->shadowed_tags);
	IDENTIFIER_TAG_VALUE (TREE_PURPOSE (decl)) = TREE_VALUE (decl);
      }
  current_scope->tags = tags;
}

/* Subroutine of store_parm_decls which handles old-style function
   definitions (separate parameter list and declarations).  */

static void
store_parm_decls_oldstyle (void)
{
  tree parm, decl, last;
  tree fndecl = current_function_decl;

  /* This is the identifier list from the function declarator.  */
  tree parmids = current_function_parms;

  /* We use DECL_WEAK as a flag to show which parameters have been
     seen already, since it is not used on PARM_DECL.  */
#ifdef ENABLE_CHECKING
  for (parm = current_scope->parms; parm; parm = TREE_CHAIN (parm))
    if (DECL_WEAK (parm))
      abort ();
#endif

  /* Match each formal parameter name with its declaration.  Save each
     decl in the appropriate TREE_PURPOSE slot of the parmids chain.  */
  for (parm = parmids; parm; parm = TREE_CHAIN (parm))
    {
      if (TREE_VALUE (parm) == 0)
	{
	  error ("%Jparameter name missing from parameter list", fndecl);
	  TREE_PURPOSE (parm) = 0;
	  continue;
	}

      decl = IDENTIFIER_SYMBOL_VALUE (TREE_VALUE (parm));
      if (decl && DECL_CONTEXT (decl) == fndecl)
	{
	  /* If we got something other than a PARM_DECL it is an error.  */
	  if (TREE_CODE (decl) != PARM_DECL)
	    error ("%J\"%D\" declared as a non-parameter", decl, decl);
	  /* If the declaration is already marked, we have a duplicate
	     name.  Complain and ignore the duplicate.  */
	  else if (DECL_WEAK (decl))
	    {
	      error ("%Jmultiple parameters named \"%D\"", decl, decl);
	      TREE_PURPOSE (parm) = 0;
	      continue;
	    }
	  /* If the declaration says "void", complain and turn it into
	     an int.  */
	  else if (VOID_TYPE_P (TREE_TYPE (decl)))
	    {
	      error ("%Jparameter \"%D\" declared void", decl, decl);
	      TREE_TYPE (decl) = integer_type_node;
	      DECL_ARG_TYPE (decl) = integer_type_node;
	      layout_decl (decl, 0);
	    }
	}
      /* If no declaration found, default to int.  */
      else
	{
	  decl = build_decl (PARM_DECL, TREE_VALUE (parm), integer_type_node);
	  DECL_ARG_TYPE (decl) = TREE_TYPE (decl);
	  DECL_SOURCE_LOCATION (decl) = DECL_SOURCE_LOCATION (fndecl);
	  pushdecl (decl);

	  if (flag_isoc99)
	    pedwarn ("%Jtype of \"%D\" defaults to \"int\"", decl, decl);
	  else if (extra_warnings)
	    warning ("%Jtype of \"%D\" defaults to \"int\"", decl, decl);
	}

      TREE_PURPOSE (parm) = decl;
      DECL_WEAK (decl) = 1;
    }

  /* Now examine the parms chain for incomplete declarations
     and declarations with no corresponding names.  */

  for (parm = current_scope->parms; parm; parm = TREE_CHAIN (parm))
    {
      if (!COMPLETE_TYPE_P (TREE_TYPE (parm)))
	{
	  error ("%Jparameter \"%D\" has incomplete type", parm, parm);
	  TREE_TYPE (parm) = error_mark_node;
	}

      if (! DECL_WEAK (parm))
	{
	  error ("%Jdeclaration for parameter \"%D\" but no such parameter",
		 parm, parm);

	  /* Pretend the parameter was not missing.
	     This gets us to a standard state and minimizes
	     further error messages.  */
	  parmids = chainon (parmids, tree_cons (parm, 0, 0));
	}
    }

  /* Chain the declarations together in the order of the list of
     names.  Store that chain in the function decl, replacing the
     list of names.  Update the current scope to match.  */
  DECL_ARGUMENTS (fndecl) = 0;

  for (parm = parmids; parm; parm = TREE_CHAIN (parm))
    if (TREE_PURPOSE (parm))
      break;
  if (parm && TREE_PURPOSE (parm))
    {
      last = TREE_PURPOSE (parm);
      DECL_ARGUMENTS (fndecl) = last;
      current_scope->parms = last;
      DECL_WEAK (last) = 0;

      for (parm = TREE_CHAIN (parm); parm; parm = TREE_CHAIN (parm))
	if (TREE_PURPOSE (parm))
	  {
	    TREE_CHAIN (last) = TREE_PURPOSE (parm);
	    last = TREE_PURPOSE (parm);
	    DECL_WEAK (last) = 0;
	  }
      current_scope->parms_last = last;
      TREE_CHAIN (last) = 0;
    }

  /* If there was a previous prototype,
     set the DECL_ARG_TYPE of each argument according to
     the type previously specified, and report any mismatches.  */

  if (TYPE_ARG_TYPES (TREE_TYPE (fndecl)))
    {
      tree type;
      for (parm = DECL_ARGUMENTS (fndecl),
	     type = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
	   parm || (type && (TYPE_MAIN_VARIANT (TREE_VALUE (type))
			     != void_type_node));
	   parm = TREE_CHAIN (parm), type = TREE_CHAIN (type))
	{
	  if (parm == 0 || type == 0
	      || TYPE_MAIN_VARIANT (TREE_VALUE (type)) == void_type_node)
	    {
	      error ("number of arguments doesn't match prototype");
	      error ("%Hprototype declaration",
		     &current_function_prototype_locus);
	      break;
	    }
	  /* Type for passing arg must be consistent with that
	     declared for the arg.  ISO C says we take the unqualified
	     type for parameters declared with qualified type.  */
	  if (! comptypes (TYPE_MAIN_VARIANT (DECL_ARG_TYPE (parm)),
			   TYPE_MAIN_VARIANT (TREE_VALUE (type)),
			   COMPARE_STRICT))
	    {
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (parm))
		  == TYPE_MAIN_VARIANT (TREE_VALUE (type)))
		{
		  /* Adjust argument to match prototype.  E.g. a previous
		     `int foo(float);' prototype causes
		     `int foo(x) float x; {...}' to be treated like
		     `int foo(float x) {...}'.  This is particularly
		     useful for argument types like uid_t.  */
		  DECL_ARG_TYPE (parm) = TREE_TYPE (parm);

		  if (targetm.calls.promote_prototypes (TREE_TYPE (current_function_decl))
		      && INTEGRAL_TYPE_P (TREE_TYPE (parm))
		      && TYPE_PRECISION (TREE_TYPE (parm))
		      < TYPE_PRECISION (integer_type_node))
		    DECL_ARG_TYPE (parm) = integer_type_node;

		  if (pedantic)
		    {
		      pedwarn ("promoted argument \"%D\" "
			       "doesn't match prototype", parm);
		      pedwarn ("%Hprototype declaration",
			       &current_function_prototype_locus);
		    }
		}
	      else
		{
		  error ("argument \"%D\" doesn't match prototype", parm);
		  error ("%Hprototype declaration",
			 &current_function_prototype_locus);
		}
	    }
	}
      TYPE_ACTUAL_ARG_TYPES (TREE_TYPE (fndecl)) = 0;
    }

  /* Otherwise, create a prototype that would match.  */

  else
    {
      tree actual = 0, last = 0, type;

      for (parm = DECL_ARGUMENTS (fndecl); parm; parm = TREE_CHAIN (parm))
	{
	  type = tree_cons (NULL_TREE, DECL_ARG_TYPE (parm), NULL_TREE);
	  if (last)
	    TREE_CHAIN (last) = type;
	  else
	    actual = type;
	  last = type;
	}
      type = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
      if (last)
	TREE_CHAIN (last) = type;
      else
	actual = type;

      /* We are going to assign a new value for the TYPE_ACTUAL_ARG_TYPES
	 of the type of this function, but we need to avoid having this
	 affect the types of other similarly-typed functions, so we must
	 first force the generation of an identical (but separate) type
	 node for the relevant function type.  The new node we create
	 will be a variant of the main variant of the original function
	 type.  */

      TREE_TYPE (fndecl) = build_type_copy (TREE_TYPE (fndecl));

      TYPE_ACTUAL_ARG_TYPES (TREE_TYPE (fndecl)) = actual;
    }
}

/* Store the parameter declarations into the current function declaration.
   This is called after parsing the parameter declarations, before
   digesting the body of the function.

   For an old-style definition, construct a prototype out of the old-style
   parameter declarations and inject it into the function's type.  */

void
store_parm_decls (void)
{
  tree fndecl = current_function_decl;

  /* The function containing FNDECL, if any.  */
  tree context = decl_function_context (fndecl);

  /* True if this definition is written with a prototype.  */
  bool prototype = (current_function_parms
		    && TREE_CODE (current_function_parms) != TREE_LIST);

  if (prototype)
    store_parm_decls_newstyle ();
  else
    store_parm_decls_oldstyle ();

  /* The next call to pushlevel will be a function body.  */

  next_is_function_body = true;

  /* Write a record describing this function definition to the prototypes
     file (if requested).  */

  gen_aux_info_record (fndecl, 1, 0, prototype);

  /* Initialize the RTL code for the function.  */
  allocate_struct_function (fndecl);

  /* Begin the statement tree for this function.  */
  begin_stmt_tree (&DECL_SAVED_TREE (fndecl));

  /* If this is a nested function, save away the sizes of any
     variable-size types so that we can expand them when generating
     RTL.  */
  if (context)
    {
      tree t;

      DECL_LANG_SPECIFIC (fndecl)->pending_sizes
	= nreverse (get_pending_sizes ());
      for (t = DECL_LANG_SPECIFIC (fndecl)->pending_sizes;
	   t;
	   t = TREE_CHAIN (t))
	SAVE_EXPR_CONTEXT (TREE_VALUE (t)) = context;
    }

  /* This function is being processed in whole-function mode.  */
  cfun->x_whole_function_mode_p = 1;

  /* Even though we're inside a function body, we still don't want to
     call expand_expr to calculate the size of a variable-sized array.
     We haven't necessarily assigned RTL to all variables yet, so it's
     not safe to try to expand expressions involving them.  */
  immediate_size_expand = 0;
  cfun->x_dont_save_pending_sizes_p = 1;
}

/* Finish up a function declaration and compile that function
   all the way to assembler language output.  The free the storage
   for the function definition.

   This is called after parsing the body of the function definition.  */

void
finish_function (void)
{
  tree fndecl = current_function_decl;

  /* When a function declaration is totally empty, e.g.
        void foo(void) { }
     (the argument list is irrelevant) the compstmt rule will not
     bother calling pushlevel/poplevel, which means we get here with
     the scope stack out of sync.  Detect this situation by noticing
     that current_scope is still as store_parm_decls left it, and do
     a dummy push/pop to get back to consistency.
     Note that the call to pushlevel does not actually push another
     scope - see there for details.  */

  if (current_scope->parm_flag && next_is_function_body)
    {
      pushlevel (0);
      poplevel (0, 0, 0);
    }

  if (TREE_CODE (fndecl) == FUNCTION_DECL
      && targetm.calls.promote_prototypes (TREE_TYPE (fndecl)))
    {
      tree args = DECL_ARGUMENTS (fndecl);
      for (; args; args = TREE_CHAIN (args))
 	{
 	  tree type = TREE_TYPE (args);
 	  if (INTEGRAL_TYPE_P (type)
 	      && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
 	    DECL_ARG_TYPE (args) = integer_type_node;
 	}
    }

  if (DECL_INITIAL (fndecl) && DECL_INITIAL (fndecl) != error_mark_node)
    BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Must mark the RESULT_DECL as being in this function.  */

  if (DECL_RESULT (fndecl) && DECL_RESULT (fndecl) != error_mark_node)
    DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  if (MAIN_NAME_P (DECL_NAME (fndecl)) && flag_hosted)
    {
      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (fndecl)))
	  != integer_type_node)
	{
	  /* If warn_main is 1 (-Wmain) or 2 (-Wall), we have already warned.
	     If warn_main is -1 (-Wno-main) we don't want to be warned.  */
	  if (!warn_main)
	    pedwarn ("%Jreturn type of '%D' is not `int'", fndecl, fndecl);
	}
      else
	{
#ifdef DEFAULT_MAIN_RETURN
	  /* Make it so that `main' always returns success by default.  */
	  DEFAULT_MAIN_RETURN;
#else
	  if (flag_isoc99)
	    c_expand_return (integer_zero_node);
#endif
	}
    }

  finish_fname_decls ();

  /* Tie off the statement tree for this function.  */
  finish_stmt_tree (&DECL_SAVED_TREE (fndecl));

  /* Complain if there's just no return statement.  */
  if (warn_return_type
      && TREE_CODE (TREE_TYPE (TREE_TYPE (fndecl))) != VOID_TYPE
      && !current_function_returns_value && !current_function_returns_null
      /* Don't complain if we abort.  */
      && !current_function_returns_abnormally
      /* Don't warn for main().  */
      && !MAIN_NAME_P (DECL_NAME (fndecl))
      /* Or if they didn't actually specify a return type.  */
      && !C_FUNCTION_IMPLICIT_INT (fndecl)
      /* Normally, with -Wreturn-type, flow will complain.  Unless we're an
	 inline function, as we might never be compiled separately.  */
      && DECL_INLINE (fndecl))
    warning ("no return statement in function returning non-void");

  /* With just -Wextra, complain only if function returns both with
     and without a value.  */
  if (extra_warnings
      && current_function_returns_value
      && current_function_returns_null)
    warning ("this function may return with or without a value");

  /* We're leaving the context of this function, so zap cfun.  It's still in
     DECL_SAVED_INSNS, and we'll restore it in tree_rest_of_compilation.  */
  cfun = NULL;

  /* ??? Objc emits functions after finalizing the compilation unit.
     This should be cleaned up later and this conditional removed.  */
  if (!cgraph_global_info_ready)
    cgraph_finalize_function (fndecl, false);
  else
    c_expand_body (fndecl);
  current_function_decl = NULL;
}

/* Generate the RTL for a deferred function FNDECL.  */

void
c_expand_deferred_function (tree fndecl)
{
  /* DECL_INLINE or DECL_RESULT might got cleared after the inline
     function was deferred, e.g. in duplicate_decls.  */
  if (DECL_INLINE (fndecl) && DECL_RESULT (fndecl))
    {
      if (flag_inline_trees)
	{
	  timevar_push (TV_INTEGRATION);
	  optimize_inline_calls (fndecl);
	  timevar_pop (TV_INTEGRATION);
	}
      c_expand_body (fndecl);
      current_function_decl = NULL;
    }
}

/* Generate the RTL for the body of FNDECL.  If NESTED_P is nonzero,
   then we are already in the process of generating RTL for another
   function.  */

static void
c_expand_body_1 (tree fndecl, int nested_p)
{
  if (nested_p)
    {
      /* Make sure that we will evaluate variable-sized types involved
	 in our function's type.  */
      expand_pending_sizes (DECL_LANG_SPECIFIC (fndecl)->pending_sizes);

      /* Squirrel away our current state.  */
      push_function_context ();
    }
    
  tree_rest_of_compilation (fndecl, nested_p);

  if (nested_p)
    /* Return to the enclosing function.  */
    pop_function_context ();

  if (DECL_STATIC_CONSTRUCTOR (fndecl))
    {
      if (targetm.have_ctors_dtors)
	(* targetm.asm_out.constructor) (XEXP (DECL_RTL (fndecl), 0),
				         DEFAULT_INIT_PRIORITY);
      else
	static_ctors = tree_cons (NULL_TREE, fndecl, static_ctors);
    }

  if (DECL_STATIC_DESTRUCTOR (fndecl))
    {
      if (targetm.have_ctors_dtors)
	(* targetm.asm_out.destructor) (XEXP (DECL_RTL (fndecl), 0),
				        DEFAULT_INIT_PRIORITY);
      else
	static_dtors = tree_cons (NULL_TREE, fndecl, static_dtors);
    }
}

/* Like c_expand_body_1 but only for unnested functions.  */

void
c_expand_body (tree fndecl)
{

  if (DECL_INITIAL (fndecl) && DECL_INITIAL (fndecl) != error_mark_node)
    c_expand_body_1 (fndecl, 0);
}

/* Check the declarations given in a for-loop for satisfying the C99
   constraints.  */
void
check_for_loop_decls (void)
{
  tree t;

  if (!flag_isoc99)
    {
      /* If we get here, declarations have been used in a for loop without
	 the C99 for loop scope.  This doesn't make much sense, so don't
	 allow it.  */
      error ("'for' loop initial declaration used outside C99 mode");
      return;
    }
  /* C99 subclause 6.8.5 paragraph 3:

       [#3]  The  declaration  part  of  a for statement shall only
       declare identifiers for objects having storage class auto or
       register.

     It isn't clear whether, in this sentence, "identifiers" binds to
     "shall only declare" or to "objects" - that is, whether all identifiers
     declared must be identifiers for objects, or whether the restriction
     only applies to those that are.  (A question on this in comp.std.c
     in November 2000 received no answer.)  We implement the strictest
     interpretation, to avoid creating an extension which later causes
     problems.  */

  for (t = current_scope->tags; t; t = TREE_CHAIN (t))
    {
      if (TREE_PURPOSE (t) != 0)
        {
          enum tree_code code = TREE_CODE (TREE_VALUE (t));

          if (code == RECORD_TYPE)
            error ("'struct %s' declared in 'for' loop initial declaration",
                   IDENTIFIER_POINTER (TREE_PURPOSE (t)));
          else if (code == UNION_TYPE)
            error ("'union %s' declared in 'for' loop initial declaration",
                   IDENTIFIER_POINTER (TREE_PURPOSE (t)));
          else
            error ("'enum %s' declared in 'for' loop initial declaration",
                   IDENTIFIER_POINTER (TREE_PURPOSE (t)));
        }
    }

  for (t = getdecls (); t; t = TREE_CHAIN (t))
    {
      if (TREE_CODE (t) != VAR_DECL && DECL_NAME (t))
	error ("%Jdeclaration of non-variable '%D' in 'for' loop "
               "initial declaration", t, t);
      else if (TREE_STATIC (t))
	error ("%Jdeclaration of static variable '%D' in 'for' loop "
	       "initial declaration", t, t);
      else if (DECL_EXTERNAL (t))
	error ("%Jdeclaration of 'extern' variable '%D' in 'for' loop "
               "initial declaration", t, t);
    }
}

/* Save and reinitialize the variables
   used during compilation of a C function.  */

void
c_push_function_context (struct function *f)
{
  struct language_function *p;
  p = ggc_alloc (sizeof (struct language_function));
  f->language = p;

  p->base.x_stmt_tree = c_stmt_tree;
  p->base.x_scope_stmt_stack = c_scope_stmt_stack;
  p->x_in_iteration_stmt = c_in_iteration_stmt;
  p->x_in_case_stmt = c_in_case_stmt;
  p->returns_value = current_function_returns_value;
  p->returns_null = current_function_returns_null;
  p->returns_abnormally = current_function_returns_abnormally;
  p->warn_about_return_type = warn_about_return_type;
  p->extern_inline = current_extern_inline;
}

/* Restore the variables used during compilation of a C function.  */

void
c_pop_function_context (struct function *f)
{
  struct language_function *p = f->language;

  if (DECL_SAVED_INSNS (current_function_decl) == 0
      && DECL_SAVED_TREE (current_function_decl) == NULL_TREE)
    {
      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.  */
      DECL_INITIAL (current_function_decl) = error_mark_node;
      DECL_ARGUMENTS (current_function_decl) = 0;
    }

  c_stmt_tree = p->base.x_stmt_tree;
  c_scope_stmt_stack = p->base.x_scope_stmt_stack;
  c_in_iteration_stmt = p->x_in_iteration_stmt;
  c_in_case_stmt = p->x_in_case_stmt;
  current_function_returns_value = p->returns_value;
  current_function_returns_null = p->returns_null;
  current_function_returns_abnormally = p->returns_abnormally;
  warn_about_return_type = p->warn_about_return_type;
  current_extern_inline = p->extern_inline;

  f->language = NULL;
}

/* Copy the DECL_LANG_SPECIFIC data associated with DECL.  */

void
c_dup_lang_specific_decl (tree decl)
{
  struct lang_decl *ld;

  if (!DECL_LANG_SPECIFIC (decl))
    return;

  ld = ggc_alloc (sizeof (struct lang_decl));
  memcpy (ld, DECL_LANG_SPECIFIC (decl), sizeof (struct lang_decl));
  DECL_LANG_SPECIFIC (decl) = ld;
}

/* The functions below are required for functionality of doing
   function at once processing in the C front end. Currently these
   functions are not called from anywhere in the C front end, but as
   these changes continue, that will change.  */

/* Returns nonzero if the current statement is a full expression,
   i.e. temporaries created during that statement should be destroyed
   at the end of the statement.  */

int
stmts_are_full_exprs_p (void)
{
  return 0;
}

/* Returns the stmt_tree (if any) to which statements are currently
   being added.  If there is no active statement-tree, NULL is
   returned.  */

stmt_tree
current_stmt_tree (void)
{
  return &c_stmt_tree;
}

/* Returns the stack of SCOPE_STMTs for the current function.  */

tree *
current_scope_stmt_stack (void)
{
  return &c_scope_stmt_stack;
}

/* Nonzero if TYPE is an anonymous union or struct type.  Always 0 in
   C.  */

int
anon_aggr_type_p (tree node ATTRIBUTE_UNUSED)
{
  return 0;
}

/* Dummy function in place of callback used by C++.  */

void
extract_interface_info (void)
{
}

/* Return a new COMPOUND_STMT, after adding it to the current
   statement tree.  */

tree
c_begin_compound_stmt (void)
{
  tree stmt;

  /* Create the COMPOUND_STMT.  */
  stmt = add_stmt (build_stmt (COMPOUND_STMT, NULL_TREE));

  return stmt;
}

/* Expand T (a DECL_STMT) if it declares an entity not handled by the
   common code.  */

void
c_expand_decl_stmt (tree t)
{
  tree decl = DECL_STMT_DECL (t);

  /* Expand nested functions.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_CONTEXT (decl) == current_function_decl
      && DECL_SAVED_TREE (decl))
    c_expand_body_1 (decl, 1);
}

/* Return the global value of T as a symbol.  */

tree
identifier_global_value	(tree t)
{
  tree decl = IDENTIFIER_SYMBOL_VALUE (t);
  if (decl == 0 || DECL_FILE_SCOPE_P (decl))
    return decl;

  /* Shadowed by something else; find the true global value.  */
  for (decl = global_scope->names; decl; decl = TREE_CHAIN (decl))
    if (DECL_NAME (decl) == t)
      return decl;

  /* Only local values for this decl.  */
  return 0;
}

/* Record a builtin type for C.  If NAME is non-NULL, it is the name used;
   otherwise the name is found in ridpointers from RID_INDEX.  */

void
record_builtin_type (enum rid rid_index, const char *name, tree type)
{
  tree id;
  if (name == 0)
    id = ridpointers[(int) rid_index];
  else
    id = get_identifier (name);
  pushdecl (build_decl (TYPE_DECL, id, type));
}

/* Build the void_list_node (void_type_node having been created).  */
tree
build_void_list_node (void)
{
  tree t = build_tree_list (NULL_TREE, void_type_node);
  return t;
}

/* Return something to represent absolute declarators containing a *.
   TARGET is the absolute declarator that the * contains.
   TYPE_QUALS_ATTRS is a list of modifiers such as const or volatile
   to apply to the pointer type, represented as identifiers, possible mixed
   with attributes.

   We return an INDIRECT_REF whose "contents" are TARGET (inside a TREE_LIST,
   if attributes are present) and whose type is the modifier list.  */

tree
make_pointer_declarator (tree type_quals_attrs, tree target)
{
  tree quals, attrs;
  tree itarget = target;
  split_specs_attrs (type_quals_attrs, &quals, &attrs);
  if (attrs != NULL_TREE)
    itarget = tree_cons (attrs, target, NULL_TREE);
  return build1 (INDIRECT_REF, quals, itarget);
}

/* A wrapper around lhd_set_decl_assembler_name that gives static
   variables their C names if they are at file scope and only one
   translation unit is being compiled, for backwards compatibility
   with certain bizarre assembler hacks (like crtstuff.c).  */

void
c_static_assembler_name (tree decl)
{
  if (num_in_fnames == 1
      && !TREE_PUBLIC (decl) && DECL_CONTEXT (decl)
      && TREE_CODE (DECL_CONTEXT (decl)) == TRANSLATION_UNIT_DECL)
    SET_DECL_ASSEMBLER_NAME (decl, DECL_NAME (decl));
  else
    lhd_set_decl_assembler_name (decl);
}

/* Hash and equality functions for link_hash_table: key off
   DECL_ASSEMBLER_NAME.  */

static hashval_t
link_hash_hash (const void *x_p)
{
  tree x = (tree)x_p;
  return (hashval_t) (long)DECL_ASSEMBLER_NAME (x);
}

static int
link_hash_eq (const void *x1_p, const void *x2_p)
{
  tree x1 = (tree)x1_p;
  tree x2 = (tree)x2_p;
  return DECL_ASSEMBLER_NAME (x1) == DECL_ASSEMBLER_NAME (x2);
}

/* Propagate information between definitions and uses between multiple
   translation units in TU_LIST based on linkage rules.  */

void
merge_translation_unit_decls (void)
{
  const tree tu_list = current_file_decl;
  tree tu;
  tree decl;
  htab_t link_hash_table;
  tree block;

  /* Create the BLOCK that poplevel would have created, but don't
     actually call poplevel since that's expensive.  */
  block = make_node (BLOCK);
  BLOCK_VARS (block) = current_scope->names;
  TREE_USED (block) = 1;
  DECL_INITIAL (current_file_decl) = block;

  /* If only one translation unit seen, no copying necessary.  */
  if (TREE_CHAIN (tu_list) == NULL_TREE)
    return;

  link_hash_table = htab_create (1021, link_hash_hash, link_hash_eq, NULL);

  /* Enter any actual definitions into the hash table.  */
  for (tu = tu_list; tu; tu = TREE_CHAIN (tu))
    for (decl = BLOCK_VARS (DECL_INITIAL (tu)); decl; decl = TREE_CHAIN (decl))
      if (TREE_PUBLIC (decl) && ! DECL_EXTERNAL (decl))
	{
	  PTR *slot;
	  slot = htab_find_slot (link_hash_table, decl, INSERT);

	  /* If we've already got a definition, work out which one is
	     the real one, put it into the hash table, and make the
	     other one DECL_EXTERNAL.  This is important to avoid
	     putting out two definitions of the same symbol in the
	     assembly output.  */
	  if (*slot != NULL)
	    {
	      tree old_decl = (tree) *slot;

	      /* If this is weak or common or whatever, suppress it
		 in favor of the other definition.  */
	      if (DECL_WEAK (decl))
		DECL_EXTERNAL (decl) = 1;
	      else if (DECL_WEAK (old_decl) && ! DECL_WEAK (decl))
		DECL_EXTERNAL (old_decl) = 1;
	      else if (DECL_COMMON (decl) || DECL_ONE_ONLY (decl))
		DECL_EXTERNAL (decl) = 1;
	      else if (DECL_COMMON (old_decl) || DECL_ONE_ONLY (old_decl))
		DECL_EXTERNAL (old_decl) = 1;

	      if (DECL_EXTERNAL (decl))
		{
		  DECL_INITIAL (decl) = NULL_TREE;
		  DECL_COMMON (decl) = 0;
		  DECL_ONE_ONLY (decl) = 0;
		  DECL_WEAK (decl) = 0;
		}
	      else if (DECL_EXTERNAL (old_decl))
		{
		  DECL_INITIAL (old_decl) = NULL_TREE;
		  DECL_COMMON (old_decl) = 0;
		  DECL_ONE_ONLY (old_decl) = 0;
		  DECL_WEAK (old_decl) = 0;
		  *slot = decl;
		}
	      else
		{
		  error ("%Jredefinition of global '%D'", decl, decl);
		  error ("%J'%D' previously defined here", old_decl, old_decl);
		}
	    }
	  else
	    *slot = decl;
	}

  /* Now insert the desired information from all the definitions
     into any plain declarations.  */
  for (tu = tu_list; tu; tu = TREE_CHAIN (tu))
    for (decl = BLOCK_VARS (DECL_INITIAL (tu)); decl; decl = TREE_CHAIN (decl))
      if (TREE_PUBLIC (decl) && DECL_EXTERNAL (decl))
	{
	  tree global_decl;
	  global_decl = htab_find (link_hash_table, decl);

	  if (! global_decl)
	    continue;

	  /* Print any appropriate error messages, and partially merge
	     the decls.  */
	  (void) duplicate_decls (decl, global_decl);
	}

  htab_delete (link_hash_table);
}

/* Perform final processing on file-scope data.  */

void
c_write_global_declarations(void)
{
  tree link;

  for (link = current_file_decl; link; link = TREE_CHAIN (link))
    {
      tree globals = BLOCK_VARS (DECL_INITIAL (link));
      int len = list_length (globals);
      tree *vec = xmalloc (sizeof (tree) * len);
      int i;
      tree decl;

      /* Process the decls in the order they were written.  */

      for (i = 0, decl = globals; i < len; i++, decl = TREE_CHAIN (decl))
	vec[i] = decl;

      wrapup_global_declarations (vec, len);

      check_global_declarations (vec, len);

      /* Clean up.  */
      free (vec);
    }
}

/* Reset the parser's state in preparation for a new file.  */

void
c_reset_state (void)
{
  tree link;
  tree file_scope_decl;

  /* Pop the global scope.  */
  if (current_scope != global_scope)
      current_scope = global_scope;
  file_scope_decl = current_file_decl;
  DECL_INITIAL (file_scope_decl) = poplevel (1, 0, 0);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (file_scope_decl)) = file_scope_decl;
  truly_local_externals = NULL_TREE;

  /* Start a new global binding level.  */
  pushlevel (0);
  global_scope = current_scope;
  current_file_decl = build_decl (TRANSLATION_UNIT_DECL, NULL, NULL);
  TREE_CHAIN (current_file_decl) = file_scope_decl;

  /* Reintroduce the builtin declarations.  */
  for (link = first_builtin_decl;
       link != TREE_CHAIN (last_builtin_decl);
       link = TREE_CHAIN (link))
    pushdecl (copy_node (link));
}

#include "gt-c-decl.h"
