/* Process declarations and variables for C compiler.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002 Free Software Foundation, Inc.

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
#include "timevar.h"
#include "c-common.h"
#include "c-pragma.h"
#include "libfuncs.h"
#include "except.h"

/* In grokdeclarator, distinguish syntactic contexts of declarators.  */
enum decl_context
{ NORMAL,			/* Ordinary declaration */
  FUNCDEF,			/* Function definition */
  PARM,				/* Declaration of parm before function body */
  FIELD,			/* Declaration inside struct or union */
  BITFIELD,			/* Likewise but with specified width */
  TYPENAME};			/* Typename (inside cast or sizeof)  */


/* Nonzero if we have seen an invalid cross reference
   to a struct, union, or enum, but not yet printed the message.  */

tree pending_invalid_xref;
/* File and line to appear in the eventual error message.  */
const char *pending_invalid_xref_file;
int pending_invalid_xref_line;

/* While defining an enum type, this is 1 plus the last enumerator
   constant value.  Note that will do not have to save this or `enum_overflow'
   around nested function definition since such a definition could only
   occur in an enum value expression and we don't use these variables in
   that case.  */

static tree enum_next_value;

/* Nonzero means that there was overflow computing enum_next_value.  */

static int enum_overflow;

/* Parsing a function declarator leaves a list of parameter names
   or a chain or parameter decls here.  */

static tree last_function_parms;

/* Parsing a function declarator leaves here a chain of structure
   and enum types declared in the parmlist.  */

static tree last_function_parm_tags;

/* After parsing the declarator that starts a function definition,
   `start_function' puts here the list of parameter names or chain of decls.
   `store_parm_decls' finds it here.  */

static tree current_function_parms;

/* Similar, for last_function_parm_tags.  */
static tree current_function_parm_tags;

/* Similar, for the file and line that the prototype came from if this is
   an old-style definition.  */
static const char *current_function_prototype_file;
static int current_function_prototype_line;

/* The current statement tree.  */

static GTY(()) struct stmt_tree_s c_stmt_tree;

/* The current scope statement stack.  */

static GTY(()) tree c_scope_stmt_stack;

/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */

static GTY(()) tree named_labels;

/* A list of LABEL_DECLs from outer contexts that are currently shadowed.  */

static GTY(()) tree shadowed_labels;

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

/* For each binding contour we allocate a binding_level structure
 * which records the names defined in that contour.
 * Contours include:
 *  0) the global one
 *  1) one for each function definition,
 *     where internal declarations of the parameters appear.
 *  2) one for each compound statement,
 *     to record its declarations.
 *
 * The current meaning of a name can be found by searching the levels from
 * the current one out to the global one.
 */

/* Note that the information in the `names' component of the global contour
   is duplicated in the IDENTIFIER_GLOBAL_VALUEs of all identifiers.  */

struct binding_level GTY(())
  {
    /* A chain of _DECL nodes for all variables, constants, functions,
       and typedef types.  These are in the reverse of the order supplied.
     */
    tree names;

    /* A list of structure, union and enum definitions,
     * for looking up tag names.
     * It is a chain of TREE_LIST nodes, each of whose TREE_PURPOSE is a name,
     * or NULL_TREE; and whose TREE_VALUE is a RECORD_TYPE, UNION_TYPE,
     * or ENUMERAL_TYPE node.
     */
    tree tags;

    /* For each level, a list of shadowed outer-level local definitions
       to be restored when this level is popped.
       Each link is a TREE_LIST whose TREE_PURPOSE is an identifier and
       whose TREE_VALUE is its old definition (a kind of ..._DECL node).  */
    tree shadowed;

    /* For each level (except not the global one),
       a chain of BLOCK nodes for all the levels
       that were entered and exited one level down.  */
    tree blocks;

    /* The BLOCK node for this level, if one has been preallocated.
       If 0, the BLOCK is allocated (if needed) when the level is popped.  */
    tree this_block;

    /* The binding level which this one is contained in (inherits from).  */
    struct binding_level *level_chain;

    /* Nonzero for the level that holds the parameters of a function.  */
    char parm_flag;

    /* Nonzero if this level "doesn't exist" for tags.  */
    char tag_transparent;

    /* Nonzero if sublevels of this level "don't exist" for tags.
       This is set in the parm level of a function definition
       while reading the function body, so that the outermost block
       of the function body will be tag-transparent.  */
    char subblocks_tag_transparent;

    /* Nonzero means make a BLOCK for this level regardless of all else.  */
    char keep;

    /* Nonzero means make a BLOCK if this level has any subblocks.  */
    char keep_if_subblocks;

    /* List of decls in `names' that have incomplete structure or
       union types.  */
    tree incomplete_list;

    /* A list of decls giving the (reversed) specified order of parms,
       not including any forward-decls in the parmlist.
       This is so we can put the parms in proper order for assign_parms.  */
    tree parm_order;
  };

#define NULL_BINDING_LEVEL (struct binding_level *) NULL

/* The binding level currently in effect.  */

static GTY(()) struct binding_level *current_binding_level;

/* A chain of binding_level structures awaiting reuse.  */

static GTY((deletable (""))) struct binding_level *free_binding_level;

/* The outermost binding level, for names of file scope.
   This is created when the compiler is started and exists
   through the entire run.  */

static GTY(()) struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */

static struct binding_level clear_binding_level
  = {NULL, NULL, NULL, NULL, NULL, NULL_BINDING_LEVEL, 0, 0, 0, 0, 0, NULL,
     NULL};

/* Nonzero means unconditionally make a BLOCK for the next level pushed.  */

static int keep_next_level_flag;

/* Nonzero means make a BLOCK for the next level pushed
   if it has subblocks.  */

static int keep_next_if_subblocks;

/* The chain of outer levels of label scopes.
   This uses the same data structure used for binding levels,
   but it works differently: each link in the chain records
   saved values of named_labels and shadowed_labels for
   a label binding level outside the current one.  */

static GTY(()) struct binding_level *label_level_chain;

/* Functions called automatically at the beginning and end of execution.  */

tree static_ctors, static_dtors;

/* Forward declarations.  */

static struct binding_level * make_binding_level	PARAMS ((void));
static void pop_binding_level		PARAMS ((struct binding_level **));
static void clear_limbo_values		PARAMS ((tree));
static int duplicate_decls		PARAMS ((tree, tree, int));
static int redeclaration_error_message	PARAMS ((tree, tree));
static void storedecls			PARAMS ((tree));
static void storetags			PARAMS ((tree));
static tree lookup_tag			PARAMS ((enum tree_code, tree,
						 struct binding_level *, int));
static tree lookup_tag_reverse		PARAMS ((tree));
static tree grokdeclarator		PARAMS ((tree, tree, enum decl_context,
						 int));
static tree grokparms			PARAMS ((tree, int));
static void layout_array_type		PARAMS ((tree));
static tree c_make_fname_decl           PARAMS ((tree, int));
static void c_expand_body               PARAMS ((tree, int, int));
static void warn_if_shadowing		PARAMS ((tree, tree));
static bool flexible_array_type_p	PARAMS ((tree));
static tree set_save_expr_context	PARAMS ((tree *, int *, void *));

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
c_print_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  print_node (file, "global", IDENTIFIER_GLOBAL_VALUE (node), indent + 4);
  print_node (file, "local", IDENTIFIER_LOCAL_VALUE (node), indent + 4);
  print_node (file, "label", IDENTIFIER_LABEL_VALUE (node), indent + 4);
  print_node (file, "implicit", IDENTIFIER_IMPLICIT_DECL (node), indent + 4);
  print_node (file, "error locus", IDENTIFIER_ERROR_LOCUS (node), indent + 4);
  print_node (file, "limbo value", IDENTIFIER_LIMBO_VALUE (node), indent + 4);
  if (C_IS_RESERVED_WORD (node))
    {
      tree rid = ridpointers[C_RID_CODE (node)];
      indent_to (file, indent + 4);
      fprintf (file, "rid ");
      fprintf (file, HOST_PTR_PRINTF, (void *)rid);
      fprintf (file, " \"%s\"", IDENTIFIER_POINTER (rid));
    }
}

/* Hook called at end of compilation to assume 1 elt
   for a top-level tentative array defn that wasn't complete before.  */

void
c_finish_incomplete_decl (decl)
     tree decl;
{
  if (TREE_CODE (decl) == VAR_DECL)
    {
      tree type = TREE_TYPE (decl);
      if (type != error_mark_node
	  && TREE_CODE (type) == ARRAY_TYPE
	  && ! DECL_EXTERNAL (decl)
	  && TYPE_DOMAIN (type) == 0)
	{
	  warning_with_decl (decl, "array `%s' assumed to have one element");

	  complete_array_type (type, NULL_TREE, 1);

	  layout_decl (decl, 0);
	}
    }
}

/* Reuse or create a struct for this binding level.  */

static struct binding_level *
make_binding_level ()
{
  if (free_binding_level)
    {
      struct binding_level *result = free_binding_level;
      free_binding_level = result->level_chain;
      return result;
    }
  else
    return (struct binding_level *) ggc_alloc (sizeof (struct binding_level));
}

/* Remove a binding level from a list and add it to the level chain.  */

static void
pop_binding_level (lp)
     struct binding_level **lp;
{
  struct binding_level *l = *lp;
  *lp = l->level_chain;
  
  memset (l, 0, sizeof (struct binding_level));
  l->level_chain = free_binding_level;
  free_binding_level = l;
}

/* Nonzero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  return current_binding_level == global_binding_level;
}

void
keep_next_level ()
{
  keep_next_level_flag = 1;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p ()
{
  return ((current_binding_level->keep_if_subblocks
	   && current_binding_level->blocks != 0)
	  || current_binding_level->keep
	  || current_binding_level->names != 0
	  || (current_binding_level->tags != 0
	      && !current_binding_level->tag_transparent));
}

/* Identify this binding level as a level of parameters.
   DEFINITION_FLAG is 1 for a definition, 0 for a declaration.
   But it turns out there is no way to pass the right value for
   DEFINITION_FLAG, so we ignore it.  */

void
declare_parm_level (definition_flag)
     int definition_flag ATTRIBUTE_UNUSED;
{
  current_binding_level->parm_flag = 1;
}

/* Nonzero if currently making parm declarations.  */

int
in_parm_level_p ()
{
  return current_binding_level->parm_flag;
}

/* Enter a new binding level.
   If TAG_TRANSPARENT is nonzero, do so only for the name space of variables,
   not for that of tags.  */

void
pushlevel (tag_transparent)
     int tag_transparent;
{
  struct binding_level *newlevel = NULL_BINDING_LEVEL;

  /* If this is the top level of a function,
     just make sure that NAMED_LABELS is 0.  */

  if (current_binding_level == global_binding_level)
    {
      named_labels = 0;
    }

  newlevel = make_binding_level ();

  /* Add this level to the front of the chain (stack) of levels that
     are active.  */

  *newlevel = clear_binding_level;
  newlevel->tag_transparent
    = (tag_transparent
       || (current_binding_level
	   ? current_binding_level->subblocks_tag_transparent
	   : 0));
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
  newlevel->keep = keep_next_level_flag;
  keep_next_level_flag = 0;
  newlevel->keep_if_subblocks = keep_next_if_subblocks;
  keep_next_if_subblocks = 0;
}

/* Clear the limbo values of all identifiers defined in BLOCK or a subblock.  */

static void
clear_limbo_values (block)
     tree block;
{
  tree tem;

  for (tem = BLOCK_VARS (block); tem; tem = TREE_CHAIN (tem))
    if (DECL_NAME (tem) != 0)
      IDENTIFIER_LIMBO_VALUE (DECL_NAME (tem)) = 0;

  for (tem = BLOCK_SUBBLOCKS (block); tem; tem = TREE_CHAIN (tem))
    clear_limbo_values (tem);
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  tree link;
  /* The chain of decls was accumulated in reverse order.
     Put it into forward order, just for cleanliness.  */
  tree decls;
  tree tags = current_binding_level->tags;
  tree subblocks = current_binding_level->blocks;
  tree block = 0;
  tree decl;
  int block_previously_created;

  keep |= current_binding_level->keep;

  /* This warning is turned off because it causes warnings for
     declarations like `extern struct foo *x'.  */
#if 0
  /* Warn about incomplete structure types in this level.  */
  for (link = tags; link; link = TREE_CHAIN (link))
    if (!COMPLETE_TYPE_P (TREE_VALUE (link)))
      {
	tree type = TREE_VALUE (link);
	tree type_name = TYPE_NAME (type);
	char *id = IDENTIFIER_POINTER (TREE_CODE (type_name) == IDENTIFIER_NODE
				       ? type_name
				       : DECL_NAME (type_name));
	switch (TREE_CODE (type))
	  {
	  case RECORD_TYPE:
	    error ("`struct %s' incomplete in scope ending here", id);
	    break;
	  case UNION_TYPE:
	    error ("`union %s' incomplete in scope ending here", id);
	    break;
	  case ENUMERAL_TYPE:
	    error ("`enum %s' incomplete in scope ending here", id);
	    break;
	  }
      }
#endif /* 0 */

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
	&& DECL_INITIAL (decl) != 0
	&& TREE_ADDRESSABLE (decl))
      {
	/* If this decl was copied from a file-scope decl
	   on account of a block-scope extern decl,
	   propagate TREE_ADDRESSABLE to the file-scope decl.

	   DECL_ABSTRACT_ORIGIN can be set to itself if warn_return_type is
	   true, since then the decl goes through save_for_inline_copying.  */
	if (DECL_ABSTRACT_ORIGIN (decl) != 0
	    && DECL_ABSTRACT_ORIGIN (decl) != decl)
	  TREE_ADDRESSABLE (DECL_ABSTRACT_ORIGIN (decl)) = 1;
      }

  /* We used to warn about unused variables in expand_end_bindings,
     i.e. while generating RTL.  But in function-at-a-time mode we may
     choose to never expand a function at all (e.g. auto inlining), so
     we do this explicitly now.  */
  warn_about_unused_variables (getdecls ());

  /* If there were any declarations or structure tags in that level,
     or if this level is a function body,
     create a BLOCK to record them for the life of this function.  */

  block = 0;
  block_previously_created = (current_binding_level->this_block != 0);
  if (block_previously_created)
    block = current_binding_level->this_block;
  else if (keep || functionbody
	   || (current_binding_level->keep_if_subblocks && subblocks != 0))
    block = make_node (BLOCK);
  if (block != 0)
    {
      BLOCK_VARS (block) = decls;
      BLOCK_SUBBLOCKS (block) = subblocks;
    }

  /* In each subblock, record that this is its superior.  */

  for (link = subblocks; link; link = TREE_CHAIN (link))
    BLOCK_SUPERCONTEXT (link) = block;

  /* Clear out the meanings of the local variables of this level.  */

  for (link = decls; link; link = TREE_CHAIN (link))
    {
      if (DECL_NAME (link) != 0)
	{
	  /* If the ident. was used or addressed via a local extern decl,
	     don't forget that fact.  */
	  if (DECL_EXTERNAL (link))
	    {
	      if (TREE_USED (link))
		TREE_USED (DECL_NAME (link)) = 1;
	      if (TREE_ADDRESSABLE (link))
		TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (link)) = 1;
	    }
	  IDENTIFIER_LOCAL_VALUE (DECL_NAME (link)) = 0;
	}
    }

  /* Restore all name-meanings of the outer levels
     that were shadowed by this level.  */

  for (link = current_binding_level->shadowed; link; link = TREE_CHAIN (link))
    IDENTIFIER_LOCAL_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);

  /* If the level being exited is the top level of a function,
     check over all the labels, and clear out the current
     (function local) meanings of their names.  */

  if (functionbody)
    {
      clear_limbo_values (block);

      /* If this is the top level block of a function,
	 the vars are the function's parameters.
	 Don't leave them in the BLOCK because they are
	 found in the FUNCTION_DECL instead.  */

      BLOCK_VARS (block) = 0;

      /* Clear out the definitions of all label names,
	 since their scopes end here,
	 and add them to BLOCK_VARS.  */

      for (link = named_labels; link; link = TREE_CHAIN (link))
	{
	  tree label = TREE_VALUE (link);

	  if (DECL_INITIAL (label) == 0)
	    {
	      error_with_decl (label, "label `%s' used but not defined");
	      /* Avoid crashing later.  */
	      define_label (input_filename, lineno,
			    DECL_NAME (label));
	    }
	  else if (warn_unused_label && !TREE_USED (label))
	    warning_with_decl (label, "label `%s' defined but not used");
	  IDENTIFIER_LABEL_VALUE (DECL_NAME (label)) = 0;

	  /* Put the labels into the "variables" of the
	     top-level block, so debugger can see them.  */
	  TREE_CHAIN (label) = BLOCK_VARS (block);
	  BLOCK_VARS (block) = label;
	}
    }

  /* Pop the current level, and free the structure for reuse.  */

  pop_binding_level (&current_binding_level);

  /* Dispose of the block that we just made inside some higher level.  */
  if (functionbody)
    DECL_INITIAL (current_function_decl) = block;
  else if (block)
    {
      if (!block_previously_created)
	current_binding_level->blocks
	  = chainon (current_binding_level->blocks, block);
    }
  /* If we did not make a block for the level just exited,
     any blocks made for inner levels
     (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks
     of something else.  */
  else if (subblocks)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblocks);

  /* Set the TYPE_CONTEXTs for all of the tagged types belonging to this
     binding contour so that they point to the appropriate construct, i.e.
     either to the current FUNCTION_DECL node, or else to the BLOCK node
     we just constructed.

     Note that for tagged types whose scope is just the formal parameter
     list for some function type specification, we can't properly set
     their TYPE_CONTEXTs here, because we don't have a pointer to the
     appropriate FUNCTION_TYPE node readily available to us.  For those
     cases, the TYPE_CONTEXTs of the relevant tagged type nodes get set
     in `grokdeclarator' as soon as we have created the FUNCTION_TYPE
     node which will represent the "scope" for these "parameter list local"
     tagged types.  */

  if (functionbody)
    for (link = tags; link; link = TREE_CHAIN (link))
      TYPE_CONTEXT (TREE_VALUE (link)) = current_function_decl;
  else if (block)
    for (link = tags; link; link = TREE_CHAIN (link))
      TYPE_CONTEXT (TREE_VALUE (link)) = block;

  if (block)
    TREE_USED (block) = 1;

  return block;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (block)
     tree block;
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (block)
     tree block;
{
  current_binding_level->this_block = block;
  current_binding_level->names = chainon (current_binding_level->names,
					  BLOCK_VARS (block));
  current_binding_level->blocks = chainon (current_binding_level->blocks,
					   BLOCK_SUBBLOCKS (block));
}

void
push_label_level ()
{
  struct binding_level *newlevel;

  newlevel = make_binding_level ();

  /* Add this level to the front of the chain (stack) of label levels.  */

  newlevel->level_chain = label_level_chain;
  label_level_chain = newlevel;

  newlevel->names = named_labels;
  newlevel->shadowed = shadowed_labels;
  named_labels = 0;
  shadowed_labels = 0;
}

void
pop_label_level ()
{
  struct binding_level *level = label_level_chain;
  tree link, prev;

  /* Clear out the definitions of the declared labels in this level.
     Leave in the list any ordinary, non-declared labels.  */
  for (link = named_labels, prev = 0; link;)
    {
      if (C_DECLARED_LABEL_FLAG (TREE_VALUE (link)))
	{
	  if (DECL_SOURCE_LINE (TREE_VALUE (link)) == 0)
	    {
	      error_with_decl (TREE_VALUE (link),
			       "label `%s' used but not defined");
	      /* Avoid crashing later.  */
	      define_label (input_filename, lineno,
			    DECL_NAME (TREE_VALUE (link)));
	    }
	  else if (warn_unused_label && !TREE_USED (TREE_VALUE (link)))
	    warning_with_decl (TREE_VALUE (link),
			       "label `%s' defined but not used");
	  IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link))) = 0;

	  /* Delete this element from the list.  */
	  link = TREE_CHAIN (link);
	  if (prev)
	    TREE_CHAIN (prev) = link;
	  else
	    named_labels = link;
	}
      else
	{
	  prev = link;
	  link = TREE_CHAIN (link);
	}
    }

  /* Bring back all the labels that were shadowed.  */
  for (link = shadowed_labels; link; link = TREE_CHAIN (link))
    if (DECL_NAME (TREE_VALUE (link)) != 0)
      IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link)))
	= TREE_VALUE (link);

  named_labels = chainon (named_labels, level->names);
  shadowed_labels = level->shadowed;

  /* Pop the current level, and free the structure for reuse.  */
  pop_binding_level (&label_level_chain);
}

/* Push a definition or a declaration of struct, union or enum tag "name".
   "type" should be the type node.
   We assume that the tag "name" is not already defined.

   Note that the definition may really be just a forward reference.
   In that case, the TYPE_SIZE will be zero.  */

void
pushtag (name, type)
     tree name, type;
{
  struct binding_level *b;

  /* Find the proper binding level for this type tag.  */

  for (b = current_binding_level; b->tag_transparent; b = b->level_chain)
    continue;

  if (name)
    {
      /* Record the identifier as the type's name if it has none.  */

      if (TYPE_NAME (type) == 0)
	TYPE_NAME (type) = name;
    }

  b->tags = tree_cons (name, type, b->tags);

  /* Create a fake NULL-named TYPE_DECL node whose TREE_TYPE will be the
     tagged type we just added to the current binding level.  This fake
     NULL-named TYPE_DECL node helps dwarfout.c to know when it needs
     to output a representation of a tagged type, and it also gives
     us a convenient place to record the "scope start" address for the
     tagged type.  */

  TYPE_STUB_DECL (type) = pushdecl (build_decl (TYPE_DECL, NULL_TREE, type));

  /* An approximation for now, so we can tell this is a function-scope tag.
     This will be updated in poplevel.  */
  TYPE_CONTEXT (type) = DECL_CONTEXT (TYPE_STUB_DECL (type));
}

/* Handle when a new declaration NEWDECL
   has the same name as an old one OLDDECL
   in the same binding contour.
   Prints an error message if appropriate.

   If safely possible, alter OLDDECL to look like NEWDECL, and return 1.
   Otherwise, return 0.

   When DIFFERENT_BINDING_LEVEL is true, NEWDECL is an external declaration,
   and OLDDECL is in an outer binding level and should thus not be changed.  */

static int
duplicate_decls (newdecl, olddecl, different_binding_level)
     tree newdecl, olddecl;
     int different_binding_level;
{
  int types_match = comptypes (TREE_TYPE (newdecl), TREE_TYPE (olddecl));
  int new_is_definition = (TREE_CODE (newdecl) == FUNCTION_DECL
			   && DECL_INITIAL (newdecl) != 0);
  tree oldtype = TREE_TYPE (olddecl);
  tree newtype = TREE_TYPE (newdecl);
  int errmsg = 0;

  if (DECL_P (olddecl))
    {
      if (TREE_CODE (newdecl) == FUNCTION_DECL
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
		   && ! DECL_DECLARED_INLINE_P (olddecl)
		   && DECL_UNINLINABLE (olddecl)
		   && lookup_attribute ("noinline", DECL_ATTRIBUTES (olddecl)))
	    {
	      warning_with_decl (newdecl,
				 "function `%s' redeclared as inline");
	      warning_with_decl (olddecl,
				 "previous declaration of function `%s' with attribute noinline");
	    }
	  else if (DECL_DECLARED_INLINE_P (olddecl)
		   && DECL_UNINLINABLE (newdecl)
		   && lookup_attribute ("noinline", DECL_ATTRIBUTES (newdecl)))
	    {
	      warning_with_decl (newdecl,
				 "function `%s' redeclared with attribute noinline");
	      warning_with_decl (olddecl,
				 "previous declaration of function `%s' was inline");
	    }
	}

      DECL_ATTRIBUTES (newdecl)
	= (*targetm.merge_decl_attributes) (olddecl, newdecl);
    }

  if (TREE_CODE (newtype) == ERROR_MARK
      || TREE_CODE (oldtype) == ERROR_MARK)
    types_match = 0;

  /* New decl is completely inconsistent with the old one =>
     tell caller to replace the old one.
     This is always an error except in the case of shadowing a builtin.  */
  if (TREE_CODE (olddecl) != TREE_CODE (newdecl))
    {
      if (TREE_CODE (olddecl) == FUNCTION_DECL
	  && (DECL_BUILT_IN (olddecl)
	      || DECL_BUILT_IN_NONANSI (olddecl)))
	{
	  /* If you declare a built-in or predefined function name as static,
	     the old definition is overridden,
	     but optionally warn this was a bad choice of name.  */
	  if (!TREE_PUBLIC (newdecl))
	    {
	      if (!warn_shadow)
		;
	      else if (DECL_BUILT_IN (olddecl))
		warning_with_decl (newdecl, "shadowing built-in function `%s'");
	      else
		warning_with_decl (newdecl, "shadowing library function `%s'");
	    }
	  /* Likewise, if the built-in is not ansi, then programs can
	     override it even globally without an error.  */
	  else if (! DECL_BUILT_IN (olddecl))
	    warning_with_decl (newdecl,
			       "library function `%s' declared as non-function");

	  else if (DECL_BUILT_IN_NONANSI (olddecl))
	    warning_with_decl (newdecl,
			       "built-in function `%s' declared as non-function");
	  else
	    warning_with_decl (newdecl,
			       "built-in function `%s' declared as non-function");
	}
      else
	{
	  error_with_decl (newdecl, "`%s' redeclared as different kind of symbol");
	  error_with_decl (olddecl, "previous declaration of `%s'");
	}

      return 0;
    }

  /* For real parm decl following a forward decl,
     return 1 so old decl will be reused.  */
  if (types_match && TREE_CODE (newdecl) == PARM_DECL
      && TREE_ASM_WRITTEN (olddecl) && ! TREE_ASM_WRITTEN (newdecl))
    return 1;

  /* The new declaration is the same kind of object as the old one.
     The declarations may partially match.  Print warnings if they don't
     match enough.  Ultimately, copy most of the information from the new
     decl to the old one, and keep using the old one.  */

  if (TREE_CODE (olddecl) == FUNCTION_DECL && DECL_BUILT_IN (olddecl))
    {
      /* A function declaration for a built-in function.  */
      if (!TREE_PUBLIC (newdecl))
	{
	  /* If you declare a built-in function name as static, the
	     built-in definition is overridden,
	     but optionally warn this was a bad choice of name.  */
	  if (warn_shadow)
	    warning_with_decl (newdecl, "shadowing built-in function `%s'");
	  /* Discard the old built-in function.  */
	  return 0;
	}
      else if (!types_match)
	{
	  /* Accept the return type of the new declaration if same modes.  */
	  tree oldreturntype = TREE_TYPE (oldtype);
	  tree newreturntype = TREE_TYPE (newtype);

	  if (TYPE_MODE (oldreturntype) == TYPE_MODE (newreturntype))
	    {
	      /* Function types may be shared, so we can't just modify
		 the return type of olddecl's function type.  */
	      tree trytype
		= build_function_type (newreturntype,
				       TYPE_ARG_TYPES (oldtype));
	      trytype = build_type_attribute_variant (trytype,
						      TYPE_ATTRIBUTES (oldtype));

              types_match = comptypes (newtype, trytype);
	      if (types_match)
		oldtype = trytype;
	    }
	  /* Accept harmless mismatch in first argument type also.
	     This is for the ffs and fprintf builtins.  */
	  if (TYPE_ARG_TYPES (TREE_TYPE (newdecl)) != 0
	      && TYPE_ARG_TYPES (oldtype) != 0
	      && TREE_VALUE (TYPE_ARG_TYPES (newtype)) != 0
	      && TREE_VALUE (TYPE_ARG_TYPES (oldtype)) != 0
	      && (TYPE_MODE (TREE_VALUE (TYPE_ARG_TYPES (newtype)))
		  == TYPE_MODE (TREE_VALUE (TYPE_ARG_TYPES (oldtype)))))
	    {
	      /* Function types may be shared, so we can't just modify
		 the return type of olddecl's function type.  */
	      tree trytype
		= build_function_type (TREE_TYPE (oldtype),
				       tree_cons (NULL_TREE,
						  TREE_VALUE (TYPE_ARG_TYPES (newtype)),
						  TREE_CHAIN (TYPE_ARG_TYPES (oldtype))));
	      trytype = build_type_attribute_variant (trytype,
						      TYPE_ATTRIBUTES (oldtype));

	      types_match = comptypes (newtype, trytype);
	      if (types_match)
		oldtype = trytype;
	    }
	  if (! different_binding_level)
	    TREE_TYPE (olddecl) = oldtype;
	}
      else if (TYPE_ARG_TYPES (oldtype) == NULL
	       && TYPE_ARG_TYPES (newtype) != NULL)
	{
	  /* For bcmp, bzero, fputs the builtin type has arguments not
	     specified.  Use the ones from the prototype so that type checking
	     is done for them.  */
	  tree trytype
	    = build_function_type (TREE_TYPE (oldtype),
				   TYPE_ARG_TYPES (newtype));
	  trytype = build_type_attribute_variant (trytype,
						  TYPE_ATTRIBUTES (oldtype));

	  oldtype = trytype;
	  if (! different_binding_level)
	    TREE_TYPE (olddecl) = oldtype;
	}
      if (!types_match)
	{
	  /* If types don't match for a built-in, throw away the built-in.  */
	  warning_with_decl (newdecl, "conflicting types for built-in function `%s'");
	  return 0;
	}
    }
  else if (TREE_CODE (olddecl) == FUNCTION_DECL
	   && DECL_SOURCE_LINE (olddecl) == 0)
    {
      /* A function declaration for a predeclared function
	 that isn't actually built in.  */
      if (!TREE_PUBLIC (newdecl))
	{
	  /* If you declare it as static, the
	     default definition is overridden.  */
	  return 0;
	}
      else if (!types_match)
	{
	  /* If the types don't match, preserve volatility indication.
	     Later on, we will discard everything else about the
	     default declaration.  */
	  TREE_THIS_VOLATILE (newdecl) |= TREE_THIS_VOLATILE (olddecl);
	}
    }
  /* Permit char *foo () to match void *foo (...) if not pedantic,
     if one of them came from a system header file.  */
  else if (!types_match
	   && TREE_CODE (olddecl) == FUNCTION_DECL
	   && TREE_CODE (newdecl) == FUNCTION_DECL
	   && TREE_CODE (TREE_TYPE (oldtype)) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (newtype)) == POINTER_TYPE
	   && (DECL_IN_SYSTEM_HEADER (olddecl)
	       || DECL_IN_SYSTEM_HEADER (newdecl))
	   && ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (newtype))) == void_type_node
		&& TYPE_ARG_TYPES (oldtype) == 0
		&& self_promoting_args_p (TYPE_ARG_TYPES (newtype))
		&& TREE_TYPE (TREE_TYPE (oldtype)) == char_type_node)
	       ||
	       (TREE_TYPE (TREE_TYPE (newtype)) == char_type_node
		&& TYPE_ARG_TYPES (newtype) == 0
		&& self_promoting_args_p (TYPE_ARG_TYPES (oldtype))
		&& TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (oldtype))) == void_type_node)))
    {
      if (pedantic)
	pedwarn_with_decl (newdecl, "conflicting types for `%s'");
      /* Make sure we keep void * as ret type, not char *.  */
      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (oldtype))) == void_type_node)
	TREE_TYPE (newdecl) = newtype = oldtype;

      /* Set DECL_IN_SYSTEM_HEADER, so that if we see another declaration
	 we will come back here again.  */
      DECL_IN_SYSTEM_HEADER (newdecl) = 1;
    }
  else if (!types_match
	   /* Permit char *foo (int, ...); followed by char *foo ();
	      if not pedantic.  */
	   && ! (TREE_CODE (olddecl) == FUNCTION_DECL
		 && ! pedantic
		 /* Return types must still match.  */
		 && comptypes (TREE_TYPE (oldtype),
			       TREE_TYPE (newtype))
		 && TYPE_ARG_TYPES (newtype) == 0))
    {
      error_with_decl (newdecl, "conflicting types for `%s'");
      /* Check for function type mismatch
	 involving an empty arglist vs a nonempty one.  */
      if (TREE_CODE (olddecl) == FUNCTION_DECL
	  && comptypes (TREE_TYPE (oldtype),
			TREE_TYPE (newtype))
	  && ((TYPE_ARG_TYPES (oldtype) == 0
	       && DECL_INITIAL (olddecl) == 0)
	      ||
	      (TYPE_ARG_TYPES (newtype) == 0
	       && DECL_INITIAL (newdecl) == 0)))
	{
	  /* Classify the problem further.  */
	  tree t = TYPE_ARG_TYPES (oldtype);
	  if (t == 0)
	    t = TYPE_ARG_TYPES (newtype);
	  for (; t; t = TREE_CHAIN (t))
	    {
	      tree type = TREE_VALUE (t);

	      if (TREE_CHAIN (t) == 0
		  && TYPE_MAIN_VARIANT (type) != void_type_node)
		{
		  error ("a parameter list with an ellipsis can't match an empty parameter name list declaration");
		  break;
		}

	      if (c_type_promotes_to (type) != type)
		{
		  error ("an argument type that has a default promotion can't match an empty parameter name list declaration");
		  break;
		}
	    }
	}
      error_with_decl (olddecl, "previous declaration of `%s'");

      /* This is safer because the initializer might contain references
	 to variables that were declared between olddecl and newdecl. This
	 will make the initializer invalid for olddecl in case it gets
	 assigned to olddecl below.  */
      if (TREE_CODE (newdecl) == VAR_DECL)
	DECL_INITIAL (newdecl) = 0;
    }
  /* TLS cannot follow non-TLS declaration.  */
  else if (TREE_CODE (olddecl) == VAR_DECL && TREE_CODE (newdecl) == VAR_DECL
	   && !DECL_THREAD_LOCAL (olddecl) && DECL_THREAD_LOCAL (newdecl))
    {
      error_with_decl (newdecl, "thread-local declaration of `%s' follows non thread-local declaration");
      error_with_decl (olddecl, "previous declaration of `%s'");
    }
  /* non-TLS declaration cannot follow TLS declaration.  */
  else if (TREE_CODE (olddecl) == VAR_DECL && TREE_CODE (newdecl) == VAR_DECL
	   && DECL_THREAD_LOCAL (olddecl) && !DECL_THREAD_LOCAL (newdecl))
    {
      error_with_decl (newdecl, "non thread-local declaration of `%s' follows thread-local declaration");
      error_with_decl (olddecl, "previous declaration of `%s'");
    }
  else
    {
      errmsg = redeclaration_error_message (newdecl, olddecl);
      if (errmsg)
	{
	  switch (errmsg)
	    {
	    case 1:
	      error_with_decl (newdecl, "redefinition of `%s'");
	      break;
	    case 2:
	      error_with_decl (newdecl, "redeclaration of `%s'");
	      break;
	    case 3:
	      error_with_decl (newdecl, "conflicting declarations of `%s'");
	      break;
	    default:
	      abort ();
	    }

	  error_with_decl (olddecl,
			   ((DECL_INITIAL (olddecl)
			     && current_binding_level == global_binding_level)
			    ? "`%s' previously defined here"
			    : "`%s' previously declared here"));
	  return 0;
	}
      else if (TREE_CODE (newdecl) == TYPE_DECL
               && (DECL_IN_SYSTEM_HEADER (olddecl)
                   || DECL_IN_SYSTEM_HEADER (newdecl)))
	{
	  warning_with_decl (newdecl, "redefinition of `%s'");
	  warning_with_decl
	    (olddecl,
	     ((DECL_INITIAL (olddecl)
	       && current_binding_level == global_binding_level)
	      ? "`%s' previously defined here"
	      : "`%s' previously declared here"));
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_INITIAL (olddecl) != 0
	       && TYPE_ARG_TYPES (oldtype) == 0
	       && TYPE_ARG_TYPES (newtype) != 0
	       && TYPE_ACTUAL_ARG_TYPES (oldtype) != 0)
	{
	  tree type, parm;
	  int nargs;
	  /* Prototype decl follows defn w/o prototype.  */

	  for (parm = TYPE_ACTUAL_ARG_TYPES (oldtype),
	       type = TYPE_ARG_TYPES (newtype),
	       nargs = 1;
	       ;
	       parm = TREE_CHAIN (parm), type = TREE_CHAIN (type), nargs++)
	    {
	      if (TYPE_MAIN_VARIANT (TREE_VALUE (parm)) == void_type_node
		  && TYPE_MAIN_VARIANT (TREE_VALUE (type)) == void_type_node)
		{
		  warning_with_decl (newdecl, "prototype for `%s' follows");
		  warning_with_decl (olddecl, "non-prototype definition here");
		  break;
		}
	      if (TYPE_MAIN_VARIANT (TREE_VALUE (parm)) == void_type_node
		  || TYPE_MAIN_VARIANT (TREE_VALUE (type)) == void_type_node)
		{
		  error_with_decl (newdecl,
				   "prototype for `%s' follows and number of arguments doesn't match");
		  error_with_decl (olddecl, "non-prototype definition here");
		  errmsg = 1;
		  break;
		}
	      /* Type for passing arg must be consistent
		 with that declared for the arg.  */
	      if (! comptypes (TREE_VALUE (parm), TREE_VALUE (type)))
		{
		  error_with_decl (newdecl,
				   "prototype for `%s' follows and argument %d doesn't match",
				   nargs);
		  error_with_decl (olddecl, "non-prototype definition here");
		  errmsg = 1;
		  break;
		}
	    }
	}
      /* Warn about mismatches in various flags.  */
      else
	{
	  /* Warn if function is now inline
	     but was previously declared not inline and has been called.  */
	  if (TREE_CODE (olddecl) == FUNCTION_DECL
	      && ! DECL_DECLARED_INLINE_P (olddecl)
	      && DECL_DECLARED_INLINE_P (newdecl)
	      && TREE_USED (olddecl))
	    warning_with_decl (newdecl,
			       "`%s' declared inline after being called");
	  if (TREE_CODE (olddecl) == FUNCTION_DECL
	      && ! DECL_DECLARED_INLINE_P (olddecl)
	      && DECL_DECLARED_INLINE_P (newdecl)
	      && DECL_INITIAL (olddecl) != 0)
	    warning_with_decl (newdecl,
			       "`%s' declared inline after its definition");

	  /* If pedantic, warn when static declaration follows a non-static
	     declaration.  Otherwise, do so only for functions.  */
	  if ((pedantic || TREE_CODE (olddecl) == FUNCTION_DECL)
	      && TREE_PUBLIC (olddecl)
	      && !TREE_PUBLIC (newdecl))
	    warning_with_decl (newdecl, "static declaration for `%s' follows non-static");

	  /* If warn_traditional, warn when a non-static function
	     declaration follows a static one.  */
	  if (warn_traditional && !in_system_header
	      && TREE_CODE (olddecl) == FUNCTION_DECL
	      && !TREE_PUBLIC (olddecl)
	      && TREE_PUBLIC (newdecl))
	    warning_with_decl (newdecl, "non-static declaration for `%s' follows static");

	  /* Warn when const declaration follows a non-const
	     declaration, but not for functions.  */
	  if (TREE_CODE (olddecl) != FUNCTION_DECL
	      && !TREE_READONLY (olddecl)
	      && TREE_READONLY (newdecl))
	    warning_with_decl (newdecl, "const declaration for `%s' follows non-const");
	  /* These bits are logically part of the type, for variables.
	     But not for functions
	     (where qualifiers are not valid ANSI anyway).  */
	  else if (pedantic && TREE_CODE (olddecl) != FUNCTION_DECL
	      && (TREE_READONLY (newdecl) != TREE_READONLY (olddecl)
		  || TREE_THIS_VOLATILE (newdecl) != TREE_THIS_VOLATILE (olddecl)))
	    pedwarn_with_decl (newdecl, "type qualifiers for `%s' conflict with previous decl");
	}
    }

  /* Optionally warn about more than one declaration for the same name.  */
  if (errmsg == 0 && warn_redundant_decls && DECL_SOURCE_LINE (olddecl) != 0
      /* Don't warn about a function declaration
	 followed by a definition.  */
      && !(TREE_CODE (newdecl) == FUNCTION_DECL && DECL_INITIAL (newdecl) != 0
	   && DECL_INITIAL (olddecl) == 0)
      /* Don't warn about extern decl followed by (tentative) definition.  */
      && !(DECL_EXTERNAL (olddecl) && ! DECL_EXTERNAL (newdecl)))
    {
      warning_with_decl (newdecl, "redundant redeclaration of `%s' in same scope");
      warning_with_decl (olddecl, "previous declaration of `%s'");
    }

  /* Copy all the DECL_... slots specified in the new decl
     except for any that we copy here from the old type.

     Past this point, we don't change OLDTYPE and NEWTYPE
     even if we change the types of NEWDECL and OLDDECL.  */

  if (types_match)
    {
      /* When copying info to olddecl, we store into write_olddecl
	 instead.  This allows us to avoid modifying olddecl when
	 different_binding_level is true.  */
      tree write_olddecl = different_binding_level ? newdecl : olddecl;

      /* Merge the data types specified in the two decls.  */
      if (TREE_CODE (newdecl) != FUNCTION_DECL || !DECL_BUILT_IN (olddecl))
	{
	  if (different_binding_level)
	    {
	      if (TYPE_ARG_TYPES (oldtype) != 0
		  && TYPE_ARG_TYPES (newtype) == 0)
		TREE_TYPE (newdecl) = common_type (newtype, oldtype);
	      else
		TREE_TYPE (newdecl)
		  = build_type_attribute_variant
		    (newtype,
		     merge_attributes (TYPE_ATTRIBUTES (newtype),
				       TYPE_ATTRIBUTES (oldtype)));
	    }
	  else
	    TREE_TYPE (newdecl)
	      = TREE_TYPE (olddecl)
		= common_type (newtype, oldtype);
	}

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
	TREE_READONLY (write_olddecl) = 1;

      if (TREE_THIS_VOLATILE (newdecl))
	{
	  TREE_THIS_VOLATILE (write_olddecl) = 1;
	  if (TREE_CODE (newdecl) == VAR_DECL
	      /* If an automatic variable is re-declared in the same
		 function scope, but the old declaration was not
		 volatile, make_var_volatile() would crash because the
		 variable would have been assigned to a pseudo, not a
		 MEM.  Since this duplicate declaration is invalid
		 anyway, we just skip the call.  */
	      && errmsg == 0)
	    make_var_volatile (newdecl);
	}

      /* Keep source location of definition rather than declaration.  */
      /* When called with different_binding_level set, keep the old
	 information so that meaningful diagnostics can be given.  */
      if (DECL_INITIAL (newdecl) == 0 && DECL_INITIAL (olddecl) != 0
	  && ! different_binding_level)
	{
	  DECL_SOURCE_LINE (newdecl) = DECL_SOURCE_LINE (olddecl);
	  DECL_SOURCE_FILE (newdecl) = DECL_SOURCE_FILE (olddecl);
	}

      /* Merge the unused-warning information.  */
      if (DECL_IN_SYSTEM_HEADER (olddecl))
	DECL_IN_SYSTEM_HEADER (newdecl) = 1;
      else if (DECL_IN_SYSTEM_HEADER (newdecl))
	DECL_IN_SYSTEM_HEADER (write_olddecl) = 1;

      /* Merge the initialization information.  */
      /* When called with different_binding_level set, don't copy over
	 DECL_INITIAL, so that we don't accidentally change function
	 declarations into function definitions.  */
      if (DECL_INITIAL (newdecl) == 0 && ! different_binding_level)
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

      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  DECL_STATIC_CONSTRUCTOR(newdecl) |= DECL_STATIC_CONSTRUCTOR(olddecl);
	  DECL_STATIC_DESTRUCTOR (newdecl) |= DECL_STATIC_DESTRUCTOR (olddecl);
	  DECL_NO_LIMIT_STACK (newdecl) |= DECL_NO_LIMIT_STACK (olddecl);
	  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (newdecl)
	    |= DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (olddecl);
	}
    }
  /* If cannot merge, then use the new type and qualifiers,
     and don't preserve the old rtl.  */
  else if (! different_binding_level)
    {
      TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
      TREE_READONLY (olddecl) = TREE_READONLY (newdecl);
      TREE_THIS_VOLATILE (olddecl) = TREE_THIS_VOLATILE (newdecl);
      TREE_SIDE_EFFECTS (olddecl) = TREE_SIDE_EFFECTS (newdecl);
    }

  /* Merge the storage class information.  */
  merge_weak (newdecl, olddecl);

  /* For functions, static overrides non-static.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      TREE_PUBLIC (newdecl) &= TREE_PUBLIC (olddecl);
      /* This is since we don't automatically
	 copy the attributes of NEWDECL into OLDDECL.  */
      /* No need to worry about different_binding_level here because
	 then TREE_PUBLIC (newdecl) was true.  */
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
      /* If this clears `static', clear it in the identifier too.  */
      if (! TREE_PUBLIC (olddecl))
	TREE_PUBLIC (DECL_NAME (olddecl)) = 0;
    }
  if (DECL_EXTERNAL (newdecl))
    {
      if (! different_binding_level)
	{
	  /* Don't mess with these flags on local externs; they remain
	     external even if there's a declaration at file scope which
	     isn't.  */
	  TREE_STATIC (newdecl) = TREE_STATIC (olddecl);
	  DECL_EXTERNAL (newdecl) = DECL_EXTERNAL (olddecl);
	}
      /* An extern decl does not override previous storage class.  */
      TREE_PUBLIC (newdecl) = TREE_PUBLIC (olddecl);
      if (! DECL_EXTERNAL (newdecl))
	DECL_CONTEXT (newdecl) = DECL_CONTEXT (olddecl);
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
	  if (TREE_USED (olddecl))
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
	  /* Get rid of any built-in function if new arg types don't match it
	     or if we have a function definition.  */
	  if (! types_match || new_is_definition)
	    {
	      if (! different_binding_level)
		{
		  TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
		  DECL_BUILT_IN_CLASS (olddecl) = NOT_BUILT_IN;
		}
	    }
	  else
	    {
	      /* If redeclaring a builtin function, and not a definition,
		 it stays built in.  */
	      DECL_BUILT_IN_CLASS (newdecl) = DECL_BUILT_IN_CLASS (olddecl);
	      DECL_FUNCTION_CODE (newdecl) = DECL_FUNCTION_CODE (olddecl);
	    }
	}

      /* Also preserve various other info from the definition.  */
      if (! new_is_definition)
	{
	  DECL_RESULT (newdecl) = DECL_RESULT (olddecl);
	  /* When called with different_binding_level set, don't copy over
	     DECL_INITIAL, so that we don't accidentally change function
	     declarations into function definitions.  */
	  if (! different_binding_level)
	    DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
	  DECL_SAVED_INSNS (newdecl) = DECL_SAVED_INSNS (olddecl);
	  DECL_SAVED_TREE (newdecl) = DECL_SAVED_TREE (olddecl);
	  DECL_NUM_STMTS (newdecl) = DECL_NUM_STMTS (olddecl);
	  DECL_ARGUMENTS (newdecl) = DECL_ARGUMENTS (olddecl);

	  /* Set DECL_INLINE on the declaration if we've got a body
	     from which to instantiate.  */
	  if (DECL_INLINE (olddecl) && ! DECL_UNINLINABLE (newdecl))
	    {
	      DECL_INLINE (newdecl) = 1;
	      DECL_ABSTRACT_ORIGIN (newdecl)
		= (different_binding_level
		   ? DECL_ORIGIN (olddecl)
		   : DECL_ABSTRACT_ORIGIN (olddecl));
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
  if (different_binding_level)
    return 0;

  /* Copy most of the decl-specific fields of NEWDECL into OLDDECL.
     But preserve OLDDECL's DECL_UID.  */
  {
    unsigned olddecl_uid = DECL_UID (olddecl);

    memcpy ((char *) olddecl + sizeof (struct tree_common),
	    (char *) newdecl + sizeof (struct tree_common),
	    sizeof (struct tree_decl) - sizeof (struct tree_common));
    DECL_UID (olddecl) = olddecl_uid;
  }

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

  return 1;
}

/* Check whether decl-node X shadows an existing declaration.
   OLDLOCAL is the old IDENTIFIER_LOCAL_VALUE of the DECL_NAME of X,
   which might be a NULL_TREE.  */
static void
warn_if_shadowing (x, oldlocal)
     tree x, oldlocal;
{
  tree name;

  if (DECL_EXTERNAL (x))
    return;

  name = DECL_NAME (x);

  /* Warn if shadowing an argument at the top level of the body.  */
  if (oldlocal != 0
      /* This warning doesn't apply to the parms of a nested fcn.  */
      && ! current_binding_level->parm_flag
      /* Check that this is one level down from the parms.  */
      && current_binding_level->level_chain->parm_flag
      /* Check that the decl being shadowed
	 comes from the parm level, one level up.  */
      && chain_member (oldlocal, current_binding_level->level_chain->names))
    {
      if (TREE_CODE (oldlocal) == PARM_DECL)
	pedwarn ("declaration of `%s' shadows a parameter",
		 IDENTIFIER_POINTER (name));
      else
	pedwarn ("declaration of `%s' shadows a symbol from the parameter list",
		 IDENTIFIER_POINTER (name));
    }
  /* Maybe warn if shadowing something else.  */
  else if (warn_shadow
	   /* No shadow warnings for internally generated vars.  */
	   && DECL_SOURCE_LINE (x) != 0
	   /* No shadow warnings for vars made for inlining.  */
	   && ! DECL_FROM_INLINE (x))
    {
      if (TREE_CODE (x) == PARM_DECL
	  && current_binding_level->level_chain->parm_flag)
	/* Don't warn about the parm names in function declarator
	   within a function declarator.
	   It would be nice to avoid warning in any function
	   declarator in a declaration, as opposed to a definition,
	   but there is no way to tell it's not a definition.  */
	;
      else if (oldlocal)
	{
	  if (TREE_CODE (oldlocal) == PARM_DECL)
	    shadow_warning ("a parameter", name, oldlocal);
	  else
	    shadow_warning ("a previous local", name, oldlocal);
	}
      else if (IDENTIFIER_GLOBAL_VALUE (name) != 0
	       && IDENTIFIER_GLOBAL_VALUE (name) != error_mark_node)
	shadow_warning ("a global declaration", name,
			IDENTIFIER_GLOBAL_VALUE (name));
    }
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */

tree
pushdecl (x)
     tree x;
{
  tree t;
  tree name = DECL_NAME (x);
  struct binding_level *b = current_binding_level;

  /* Functions need the lang_decl data.  */
  if (TREE_CODE (x) == FUNCTION_DECL && ! DECL_LANG_SPECIFIC (x))
    DECL_LANG_SPECIFIC (x) = (struct lang_decl *)
      ggc_alloc_cleared (sizeof (struct lang_decl));

  DECL_CONTEXT (x) = current_function_decl;
  /* A local extern declaration for a function doesn't constitute nesting.
     A local auto declaration does, since it's a forward decl
     for a nested function coming later.  */
  if ((TREE_CODE (x) == FUNCTION_DECL || TREE_CODE (x) == VAR_DECL)
      && DECL_INITIAL (x) == 0 && DECL_EXTERNAL (x))
    DECL_CONTEXT (x) = 0;

  if (name)
    {
      int different_binding_level = 0;

      if (warn_nested_externs
	  && DECL_EXTERNAL (x)
	  && b != global_binding_level
	  && x != IDENTIFIER_IMPLICIT_DECL (name)
	  /* No error messages for __FUNCTION__ and __PRETTY_FUNCTION__.  */
	  && !DECL_IN_SYSTEM_HEADER (x))
	warning ("nested extern declaration of `%s'",
		 IDENTIFIER_POINTER (name));

      t = lookup_name_current_level (name);
      if (! t && DECL_EXTERNAL (x) && TREE_PUBLIC (x))
	{
	  t = IDENTIFIER_GLOBAL_VALUE (name);
	  /* Type decls at global scope don't conflict with externs declared
	     inside lexical blocks.  */
	  if (! t || TREE_CODE (t) == TYPE_DECL)
	    /* If there's no visible global declaration, try for an
               invisible one.  */
	    t = IDENTIFIER_LIMBO_VALUE (name);
	  different_binding_level = 1;
	}
      if (t != 0 && t == error_mark_node)
	/* error_mark_node is 0 for a while during initialization!  */
	{
	  t = 0;
	  error_with_decl (x, "`%s' used prior to declaration");
	}

      /* If this decl is `static' and an implicit decl was seen previously,
	 warn.  */
      if (TREE_PUBLIC (name)
	  /* Don't test for DECL_EXTERNAL, because grokdeclarator
	     sets this for all functions.  */
	  && ! TREE_PUBLIC (x)
	  && (TREE_CODE (x) == FUNCTION_DECL || b == global_binding_level)
	  /* We used to warn also for explicit extern followed by static,
	     but sometimes you need to do it that way.  */
	  && IDENTIFIER_IMPLICIT_DECL (name) != 0)
	{
	  pedwarn ("`%s' was declared implicitly `extern' and later `static'",
		   IDENTIFIER_POINTER (name));
	  pedwarn_with_file_and_line
	    (DECL_SOURCE_FILE (IDENTIFIER_IMPLICIT_DECL (name)),
	     DECL_SOURCE_LINE (IDENTIFIER_IMPLICIT_DECL (name)),
	     "previous declaration of `%s'",
	     IDENTIFIER_POINTER (name));
	  TREE_THIS_VOLATILE (name) = 1;
	}

      if (t != 0 && duplicate_decls (x, t, different_binding_level))
	{
	  if (TREE_CODE (t) == PARM_DECL)
	    {
	      /* Don't allow more than one "real" duplicate
		 of a forward parm decl.  */
	      TREE_ASM_WRITTEN (t) = TREE_ASM_WRITTEN (x);
	      return t;
	    }
	  return t;
	}

      /* If we are processing a typedef statement, generate a whole new
	 ..._TYPE node (which will be just a variant of the existing
	 ..._TYPE node with identical properties) and then install the
	 TYPE_DECL node generated to represent the typedef name as the
	 TYPE_NAME of this brand new (duplicate) ..._TYPE node.

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

      if (TREE_CODE (x) == TYPE_DECL)
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

      /* Multiple external decls of the same identifier ought to match.
	 We get warnings about inline functions where they are defined.
	 Avoid duplicate warnings where they are used.  */
      if (TREE_PUBLIC (x)
	  && ! (TREE_CODE (x) == FUNCTION_DECL && DECL_INLINE (x)))
	{
	  tree decl;

	  if (IDENTIFIER_LIMBO_VALUE (name) != 0)
	    /* Decls in limbo are always extern, so no need to check that.  */
	    decl = IDENTIFIER_LIMBO_VALUE (name);
	  else
	    decl = 0;

	  if (decl && ! comptypes (TREE_TYPE (x), TREE_TYPE (decl))
	      /* If old decl is built-in, we already warned if we should.  */
	      && !DECL_BUILT_IN (decl))
	    {
	      pedwarn_with_decl (x,
				 "type mismatch with previous external decl");
	      pedwarn_with_decl (decl, "previous external decl of `%s'");
	    }
	}

      /* If a function has had an implicit declaration, and then is defined,
	 make sure they are compatible.  */

      if (IDENTIFIER_IMPLICIT_DECL (name) != 0
	  && IDENTIFIER_GLOBAL_VALUE (name) == 0
	  && TREE_CODE (x) == FUNCTION_DECL
	  && ! comptypes (TREE_TYPE (x),
			  TREE_TYPE (IDENTIFIER_IMPLICIT_DECL (name))))
	{
	  warning_with_decl (x, "type mismatch with previous implicit declaration");
	  warning_with_decl (IDENTIFIER_IMPLICIT_DECL (name),
			     "previous implicit declaration of `%s'");
	}

      /* This name is new in its binding level.
	 Install the new declaration and return it.  */
      if (b == global_binding_level)
	{
	  /* Install a global value.  */

	  /* If the first global decl has external linkage,
	     warn if we later see static one.  */
	  if (IDENTIFIER_GLOBAL_VALUE (name) == 0 && TREE_PUBLIC (x))
	    TREE_PUBLIC (name) = 1;

	  IDENTIFIER_GLOBAL_VALUE (name) = x;

	  /* We no longer care about any previous block level declarations.  */
	  IDENTIFIER_LIMBO_VALUE (name) = 0;

	  /* Don't forget if the function was used via an implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_USED (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_USED (x) = 1, TREE_USED (name) = 1;

	  /* Don't forget if its address was taken in that way.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_ADDRESSABLE (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_ADDRESSABLE (x) = 1;

	  /* Warn about mismatches against previous implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name) != 0
	      /* If this real decl matches the implicit, don't complain.  */
	      && ! (TREE_CODE (x) == FUNCTION_DECL
		    && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (x)))
			== integer_type_node)))
	    pedwarn ("`%s' was previously implicitly declared to return `int'",
		     IDENTIFIER_POINTER (name));

	  /* If this decl is `static' and an `extern' was seen previously,
	     that is erroneous.  */
	  if (TREE_PUBLIC (name)
	      && ! TREE_PUBLIC (x) && ! DECL_EXTERNAL (x))
	    {
	      /* Okay to redeclare an ANSI built-in as static.  */
	      if (t != 0 && DECL_BUILT_IN (t))
		;
	      /* Okay to declare a non-ANSI built-in as anything.  */
	      else if (t != 0 && DECL_BUILT_IN_NONANSI (t))
		;
	      /* Okay to have global type decl after an earlier extern
		 declaration inside a lexical block.  */
	      else if (TREE_CODE (x) == TYPE_DECL)
		;
	      else if (IDENTIFIER_IMPLICIT_DECL (name))
		{
		  if (! TREE_THIS_VOLATILE (name))
		    pedwarn ("`%s' was declared implicitly `extern' and later `static'",
			     IDENTIFIER_POINTER (name));
		}
	      else
		pedwarn ("`%s' was declared `extern' and later `static'",
			 IDENTIFIER_POINTER (name));
	    }
	}
      else
	{
	  /* Here to install a non-global value.  */
	  tree oldlocal = IDENTIFIER_LOCAL_VALUE (name);
	  tree oldglobal = IDENTIFIER_GLOBAL_VALUE (name);

	  IDENTIFIER_LOCAL_VALUE (name) = x;

	  /* If this is an extern function declaration, see if we
	     have a global definition or declaration for the function.  */
	  if (oldlocal == 0
	      && oldglobal != 0
	      && TREE_CODE (x) == FUNCTION_DECL
	      && TREE_CODE (oldglobal) == FUNCTION_DECL
	      && DECL_EXTERNAL (x)
	      && ! DECL_DECLARED_INLINE_P (x))
	    {
	      /* We have one.  Their types must agree.  */
	      if (! comptypes (TREE_TYPE (x),
			       TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (name))))
		pedwarn_with_decl (x, "extern declaration of `%s' doesn't match global one");
	      else
		{
		  /* Inner extern decl is inline if global one is.
		     Copy enough to really inline it.  */
		  if (DECL_DECLARED_INLINE_P (oldglobal))
		    {
		      DECL_DECLARED_INLINE_P (x)
		        = DECL_DECLARED_INLINE_P (oldglobal);
		      DECL_INLINE (x) = DECL_INLINE (oldglobal);
		      DECL_INITIAL (x) = (current_function_decl == oldglobal
					  ? 0 : DECL_INITIAL (oldglobal));
		      DECL_SAVED_INSNS (x) = DECL_SAVED_INSNS (oldglobal);
		      DECL_NUM_STMTS (x) = DECL_NUM_STMTS (oldglobal);
		      DECL_ARGUMENTS (x) = DECL_ARGUMENTS (oldglobal);
		      DECL_RESULT (x) = DECL_RESULT (oldglobal);
		      TREE_ASM_WRITTEN (x) = TREE_ASM_WRITTEN (oldglobal);
		      DECL_ABSTRACT_ORIGIN (x)
			= DECL_ABSTRACT_ORIGIN (oldglobal);
		    }
		  /* Inner extern decl is built-in if global one is.  */
		  if (DECL_BUILT_IN (oldglobal))
		    {
		      DECL_BUILT_IN_CLASS (x) = DECL_BUILT_IN_CLASS (oldglobal);
		      DECL_FUNCTION_CODE (x) = DECL_FUNCTION_CODE (oldglobal);
		    }
		  /* Keep the arg types from a file-scope fcn defn.  */
		  if (TYPE_ARG_TYPES (TREE_TYPE (oldglobal)) != 0
		      && DECL_INITIAL (oldglobal)
		      && TYPE_ARG_TYPES (TREE_TYPE (x)) == 0)
		    TREE_TYPE (x) = TREE_TYPE (oldglobal);
		}
	    }

#if 0
	  /* This case is probably sometimes the right thing to do.  */
	  /* If we have a local external declaration,
	     then any file-scope declaration should not
	     have been static.  */
	  if (oldlocal == 0 && oldglobal != 0
	      && !TREE_PUBLIC (oldglobal)
	      && DECL_EXTERNAL (x) && TREE_PUBLIC (x))
	    warning ("`%s' locally external but globally static",
		     IDENTIFIER_POINTER (name));
#endif

	  /* If we have a local external declaration,
	     and no file-scope declaration has yet been seen,
	     then if we later have a file-scope decl it must not be static.  */
	  if (oldlocal == 0
	      && DECL_EXTERNAL (x)
	      && TREE_PUBLIC (x))
	    {
	      if (oldglobal == 0)
		TREE_PUBLIC (name) = 1;

	      /* Save this decl, so that we can do type checking against
		 other decls after it falls out of scope.

		 Only save it once.  This prevents temporary decls created in
		 expand_inline_function from being used here, since this
		 will have been set when the inline function was parsed.
		 It also helps give slightly better warnings.  */
	      if (IDENTIFIER_LIMBO_VALUE (name) == 0)
		IDENTIFIER_LIMBO_VALUE (name) = x;
	    }

	  warn_if_shadowing (x, oldlocal);

	  /* If storing a local value, there may already be one (inherited).
	     If so, record it for restoration when this binding level ends.  */
	  if (oldlocal != 0)
	    b->shadowed = tree_cons (name, oldlocal, b->shadowed);
	}

      /* Keep list of variables in this level with incomplete type.
	 If the input is erroneous, we can have error_mark in the type
	 slot (e.g. "f(void a, ...)") - that doesn't count as an
	 incomplete type.  */
      if (TREE_TYPE (x) != error_mark_node
	  && !COMPLETE_TYPE_P (TREE_TYPE (x)))
	{
	  tree element = TREE_TYPE (x);

	  while (TREE_CODE (element) == ARRAY_TYPE)
	    element = TREE_TYPE (element);
	  if (TREE_CODE (element) == RECORD_TYPE
	      || TREE_CODE (element) == UNION_TYPE)
	    b->incomplete_list = tree_cons (NULL_TREE, x, b->incomplete_list);
	}
    }

  /* Put decls on list in reverse order.
     We will reverse them later if necessary.  */
  TREE_CHAIN (x) = b->names;
  b->names = x;

  return x;
}

/* Like pushdecl, only it places X in GLOBAL_BINDING_LEVEL, if appropriate.  */

tree
pushdecl_top_level (x)
     tree x;
{
  tree t;
  struct binding_level *b = current_binding_level;

  current_binding_level = global_binding_level;
  t = pushdecl (x);
  current_binding_level = b;
  return t;
}

/* Generate an implicit declaration for identifier FUNCTIONID
   as a function of type int ().  Print a warning if appropriate.  */

tree
implicitly_declare (functionid)
     tree functionid;
{
  tree decl;
  int traditional_warning = 0;
  /* Only one "implicit declaration" warning per identifier.  */
  int implicit_warning;

  /* We used to reuse an old implicit decl here,
     but this loses with inline functions because it can clobber
     the saved decl chains.  */
#if 0
  if (IDENTIFIER_IMPLICIT_DECL (functionid) != 0)
    decl = IDENTIFIER_IMPLICIT_DECL (functionid);
  else
#endif
    decl = build_decl (FUNCTION_DECL, functionid, default_function_type);

  /* Warn of implicit decl following explicit local extern decl.
     This is probably a program designed for traditional C.  */
  if (TREE_PUBLIC (functionid) && IDENTIFIER_GLOBAL_VALUE (functionid) == 0)
    traditional_warning = 1;

  /* Warn once of an implicit declaration.  */
  implicit_warning = (IDENTIFIER_IMPLICIT_DECL (functionid) == 0);

  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  /* Record that we have an implicit decl and this is it.  */
  IDENTIFIER_IMPLICIT_DECL (functionid) = decl;

  /* ANSI standard says implicit declarations are in the innermost block.
     So we record the decl in the standard fashion.  */
  pushdecl (decl);

  /* This is a no-op in c-lang.c or something real in objc-act.c.  */
  if (flag_objc)
    objc_check_decl (decl);

  rest_of_decl_compilation (decl, NULL, 0, 0);

  if (implicit_warning)
    implicit_decl_warning (functionid);
  else if (warn_traditional && traditional_warning)
    warning ("function `%s' was previously declared within a block",
	     IDENTIFIER_POINTER (functionid));

  /* Write a record describing this implicit function declaration to the
     prototypes file (if requested).  */

  gen_aux_info_record (decl, 0, 1, 0);

  /* Possibly apply some default attributes to this implicit declaration.  */
  decl_attributes (&decl, NULL_TREE, 0);

  return decl;
}

void
implicit_decl_warning (id)
     tree id;
{
  const char *name = IDENTIFIER_POINTER (id);
  if (mesg_implicit_function_declaration == 2)
    error ("implicit declaration of function `%s'", name);
  else if (mesg_implicit_function_declaration == 1)
    warning ("implicit declaration of function `%s'", name);
}

/* Return zero if the declaration NEWDECL is valid
   when the declaration OLDDECL (assumed to be for the same name)
   has already been seen.
   Otherwise return 1 if NEWDECL is a redefinition, 2 if it is a redeclaration,
   and 3 if it is a conflicting declaration.  */

static int
redeclaration_error_message (newdecl, olddecl)
     tree newdecl, olddecl;
{
  if (TREE_CODE (newdecl) == TYPE_DECL)
    {
      /* Do not complain about type redeclarations where at least one
	 declaration was in a system header.  */
      if (DECL_IN_SYSTEM_HEADER (olddecl) || DECL_IN_SYSTEM_HEADER (newdecl))
	return 0;
      return 1;
    }
  else if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* Declarations of functions can insist on internal linkage
	 but they can't be inconsistent with internal linkage,
	 so there can be no error on that account.
	 However defining the same name twice is no good.  */
      if (DECL_INITIAL (olddecl) != 0 && DECL_INITIAL (newdecl) != 0
	  /* However, defining once as extern inline and a second
	     time in another way is ok.  */
	  && ! (DECL_DECLARED_INLINE_P (olddecl) && DECL_EXTERNAL (olddecl)
	       && ! (DECL_DECLARED_INLINE_P (newdecl)
		     && DECL_EXTERNAL (newdecl))))
	return 1;
      return 0;
    }
  else if (DECL_CONTEXT (newdecl) == NULL_TREE)
    {
      /* Objects declared at top level:  */
      /* If at least one is a reference, it's ok.  */
      if (DECL_EXTERNAL (newdecl) || DECL_EXTERNAL (olddecl))
	return 0;
      /* Reject two definitions.  */
      if (DECL_INITIAL (olddecl) != 0 && DECL_INITIAL (newdecl) != 0)
	return 1;
      /* Now we have two tentative defs, or one tentative and one real def.  */
      /* Insist that the linkage match.  */
      if (TREE_PUBLIC (olddecl) != TREE_PUBLIC (newdecl))
	return 3;
      return 0;
    }
  else if (current_binding_level->parm_flag
	   && TREE_ASM_WRITTEN (olddecl) && !TREE_ASM_WRITTEN (newdecl))
    return 0;
  else
    {
      /* Newdecl has block scope.  If olddecl has block scope also, then
	 reject two definitions, and reject a definition together with an
	 external reference.  Otherwise, it is OK, because newdecl must
	 be an extern reference to olddecl.  */
      if (!(DECL_EXTERNAL (newdecl) && DECL_EXTERNAL (olddecl))
	  && DECL_CONTEXT (newdecl) == DECL_CONTEXT (olddecl))
	return 2;
      return 0;
    }
}

/* Get the LABEL_DECL corresponding to identifier ID as a label.
   Create one if none exists so far for the current function.
   This function is called for both label definitions and label references.  */

tree
lookup_label (id)
     tree id;
{
  tree decl = IDENTIFIER_LABEL_VALUE (id);

  if (current_function_decl == 0)
    {
      error ("label %s referenced outside of any function",
	     IDENTIFIER_POINTER (id));
      return 0;
    }

  /* Use a label already defined or ref'd with this name.  */
  if (decl != 0)
    {
      /* But not if it is inherited and wasn't declared to be inheritable.  */
      if (DECL_CONTEXT (decl) != current_function_decl
	  && ! C_DECLARED_LABEL_FLAG (decl))
	return shadow_label (id);
      return decl;
    }

  decl = build_decl (LABEL_DECL, id, void_type_node);

  /* A label not explicitly declared must be local to where it's ref'd.  */
  DECL_CONTEXT (decl) = current_function_decl;

  DECL_MODE (decl) = VOIDmode;

  /* Say where one reference is to the label,
     for the sake of the error if it is not defined.  */
  DECL_SOURCE_LINE (decl) = lineno;
  DECL_SOURCE_FILE (decl) = input_filename;

  IDENTIFIER_LABEL_VALUE (id) = decl;

  named_labels = tree_cons (NULL_TREE, decl, named_labels);

  return decl;
}

/* Make a label named NAME in the current function,
   shadowing silently any that may be inherited from containing functions
   or containing scopes.

   Note that valid use, if the label being shadowed
   comes from another scope in the same function,
   requires calling declare_nonlocal_label right away.  */

tree
shadow_label (name)
     tree name;
{
  tree decl = IDENTIFIER_LABEL_VALUE (name);

  if (decl != 0)
    {
      tree dup;

      /* Check to make sure that the label hasn't already been declared
	 at this label scope */
      for (dup = named_labels; dup; dup = TREE_CHAIN (dup))
	if (TREE_VALUE (dup) == decl)
	  {
	    error ("duplicate label declaration `%s'",
		   IDENTIFIER_POINTER (name));
	    error_with_decl (TREE_VALUE (dup),
			     "this is a previous declaration");
	    /* Just use the previous declaration.  */
	    return lookup_label (name);
	  }

      shadowed_labels = tree_cons (NULL_TREE, decl, shadowed_labels);
      IDENTIFIER_LABEL_VALUE (name) = decl = 0;
    }

  return lookup_label (name);
}

/* Define a label, specifying the location in the source file.
   Return the LABEL_DECL node for the label, if the definition is valid.
   Otherwise return 0.  */

tree
define_label (filename, line, name)
     const char *filename;
     int line;
     tree name;
{
  tree decl = lookup_label (name);

  /* If label with this name is known from an outer context, shadow it.  */
  if (decl != 0 && DECL_CONTEXT (decl) != current_function_decl)
    {
      shadowed_labels = tree_cons (NULL_TREE, decl, shadowed_labels);
      IDENTIFIER_LABEL_VALUE (name) = 0;
      decl = lookup_label (name);
    }

  if (warn_traditional && !in_system_header && lookup_name (name))
    warning_with_file_and_line (filename, line,
				"traditional C lacks a separate namespace for labels, identifier `%s' conflicts",
				IDENTIFIER_POINTER (name));

  if (DECL_INITIAL (decl) != 0)
    {
      error_with_file_and_line (filename, line, "duplicate label `%s'",
				IDENTIFIER_POINTER (name));
      return 0;
    }
  else
    {
      /* Mark label as having been defined.  */
      DECL_INITIAL (decl) = error_mark_node;
      /* Say where in the source.  */
      DECL_SOURCE_FILE (decl) = filename;
      DECL_SOURCE_LINE (decl) = line;
      return decl;
    }
}

/* Return the list of declarations of the current level.
   Note that this list is in reverse order unless/until
   you nreverse it; and when you do nreverse it, you must
   store the result back using `storedecls' or you will lose.  */

tree
getdecls ()
{
  return current_binding_level->names;
}

/* Return the list of type-tags (for structs, etc) of the current level.  */

tree
gettags ()
{
  return current_binding_level->tags;
}

/* Store the list of declarations of the current level.
   This is done for the parameter declarations of a function being defined,
   after they are modified in the light of any missing parameters.  */

static void
storedecls (decls)
     tree decls;
{
  current_binding_level->names = decls;
}

/* Similarly, store the list of tags of the current level.  */

static void
storetags (tags)
     tree tags;
{
  current_binding_level->tags = tags;
}

/* Given NAME, an IDENTIFIER_NODE,
   return the structure (or union or enum) definition for that name.
   Searches binding levels from BINDING_LEVEL up to the global level.
   If THISLEVEL_ONLY is nonzero, searches only the specified context
   (but skips any tag-transparent contexts to find one that is
   meaningful for tags).
   CODE says which kind of type the caller wants;
   it is RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE.
   If the wrong kind of type is found, an error is reported.  */

static tree
lookup_tag (code, name, binding_level, thislevel_only)
     enum tree_code code;
     struct binding_level *binding_level;
     tree name;
     int thislevel_only;
{
  struct binding_level *level;
  int thislevel = 1;

  for (level = binding_level; level; level = level->level_chain)
    {
      tree tail;
      for (tail = level->tags; tail; tail = TREE_CHAIN (tail))
	{
	  if (TREE_PURPOSE (tail) == name)
	    {
	      if (TREE_CODE (TREE_VALUE (tail)) != code)
		{
		  /* Definition isn't the kind we were looking for.  */
		  pending_invalid_xref = name;
		  pending_invalid_xref_file = input_filename;
		  pending_invalid_xref_line = lineno;
		  /* If in the same binding level as a declaration as a tag
		     of a different type, this must not be allowed to
		     shadow that tag, so give the error immediately.
		     (For example, "struct foo; union foo;" is invalid.)  */
		  if (thislevel)
		    pending_xref_error ();
		}
	      return TREE_VALUE (tail);
	    }
	}
      if (! level->tag_transparent)
	{
	  if (thislevel_only)
	    return NULL_TREE;
	  thislevel = 0;
	}
    }
  return NULL_TREE;
}

/* Print an error message now
   for a recent invalid struct, union or enum cross reference.
   We don't print them immediately because they are not invalid
   when used in the `struct foo;' construct for shadowing.  */

void
pending_xref_error ()
{
  if (pending_invalid_xref != 0)
    error_with_file_and_line (pending_invalid_xref_file,
			      pending_invalid_xref_line,
			      "`%s' defined as wrong kind of tag",
			      IDENTIFIER_POINTER (pending_invalid_xref));
  pending_invalid_xref = 0;
}

/* Given a type, find the tag that was defined for it and return the tag name.
   Otherwise return 0.  */

static tree
lookup_tag_reverse (type)
     tree type;
{
  struct binding_level *level;

  for (level = current_binding_level; level; level = level->level_chain)
    {
      tree tail;
      for (tail = level->tags; tail; tail = TREE_CHAIN (tail))
	{
	  if (TREE_VALUE (tail) == type)
	    return TREE_PURPOSE (tail);
	}
    }
  return NULL_TREE;
}

/* Look up NAME in the current binding level and its superiors
   in the namespace of variables, functions and typedefs.
   Return a ..._DECL node of some kind representing its definition,
   or return 0 if it is undefined.  */

tree
lookup_name (name)
     tree name;
{
  tree val;

  if (current_binding_level != global_binding_level
      && IDENTIFIER_LOCAL_VALUE (name))
    val = IDENTIFIER_LOCAL_VALUE (name);
  else
    val = IDENTIFIER_GLOBAL_VALUE (name);
  return val;
}

/* Similar to `lookup_name' but look only at current binding level.  */

tree
lookup_name_current_level (name)
     tree name;
{
  tree t;

  if (current_binding_level == global_binding_level)
    return IDENTIFIER_GLOBAL_VALUE (name);

  if (IDENTIFIER_LOCAL_VALUE (name) == 0)
    return 0;

  for (t = current_binding_level->names; t; t = TREE_CHAIN (t))
    if (DECL_NAME (t) == name)
      break;

  return t;
}

/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

void
c_init_decl_processing ()
{
  tree endlink;
  tree ptr_ftype_void, ptr_ftype_ptr;

  /* Adds some ggc roots, and reserved words for c-parse.in.  */
  c_parse_init ();

  current_function_decl = NULL;
  named_labels = NULL;
  current_binding_level = NULL_BINDING_LEVEL;
  free_binding_level = NULL_BINDING_LEVEL;

  /* Make the binding_level structure for global names.  */
  pushlevel (0);
  global_binding_level = current_binding_level;

  build_common_tree_nodes (flag_signed_char);

  c_common_nodes_and_builtins ();

  boolean_type_node = integer_type_node;
  boolean_true_node = integer_one_node;
  boolean_false_node = integer_zero_node;

  c_bool_type_node = make_unsigned_type (BOOL_TYPE_SIZE);
  TREE_SET_CODE (c_bool_type_node, BOOLEAN_TYPE);
  TYPE_MAX_VALUE (c_bool_type_node) = build_int_2 (1, 0);
  TREE_TYPE (TYPE_MAX_VALUE (c_bool_type_node)) = c_bool_type_node;
  TYPE_PRECISION (c_bool_type_node) = 1;
  pushdecl (build_decl (TYPE_DECL, get_identifier ("_Bool"),
			c_bool_type_node));
  c_bool_false_node = build_int_2 (0, 0);
  TREE_TYPE (c_bool_false_node) = c_bool_type_node;
  c_bool_true_node = build_int_2 (1, 0);
  TREE_TYPE (c_bool_true_node) = c_bool_type_node;

  endlink = void_list_node;
  ptr_ftype_void = build_function_type (ptr_type_node, endlink);
  ptr_ftype_ptr
    = build_function_type (ptr_type_node,
			   tree_cons (NULL_TREE, ptr_type_node, endlink));

  pedantic_lvalues = pedantic;

  make_fname_decl = c_make_fname_decl;
  start_fname_decls ();
}

/* Create the VAR_DECL for __FUNCTION__ etc. ID is the name to give the
   decl, NAME is the initialization string and TYPE_DEP indicates whether
   NAME depended on the type of the function.  As we don't yet implement
   delayed emission of static data, we mark the decl as emitted
   so it is not placed in the output.  Anything using it must therefore pull
   out the STRING_CST initializer directly.  This does mean that these names
   are string merging candidates, which is wrong for C99's __func__.  FIXME.  */

static tree
c_make_fname_decl (id, type_dep)
     tree id;
     int type_dep;
{
  const char *name = fname_as_string (type_dep);
  tree decl, type, init;
  size_t length = strlen (name);

  type =  build_array_type
          (build_qualified_type (char_type_node, TYPE_QUAL_CONST),
	   build_index_type (size_int (length)));

  decl = build_decl (VAR_DECL, id, type);
  /* We don't push the decl, so have to set its context here.  */
  DECL_CONTEXT (decl) = current_function_decl;
  
  TREE_STATIC (decl) = 1;
  TREE_READONLY (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  
  init = build_string (length + 1, name);
  TREE_TYPE (init) = type;
  DECL_INITIAL (decl) = init;

  TREE_USED (decl) = 1;
  
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
builtin_function (name, type, function_code, class, library_name, attrs)
     const char *name;
     tree type;
     int function_code;
     enum built_in_class class;
     const char *library_name;
     tree attrs;
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
    C_DECL_ANTICIPATED (decl) = 1;

  /* Possibly apply some default attributes to this built-in function.  */
  if (attrs)
    decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
  else
    decl_attributes (&decl, NULL_TREE, 0);

  return decl;
}

/* Apply default attributes to a function, if a system function with default
   attributes.  */

void
c_insert_default_attributes (decl)
     tree decl;
{
  if (!TREE_PUBLIC (decl))
    return;
  c_common_insert_default_attributes (decl);
}

/* Called when a declaration is seen that contains no names to declare.
   If its type is a reference to a structure, union or enum inherited
   from a containing scope, shadow that tag name for the current scope
   with a forward reference.
   If its type defines a new named structure or union
   or defines an enum, it is valid but we need not do anything here.
   Otherwise, it is an error.  */

void
shadow_tag (declspecs)
     tree declspecs;
{
  shadow_tag_warned (declspecs, 0);
}

void
shadow_tag_warned (declspecs, warned)
     tree declspecs;
     int warned;
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
	  tree name = lookup_tag_reverse (value);
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
	      t = lookup_tag (code, name, current_binding_level, 1);

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
build_array_declarator (expr, quals, static_p, vla_unspec_p)
     tree expr;
     tree quals;
     int static_p;
     int vla_unspec_p;
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
set_array_declarator_type (decl, type, abstract_p)
     tree decl;
     tree type;
     int abstract_p;
{
  TREE_OPERAND (decl, 0) = type;
  if (abstract_p && (TREE_TYPE (decl) != NULL_TREE || TREE_STATIC (decl)))
    error ("static or type qualifiers in abstract declarator");
  return decl;
}

/* Decode a "typename", such as "int **", returning a ..._TYPE node.  */

tree
groktypename (typename)
     tree typename;
{
  tree specs, attrs;

  if (TREE_CODE (typename) != TREE_LIST)
    return typename;

  split_specs_attrs (TREE_PURPOSE (typename), &specs, &attrs);

  typename = grokdeclarator (TREE_VALUE (typename), specs, TYPENAME, 0);

  /* Apply attributes.  */
  decl_attributes (&typename, attrs, 0);

  return typename;
}

/* Return a PARM_DECL node for a given pair of specs and declarator.  */

tree
groktypename_in_parm_context (typename)
     tree typename;
{
  if (TREE_CODE (typename) != TREE_LIST)
    return typename;
  return grokdeclarator (TREE_VALUE (typename),
			 TREE_PURPOSE (typename),
			 PARM, 0);
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
start_decl (declarator, declspecs, initialized, attributes)
     tree declarator, declspecs;
     int initialized;
     tree attributes;
{
  tree decl;
  tree tem;
  
  /* An object declared as __attribute__((deprecated)) suppresses
     warnings of uses of other deprecated items.  */
  if (lookup_attribute ("deprecated", attributes))
    deprecated_state = DEPRECATED_SUPPRESS;

  decl = grokdeclarator (declarator, declspecs,
			 NORMAL, initialized);
  
  deprecated_state = DEPRECATED_NORMAL;

  if (warn_main > 0 && TREE_CODE (decl) != FUNCTION_DECL
      && MAIN_NAME_P (DECL_NAME (decl)))
    warning_with_decl (decl, "`%s' is usually a function");

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
#if 0
      /* Seems redundant with grokdeclarator.  */
      if (current_binding_level != global_binding_level
	  && DECL_EXTERNAL (decl)
	  && TREE_CODE (decl) != FUNCTION_DECL)
	warning ("declaration of `%s' has `extern' and is initialized",
		 IDENTIFIER_POINTER (DECL_NAME (decl)));
#endif
      DECL_EXTERNAL (decl) = 0;
      if (current_binding_level == global_binding_level)
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
     in the presense of dynamic linking.  */

  if (TREE_CODE (decl) == VAR_DECL
      && !initialized
      && TREE_PUBLIC (decl)
      && !DECL_THREAD_LOCAL (decl)
      && !flag_no_common)
    DECL_COMMON (decl) = 1;

  /* Set attributes here so if duplicate decl, will have proper attributes.  */
  decl_attributes (&decl, attributes, 0);

  /* If #pragma weak was used, mark the decl weak now.  */
  if (current_binding_level == global_binding_level)
    maybe_apply_pragma_weak (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_DECLARED_INLINE_P (decl)
      && DECL_UNINLINABLE (decl)
      && lookup_attribute ("noinline", DECL_ATTRIBUTES (decl)))
    warning_with_decl (decl,
		       "inline function `%s' given attribute noinline");

  /* Add this decl to the current binding level.
     TEM may equal DECL or it may be a previous decl of the same name.  */
  tem = pushdecl (decl);

  /* For a local variable, define the RTL now.  */
  if (current_binding_level != global_binding_level
      /* But not if this is a duplicate decl
	 and we preserved the rtl from the previous one
	 (which may or may not happen).  */
      && !DECL_RTL_SET_P (tem)
      && !DECL_CONTEXT (tem))
    {
      if (TREE_TYPE (tem) != error_mark_node
	  && COMPLETE_TYPE_P (TREE_TYPE (tem)))
	expand_decl (tem);
      else if (TREE_CODE (TREE_TYPE (tem)) == ARRAY_TYPE
	       && DECL_INITIAL (tem) != 0)
	expand_decl (tem);
    }

  return tem;
}

/* Finish processing of a declaration;
   install its initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.  */

void
finish_decl (decl, init, asmspec_tree)
     tree decl, init;
     tree asmspec_tree;
{
  tree type = TREE_TYPE (decl);
  int was_incomplete = (DECL_SIZE (decl) == 0);
  const char *asmspec = 0;

  /* If a name was specified, get the string.  */
  if (current_binding_level == global_binding_level)
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

  /* Deduce size of array from initialization, if not already known */
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
	error_with_decl (decl, "initializer fails to determine size of `%s'");

      else if (failure == 2)
	{
	  if (do_default)
	    error_with_decl (decl, "array size missing in `%s'");
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
	error_with_decl (decl, "zero or negative size array `%s'");

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
		 || DECL_CONTEXT (decl) != 0)
	      :
		/* An automatic variable with an incomplete type
		   is an error.  */
		!DECL_EXTERNAL (decl)))
	{
	  error_with_decl (decl, "storage size of `%s' isn't known");
	  TREE_TYPE (decl) = error_mark_node;
	}

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
	  && DECL_SIZE (decl) != 0)
	{
	  if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
	    constant_expression_warning (DECL_SIZE (decl));
	  else
	    error_with_decl (decl, "storage size of `%s' isn't constant");
	}

      if (TREE_USED (type))
	TREE_USED (decl) = 1;
    }

  /* If this is a function and an assembler name is specified, it isn't
     builtin any more.  Also reset DECL_RTL so we can give it its new
     name.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && asmspec)
    {
      DECL_BUILT_IN_CLASS (decl) = NOT_BUILT_IN;
      SET_DECL_RTL (decl, NULL_RTX);
      SET_DECL_ASSEMBLER_NAME (decl, get_identifier (asmspec));
    }

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* This is a no-op in c-lang.c or something real in objc-act.c.  */
      if (flag_objc)
	objc_check_decl (decl);

      if (!DECL_CONTEXT (decl))
	{
	  if (DECL_INITIAL (decl) == NULL_TREE
	      || DECL_INITIAL (decl) == error_mark_node)
	    /* Don't output anything
	       when a tentative file-scope definition is seen.
	       But at end of compilation, do output code for them.  */
	    DECL_DEFER_OUTPUT (decl) = 1;
	  rest_of_decl_compilation (decl, asmspec,
				    (DECL_CONTEXT (decl) == 0
				     || TREE_ASM_WRITTEN (decl)), 0);
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
		warning_with_decl (decl,
				   "ignoring asm-specifier for non-static local variable `%s'");
	      else
		SET_DECL_ASSEMBLER_NAME (decl, get_identifier (asmspec));
	    }

	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    add_decl_stmt (decl);
	}

      if (DECL_CONTEXT (decl) != 0)
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

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* This is a no-op in c-lang.c or something real in objc-act.c.  */
      if (flag_objc)
	objc_check_decl (decl);
      rest_of_decl_compilation (decl, NULL, DECL_CONTEXT (decl) == 0, 0);
    }

  /* At the end of a declaration, throw away any variable type sizes
     of types defined inside that declaration.  There is no use
     computing them in the following function definition.  */
  if (current_binding_level == global_binding_level)
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

/* Given a parsed parameter declaration,
   decode it into a PARM_DECL and push that on the current binding level.
   Also, for the sake of forward parm decls,
   record the given order of parms in `parm_order'.  */

void
push_parm_decl (parm)
     tree parm;
{
  tree decl;
  int old_immediate_size_expand = immediate_size_expand;
  /* Don't try computing parm sizes now -- wait till fn is called.  */
  immediate_size_expand = 0;

  decl = grokdeclarator (TREE_VALUE (TREE_PURPOSE (parm)),
			 TREE_PURPOSE (TREE_PURPOSE (parm)), PARM, 0);
  decl_attributes (&decl, TREE_VALUE (parm), 0);

#if 0
  if (DECL_NAME (decl))
    {
      tree olddecl;
      olddecl = lookup_name (DECL_NAME (decl));
      if (pedantic && olddecl != 0 && TREE_CODE (olddecl) == TYPE_DECL)
	pedwarn_with_decl (decl,
			   "ISO C forbids parameter `%s' shadowing typedef");
    }
#endif

  decl = pushdecl (decl);

  immediate_size_expand = old_immediate_size_expand;

  current_binding_level->parm_order
    = tree_cons (NULL_TREE, decl, current_binding_level->parm_order);

  /* Add this decl to the current binding level.  */
  finish_decl (decl, NULL_TREE, NULL_TREE);
}

/* Clear the given order of parms in `parm_order'.
   Used at start of parm list,
   and also at semicolon terminating forward decls.  */

void
clear_parm_order ()
{
  current_binding_level->parm_order = NULL_TREE;
}

/* Build a COMPOUND_LITERAL_EXPR.  TYPE is the type given in the compound
   literal, which may be an incomplete array type completed by the
   initializer; INIT is a CONSTRUCTOR that initializes the compound
   literal.  */

tree
build_compound_literal (type, init)
     tree type;
     tree init;
{
  /* We do not use start_decl here because we have a type, not a declarator;
     and do not use finish_decl because the decl should be stored inside
     the COMPOUND_LITERAL_EXPR rather than added elsewhere as a DECL_STMT.  */
  tree decl = build_decl (VAR_DECL, NULL_TREE, type);
  tree complit;
  tree stmt;
  DECL_EXTERNAL (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  TREE_STATIC (decl) = (current_binding_level == global_binding_level);
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
      extern int var_labelno;

      ASM_FORMAT_PRIVATE_NAME (name, "__compound_literal", var_labelno);
      var_labelno++;
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
complete_array_type (type, initial_value, do_default)
     tree type;
     tree initial_value;
     int do_default;
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
flexible_array_type_p (type)
     tree type;
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
     BITFIELD for a field with specified width.
   INITIALIZED is 1 if the decl has an initializer.

   In the TYPENAME case, DECLARATOR is really an absolute declarator.
   It may also be so in the PARM case, for a prototype where the
   argument type is specified but not the name.

   This function is where the complicated C meanings of `static'
   and `extern' are interpreted.  */

static tree
grokdeclarator (declarator, declspecs, decl_context, initialized)
     tree declspecs;
     tree declarator;
     enum decl_context decl_context;
     int initialized;
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
  const char *name;
  tree typedef_type = 0;
  int funcdef_flag = 0;
  enum tree_code innermost_code = ERROR_MARK;
  int bitfield = 0;
  int size_varies = 0;
  tree decl_attr = NULL_TREE;
  tree array_ptr_quals = NULL_TREE;
  int array_parm_static = 0;
  tree returned_attrs = NULL_TREE;

  if (decl_context == BITFIELD)
    bitfield = 1, decl_context = FIELD;

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
    if (name == 0)
      name = "type name";
  }

  /* A function definition's declarator must have the form of
     a function declarator.  */

  if (funcdef_flag && innermost_code != CALL_EXPR)
    return 0;

  /* Anything declared one level down from the top level
     must be one of the parameters of a function
     (because the body is at least two levels down).  */

  /* If this looks like a function definition, make it one,
     even if it occurs where parms are expected.
     Then store_parm_decls will reject it and not use it as a parm.  */
  if (decl_context == NORMAL && !funcdef_flag
      && current_binding_level->parm_flag)
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
		      if (!flag_isoc99)
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
     Optionally treat bitfields as signed by default.  */
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
     declaration contains the `const'.  */
  constp = !! (specbits & 1 << (int) RID_CONST) + TYPE_READONLY (type);
  restrictp = !! (specbits & 1 << (int) RID_RESTRICT) + TYPE_RESTRICT (type);
  volatilep = !! (specbits & 1 << (int) RID_VOLATILE) + TYPE_VOLATILE (type);
  inlinep = !! (specbits & (1 << (int) RID_INLINE));
  if (constp > 1 && ! flag_isoc99)
    pedwarn ("duplicate `const'");
  if (restrictp > 1 && ! flag_isoc99)
    pedwarn ("duplicate `restrict'");
  if (volatilep > 1 && ! flag_isoc99)
    pedwarn ("duplicate `volatile'");
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
	    && (pedantic || current_binding_level == global_binding_level))
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
	/* `extern' with initialization is invalid if not at top level.  */
	if (current_binding_level == global_binding_level)
	  warning ("`%s' initialized and declared `extern'", name);
	else
	  error ("`%s' has both `extern' and initializer", name);
      }
    else if (current_binding_level == global_binding_level)
      {
	if (specbits & 1 << (int) RID_AUTO)
	  error ("top-level declaration of `%s' specifies `auto'", name);
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

	  if (pedantic && flexible_array_type_p (type))
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

#if 0
	  /* We shouldn't have a function type here at all!
	     Functions aren't allowed as array elements.  */
	  if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	      && (constp || volatilep))
	    pedwarn ("ISO C forbids const or volatile function types");
#endif

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
	  tree arg_types;

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
				 funcdef_flag
				 /* Say it's a definition
				    only for the CALL_EXPR
				    closest to the identifier.  */
				 && TREE_CODE (TREE_OPERAND (declarator, 0)) == IDENTIFIER_NODE);
	  /* Type qualifiers before the return type of the function
	     qualify the return type, not the function type.  */
	  if (type_quals)
	    {
	      /* Type qualifiers on a function return type are normally
		 permitted by the standard but have no effect, so give a
		 warning at -W.  Qualifiers on a void return type have
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
	      if (constp > 1 && ! flag_isoc99)
		pedwarn ("duplicate `const'");
	      if (volatilep > 1 && ! flag_isoc99)
		pedwarn ("duplicate `volatile'");
	      if (restrictp > 1 && ! flag_isoc99)
		pedwarn ("duplicate `restrict'");

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
		|| (current_binding_level == global_binding_level
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
	  {
	    type = build_array_type (c_build_qualified_type (TREE_TYPE (type),
							     type_quals),
				     TYPE_DOMAIN (type));
#if 0
	    /* Leave the field const or volatile as well.  */
	    type_quals = TYPE_UNQUALIFIED;
#endif
	  }
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
			  || current_binding_level == global_binding_level);

	if (specbits & (1 << (int) RID_AUTO)
	    && (pedantic || current_binding_level == global_binding_level))
	  pedwarn ("invalid storage class for function `%s'", name);
	if (specbits & (1 << (int) RID_REGISTER))
	  error ("invalid storage class for function `%s'", name);
	if (specbits & (1 << (int) RID_THREAD))
	  error ("invalid storage class for function `%s'", name);
	/* Function declaration not at top level.
	   Storage classes other than `extern' are not allowed
	   and `extern' makes no difference.  */
	if (current_binding_level != global_binding_level
	    && (specbits & ((1 << (int) RID_STATIC) | (1 << (int) RID_INLINE)))
	    && pedantic)
	  pedwarn ("invalid storage class for function `%s'", name);

	decl = build_decl (FUNCTION_DECL, declarator, type);
	decl = build_decl_attribute_variant (decl, decl_attr);

	DECL_LANG_SPECIFIC (decl) = (struct lang_decl *)
	  ggc_alloc_cleared (sizeof (struct lang_decl));

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
	    /* Assume that otherwise the function can be inlined.  */
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
	  {
	    if (!DECL_INLINE (decl))
		DID_INLINE_FUNC (decl) = 1;
	    DECL_INLINE (decl) = 1;
	    DECL_DECLARED_INLINE_P (decl) = 0;
	  }
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
#if 0 /* Leave the variable const or volatile as well.  */
	    type_quals = TYPE_UNQUALIFIED;
#endif
	  }
	else if (type_quals)
	  type = c_build_qualified_type (type, type_quals);
	  
	decl = build_decl (VAR_DECL, declarator, type);
	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;

	if (inlinep)
	  pedwarn_with_decl (decl, "variable `%s' declared `inline'");

	DECL_EXTERNAL (decl) = extern_ref;

	/* At top level, the presence of a `static' or `register' storage
	   class specifier, or the absence of all storage class specifiers
	   makes this declaration a definition (perhaps tentative).  Also,
	   the absence of both `static' and `register' makes it public.  */
	if (current_binding_level == global_binding_level)
	  {
	    TREE_PUBLIC (decl) = !(specbits & ((1 << (int) RID_STATIC)
					       | (1 << (int) RID_REGISTER)));
	    TREE_STATIC (decl) = !extern_ref;
	  }
	/* Not at top level, only `static' makes a static definition.  */
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
grokparms (parms_info, funcdef_flag)
     tree parms_info;
     int funcdef_flag;
{
  tree first_parm = TREE_CHAIN (parms_info);

  last_function_parms = TREE_PURPOSE (parms_info);
  last_function_parm_tags = TREE_VALUE (parms_info);

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
      /* We no longer test FUNCDEF_FLAG.
	 If the arg types are incomplete in a declaration,
	 they must include undefined tags.
	 These tags can never be defined in the scope of the declaration,
	 so the types can never be completed,
	 and no call can be compiled successfully.  */
#if 0
      /* In a fcn definition, arg types must be complete.  */
      if (funcdef_flag)
#endif
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
#if 0
	      /* This has been replaced by parm_tags_warning, which
		 uses a more accurate criterion for what to warn
		 about.  */
	      else
		{
		  /* Now warn if is a pointer to an incomplete type.  */
		  while (TREE_CODE (type) == POINTER_TYPE
			 || TREE_CODE (type) == REFERENCE_TYPE)
		    type = TREE_TYPE (type);
		  type = TYPE_MAIN_VARIANT (type);
		  if (!COMPLETE_TYPE_P (type))
		    {
		      if (DECL_NAME (parm) != 0)
			warning ("parameter `%s' points to incomplete type",
				 IDENTIFIER_POINTER (DECL_NAME (parm)));
		      else
			warning ("parameter points to incomplete type");
		    }
		}
#endif
	      typelt = TREE_CHAIN (typelt);
	    }

      return first_parm;
    }
}

/* Return a tree_list node with info on a parameter list just parsed.
   The TREE_PURPOSE is a chain of decls of those parms.
   The TREE_VALUE is a list of structure, union and enum tags defined.
   The TREE_CHAIN is a list of argument types to go in the FUNCTION_TYPE.
   This tree_list node is later fed to `grokparms'.

   VOID_AT_END nonzero means append `void' to the end of the type-list.
   Zero means the parmlist ended with an ellipsis so don't append `void'.  */

tree
get_parm_info (void_at_end)
     int void_at_end;
{
  tree decl, t;
  tree types = 0;
  int erred = 0;
  tree tags = gettags ();
  tree parms = getdecls ();
  tree new_parms = 0;
  tree order = current_binding_level->parm_order;

  /* Just `void' (and no ellipsis) is special.  There are really no parms.
     But if the `void' is qualified (by `const' or `volatile') or has a
     storage class specifier (`register'), then the behavior is undefined;
     by not counting it as the special case of `void' we will cause an
     error later.  Typedefs for `void' are OK (see DR#157).  */
  if (void_at_end && parms != 0
      && TREE_CHAIN (parms) == 0
      && VOID_TYPE_P (TREE_TYPE (parms))
      && ! TREE_THIS_VOLATILE (parms)
      && ! TREE_READONLY (parms)
      && ! DECL_REGISTER (parms)
      && DECL_NAME (parms) == 0)
    {
      parms = NULL_TREE;
      storedecls (NULL_TREE);
      return tree_cons (NULL_TREE, NULL_TREE,
			tree_cons (NULL_TREE, void_type_node, NULL_TREE));
    }

  /* Extract enumerator values and other non-parms declared with the parms.
     Likewise any forward parm decls that didn't have real parm decls.  */
  for (decl = parms; decl;)
    {
      tree next = TREE_CHAIN (decl);

      if (TREE_CODE (decl) != PARM_DECL)
	{
	  TREE_CHAIN (decl) = new_parms;
	  new_parms = decl;
	}
      else if (TREE_ASM_WRITTEN (decl))
	{
	  error_with_decl (decl,
			   "parameter `%s' has just a forward declaration");
	  TREE_CHAIN (decl) = new_parms;
	  new_parms = decl;
	}
      decl = next;
    }

  /* Put the parm decls back in the order they were in in the parm list.  */
  for (t = order; t; t = TREE_CHAIN (t))
    {
      if (TREE_CHAIN (t))
	TREE_CHAIN (TREE_VALUE (t)) = TREE_VALUE (TREE_CHAIN (t));
      else
	TREE_CHAIN (TREE_VALUE (t)) = 0;
    }

  new_parms = chainon (order ? nreverse (TREE_VALUE (order)) : 0,
		       new_parms);

  /* Store the parmlist in the binding level since the old one
     is no longer a valid list.  (We have changed the chain pointers.)  */
  storedecls (new_parms);

  for (decl = new_parms; decl; decl = TREE_CHAIN (decl))
    /* There may also be declarations for enumerators if an enumeration
       type is declared among the parms.  Ignore them here.  */
    if (TREE_CODE (decl) == PARM_DECL)
      {
	/* Since there is a prototype,
	   args are passed in their declared types.  */
	tree type = TREE_TYPE (decl);
	DECL_ARG_TYPE (decl) = type;
	if (PROMOTE_PROTOTYPES
	    && INTEGRAL_TYPE_P (type)
	    && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	  DECL_ARG_TYPE (decl) = integer_type_node;

	types = tree_cons (NULL_TREE, TREE_TYPE (decl), types);
	if (VOID_TYPE_P (TREE_VALUE (types)) && ! erred
	    && DECL_NAME (decl) == 0)
	  {
	    error ("`void' in parameter list must be the entire list");
	    erred = 1;
	  }
      }

  if (void_at_end)
    return tree_cons (new_parms, tags,
		      nreverse (tree_cons (NULL_TREE, void_type_node, types)));

  return tree_cons (new_parms, tags, nreverse (types));
}

/* At end of parameter list, warn about any struct, union or enum tags
   defined within.  Do so because these types cannot ever become complete.  */

void
parmlist_tags_warning ()
{
  tree elt;
  static int already;

  for (elt = current_binding_level->tags; elt; elt = TREE_CHAIN (elt))
    {
      enum tree_code code = TREE_CODE (TREE_VALUE (elt));
      /* An anonymous union parm type is meaningful as a GNU extension.
	 So don't warn for that.  */
      if (code == UNION_TYPE && TREE_PURPOSE (elt) == 0 && !pedantic)
	continue;
      if (TREE_PURPOSE (elt) != 0)
        {
          if (code == RECORD_TYPE)
            warning ("`struct %s' declared inside parameter list",
                     IDENTIFIER_POINTER (TREE_PURPOSE (elt)));
          else if (code == UNION_TYPE)
            warning ("`union %s' declared inside parameter list",
                     IDENTIFIER_POINTER (TREE_PURPOSE (elt)));
          else
            warning ("`enum %s' declared inside parameter list",
                     IDENTIFIER_POINTER (TREE_PURPOSE (elt)));
        }
      else
	{
	  /* For translation these need to be separate warnings */
	  if (code == RECORD_TYPE)
	    warning ("anonymous struct declared inside parameter list");
	  else if (code == UNION_TYPE)
	    warning ("anonymous union declared inside parameter list");
	  else
	    warning ("anonymous enum declared inside parameter list");
	}
      if (! already)
	{
	  warning ("its scope is only this definition or declaration, which is probably not what you want");
	  already = 1;
	}
    }
}

/* Get the struct, enum or union (CODE says which) with tag NAME.
   Define the tag as a forward-reference if it is not defined.  */

tree
xref_tag (code, name)
     enum tree_code code;
     tree name;
{
  /* If a cross reference is requested, look up the type
     already defined for this tag and return it.  */

  tree ref = lookup_tag (code, name, current_binding_level, 0);
  /* If this is the right type of tag, return what we found.
     (This reference will be shadowed by shadow_tag later if appropriate.)
     If this is the wrong type of tag, do not return it.  If it was the
     wrong type in the same binding level, we will have had an error
     message already; if in a different binding level and declaring
     a name, pending_xref_error will give an error message; but if in a
     different binding level and not declaring a name, this tag should
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

/* Make sure that the tag NAME is defined *in the current binding level*
   at least as a forward reference.
   CODE says which kind of tag NAME ought to be.  */

tree
start_struct (code, name)
     enum tree_code code;
     tree name;
{
  /* If there is already a tag defined at this binding level
     (as a forward reference), just return it.  */

  tree ref = 0;

  if (name != 0)
    ref = lookup_tag (code, name, current_binding_level, 1);
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
   WIDTH is non-NULL for bit fields only, and is an INTEGER_CST node.

   This is done during the parsing of the struct declaration.
   The FIELD_DECL nodes are chained together and the lot of them
   are ultimately passed to `build_struct' to make the RECORD_TYPE node.  */

tree
grokfield (filename, line, declarator, declspecs, width)
     const char *filename ATTRIBUTE_UNUSED;
     int line ATTRIBUTE_UNUSED;
     tree declarator, declspecs, width;
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

	 Oh what a horrid tangled web we weave.  I wonder if MS consiously
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

  value = grokdeclarator (declarator, declspecs, width ? BITFIELD : FIELD, 0);

  finish_decl (value, NULL_TREE, NULL_TREE);
  DECL_INITIAL (value) = width;

  if (flag_objc)
    objc_check_decl (value);
  return value;
}

/* Fill in the fields of a RECORD_TYPE or UNION_TYPE node, T.
   FIELDLIST is a chain of FIELD_DECL nodes for the fields.
   ATTRIBUTES are attributes to be applied to the structure.  */

tree
finish_struct (t, fieldlist, attributes)
     tree t;
     tree fieldlist;
     tree attributes;
{
  tree x;
  int toplevel = global_binding_level == current_binding_level;
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

      /* Detect invalid bit-field size.  */
      if (DECL_INITIAL (x))
	STRIP_NOPS (DECL_INITIAL (x));
      if (DECL_INITIAL (x))
	{
	  if (TREE_CODE (DECL_INITIAL (x)) == INTEGER_CST)
	    constant_expression_warning (DECL_INITIAL (x));
	  else
	    {
	      error_with_decl (x,
			       "bit-field `%s' width not an integer constant");
	      DECL_INITIAL (x) = NULL;
	    }
	}

      /* Detect invalid bit-field type.  */
      if (DECL_INITIAL (x)
	  && TREE_CODE (TREE_TYPE (x)) != INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (x)) != BOOLEAN_TYPE
	  && TREE_CODE (TREE_TYPE (x)) != ENUMERAL_TYPE)
	{
	  error_with_decl (x, "bit-field `%s' has invalid type");
	  DECL_INITIAL (x) = NULL;
	}

      if (DECL_INITIAL (x) && pedantic
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != integer_type_node
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != unsigned_type_node
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != c_bool_type_node
	  /* Accept an enum that's equivalent to int or unsigned int.  */
	  && !(TREE_CODE (TREE_TYPE (x)) == ENUMERAL_TYPE
	       && (TYPE_PRECISION (TREE_TYPE (x))
		   == TYPE_PRECISION (integer_type_node))))
	pedwarn_with_decl (x, "bit-field `%s' type invalid in ISO C");

      /* Detect and ignore out of range field width and process valid
	 field widths.  */
      if (DECL_INITIAL (x))
	{
	  int max_width
	    = (TYPE_MAIN_VARIANT (TREE_TYPE (x)) == c_bool_type_node
	       ? CHAR_TYPE_SIZE : TYPE_PRECISION (TREE_TYPE (x)));

	  if (tree_int_cst_sgn (DECL_INITIAL (x)) < 0)
	    error_with_decl (x, "negative width in bit-field `%s'");
	  else if (0 < compare_tree_int (DECL_INITIAL (x), max_width))
	    pedwarn_with_decl (x, "width of `%s' exceeds its type");
	  else if (integer_zerop (DECL_INITIAL (x)) && DECL_NAME (x) != 0)
	    error_with_decl (x, "zero width for bit-field `%s'");
	  else
	    {
	      /* The test above has assured us that TREE_INT_CST_HIGH is 0.  */
	      unsigned HOST_WIDE_INT width
		= tree_low_cst (DECL_INITIAL (x), 1);

	      if (TREE_CODE (TREE_TYPE (x)) == ENUMERAL_TYPE
		  && (width < min_precision (TYPE_MIN_VALUE (TREE_TYPE (x)),
					     TREE_UNSIGNED (TREE_TYPE (x)))
		      || (width
			  < min_precision (TYPE_MAX_VALUE (TREE_TYPE (x)),
					   TREE_UNSIGNED (TREE_TYPE (x))))))
		warning_with_decl (x,
				   "`%s' is narrower than values of its type");

	      DECL_SIZE (x) = bitsize_int (width);
	      DECL_BIT_FIELD (x) = 1;
	      SET_DECL_C_BIT_FIELD (x);

	      if (width == 0
		  && ! (* targetm.ms_bitfield_layout_p) (t))
		{
		  /* field size 0 => force desired amount of alignment.  */
#ifdef EMPTY_FIELD_BOUNDARY
		  DECL_ALIGN (x) = MAX (DECL_ALIGN (x), EMPTY_FIELD_BOUNDARY);
#endif
#ifdef PCC_BITFIELD_TYPE_MATTERS
		  if (PCC_BITFIELD_TYPE_MATTERS)
		    {
		      DECL_ALIGN (x) = MAX (DECL_ALIGN (x),
					    TYPE_ALIGN (TREE_TYPE (x)));
		      DECL_USER_ALIGN (x) |= TYPE_USER_ALIGN (TREE_TYPE (x));
		    }
#endif
		}
	    }
	}

      DECL_INITIAL (x) = 0;

      /* Detect flexible array member in an invalid context.  */
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	  && TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
	  && TYPE_DOMAIN (TREE_TYPE (x)) != NULL_TREE
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (x))) == NULL_TREE)
	{
	  if (TREE_CODE (t) == UNION_TYPE)
	    error_with_decl (x, "flexible array member in union");
	  else if (TREE_CHAIN (x) != NULL_TREE)
	    error_with_decl (x, "flexible array member not at end of struct");
	  else if (! saw_named_field)
	    error_with_decl (x, "flexible array member in otherwise empty struct");
	}

      if (pedantic && TREE_CODE (t) == RECORD_TYPE
	  && flexible_array_type_p (TREE_TYPE (x)))
	pedwarn_with_decl (x, "invalid use of structure with flexible array member");

      if (DECL_NAME (x))
	saw_named_field = 1;
    }

  /* Delete all duplicate fields from the fieldlist */
  for (x = fieldlist; x && TREE_CHAIN (x);)
    /* Anonymous fields aren't duplicates.  */
    if (DECL_NAME (TREE_CHAIN (x)) == 0)
      x = TREE_CHAIN (x);
    else
      {
	tree y = fieldlist;

	while (1)
	  {
	    if (DECL_NAME (y) == DECL_NAME (TREE_CHAIN (x)))
	      break;
	    if (y == x)
	      break;
	    y = TREE_CHAIN (y);
	  }
	if (DECL_NAME (y) == DECL_NAME (TREE_CHAIN (x)))
	  {
	    error_with_decl (TREE_CHAIN (x), "duplicate member `%s'");
	    TREE_CHAIN (x) = TREE_CHAIN (TREE_CHAIN (x));
	  }
	else
	  x = TREE_CHAIN (x);
      }

  /* Now we have the nearly final fieldlist.  Record it,
     then lay out the structure or union (including the fields).  */

  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);

  /* Delete all zero-width bit-fields from the fieldlist */
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
      && TYPE_MODE (t) != DECL_MODE (TYPE_FIELDS (t)))
    {
      TYPE_TRANSPARENT_UNION (t) = 0;
      warning ("union cannot be made transparent");
    }

  /* If this structure or union completes the type of any previous
     variable declaration, lay it out and output its rtl.  */

  if (current_binding_level->incomplete_list != NULL_TREE)
    {
      tree prev = NULL_TREE;

      for (x = current_binding_level->incomplete_list; x; x = TREE_CHAIN (x))
        {
	  tree decl = TREE_VALUE (x);

	  if (TYPE_MAIN_VARIANT (TREE_TYPE (decl)) == TYPE_MAIN_VARIANT (t)
	      && TREE_CODE (decl) != TYPE_DECL)
	    {
	      layout_decl (decl, 0);
	      /* This is a no-op in c-lang.c or something real in objc-act.c.  */
	      if (flag_objc)
		objc_check_decl (decl);
	      rest_of_decl_compilation (decl, NULL, toplevel, 0);
	      if (! toplevel)
		expand_decl (decl);
	      /* Unlink X from the incomplete list.  */
	      if (prev)
		TREE_CHAIN (prev) = TREE_CHAIN (x);
	      else
	        current_binding_level->incomplete_list = TREE_CHAIN (x);
	    }
	  else if (!COMPLETE_TYPE_P (TREE_TYPE (decl))
		   && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	    {
	      tree element = TREE_TYPE (decl);
	      while (TREE_CODE (element) == ARRAY_TYPE)
		element = TREE_TYPE (element);
	      if (element == t)
		{
		  layout_array_type (TREE_TYPE (decl));
		  if (TREE_CODE (decl) != TYPE_DECL)
		    {
		      layout_decl (decl, 0);
		      if (flag_objc)
			objc_check_decl (decl);
		      rest_of_decl_compilation (decl, NULL, toplevel, 0);
		      if (! toplevel)
			expand_decl (decl);
		    }
		  /* Unlink X from the incomplete list.  */
		  if (prev)
		    TREE_CHAIN (prev) = TREE_CHAIN (x);
		  else
		    current_binding_level->incomplete_list = TREE_CHAIN (x);
		}
	    }
	}
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, toplevel);

  return t;
}

/* Lay out the type T, and its element type, and so on.  */

static void
layout_array_type (t)
     tree t;
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
start_enum (name)
     tree name;
{
  tree enumtype = 0;

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

  if (name != 0)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, current_binding_level, 1);

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
finish_enum (enumtype, values, attributes)
     tree enumtype;
     tree values;
     tree attributes;
{
  tree pair, tem;
  tree minnode = 0, maxnode = 0, enum_value_type;
  int precision, unsign;
  int toplevel = (global_binding_level == current_binding_level);

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
  TYPE_PRECISION (enumtype) = precision;
  TREE_UNSIGNED (enumtype) = unsign;
  TYPE_SIZE (enumtype) = 0;
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
	  DECL_SIZE (enu) = TYPE_SIZE (enumtype);
	  DECL_SIZE_UNIT (enu) = TYPE_SIZE_UNIT (enumtype);
	  DECL_ALIGN (enu) = TYPE_ALIGN (enumtype);
	  DECL_USER_ALIGN (enu) = TYPE_USER_ALIGN (enumtype);
	  DECL_MODE (enu) = TYPE_MODE (enumtype);

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
build_enumerator (name, value)
     tree name, value;
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
start_function (declspecs, declarator, attributes)
     tree declarator, declspecs, attributes;
{
  tree decl1, old_decl;
  tree restype;
  int old_immediate_size_expand = immediate_size_expand;

  current_function_returns_value = 0;  /* Assume, until we see it does.  */
  current_function_returns_null = 0;
  current_function_returns_abnormally = 0;
  warn_about_return_type = 0;
  current_extern_inline = 0;
  named_labels = 0;
  shadowed_labels = 0;

  /* Don't expand any sizes in the return type of the function.  */
  immediate_size_expand = 0;

  decl1 = grokdeclarator (declarator, declspecs, FUNCDEF, 1);

  /* If the declarator is not suitable for a function definition,
     cause a syntax error.  */
  if (decl1 == 0)
    {
      immediate_size_expand = old_immediate_size_expand;
      return 0;
    }

  decl_attributes (&decl1, attributes, 0);

  /* If #pragma weak was used, mark the decl weak now.  */
  if (current_binding_level == global_binding_level)
    maybe_apply_pragma_weak (decl1);

  if (DECL_DECLARED_INLINE_P (decl1)
      && DECL_UNINLINABLE (decl1)
      && lookup_attribute ("noinline", DECL_ATTRIBUTES (decl1)))
    warning_with_decl (decl1,
		       "inline function `%s' given attribute noinline");

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
      current_function_prototype_file = DECL_SOURCE_FILE (old_decl);
      current_function_prototype_line = DECL_SOURCE_LINE (old_decl);
    }

  /* If there is no explicit declaration, look for any out-of-scope implicit
     declarations.  */
  if (old_decl == 0)
    old_decl = IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1));

  /* Optionally warn of old-fashioned def with no previous prototype.  */
  if (warn_strict_prototypes
      && TYPE_ARG_TYPES (TREE_TYPE (decl1)) == 0
      && !(old_decl != 0
	   && (TYPE_ARG_TYPES (TREE_TYPE (old_decl)) != 0
	       || (DECL_BUILT_IN (old_decl)
		   && ! C_DECL_ANTICIPATED (old_decl)))))
    warning ("function declaration isn't a prototype");
  /* Optionally warn of any global def with no previous prototype.  */
  else if (warn_missing_prototypes
	   && TREE_PUBLIC (decl1)
	   && !(old_decl != 0
		&& (TYPE_ARG_TYPES (TREE_TYPE (old_decl)) != 0
		    || (DECL_BUILT_IN (old_decl)
			&& ! C_DECL_ANTICIPATED (old_decl))))
	   && ! MAIN_NAME_P (DECL_NAME (decl1)))
    warning_with_decl (decl1, "no previous prototype for `%s'");
  /* Optionally warn of any def with no previous prototype
     if the function has already been used.  */
  else if (warn_missing_prototypes
	   && old_decl != 0 && TREE_USED (old_decl)
	   && TYPE_ARG_TYPES (TREE_TYPE (old_decl)) == 0)
    warning_with_decl (decl1,
		       "`%s' was used with no prototype before its definition");
  /* Optionally warn of any global def with no previous declaration.  */
  else if (warn_missing_declarations
	   && TREE_PUBLIC (decl1)
	   && old_decl == 0
	   && ! MAIN_NAME_P (DECL_NAME (decl1)))
    warning_with_decl (decl1, "no previous declaration for `%s'");
  /* Optionally warn of any def with no previous declaration
     if the function has already been used.  */
  else if (warn_missing_declarations
	   && old_decl != 0 && TREE_USED (old_decl)
	   && old_decl == IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1)))
    warning_with_decl (decl1,
		       "`%s' was used with no declaration before its definition");

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

  /* Warn for unlikely, improbable, or stupid declarations of `main'.  */
  if (warn_main > 0 && MAIN_NAME_P (DECL_NAME (decl1)))
    {
      tree args;
      int argct = 0;

      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (decl1)))
	  != integer_type_node)
	pedwarn_with_decl (decl1, "return type of `%s' is not `int'");

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
		pedwarn_with_decl (decl1,
				   "first argument of `%s' should be `int'");
	      break;

	    case 2:
	      if (TREE_CODE (type) != POINTER_TYPE
		  || TREE_CODE (TREE_TYPE (type)) != POINTER_TYPE
		  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type)))
		      != char_type_node))
		pedwarn_with_decl (decl1,
				   "second argument of `%s' should be `char **'");
	      break;

	    case 3:
	      if (TREE_CODE (type) != POINTER_TYPE
		  || TREE_CODE (TREE_TYPE (type)) != POINTER_TYPE
		  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type)))
		      != char_type_node))
		pedwarn_with_decl (decl1,
				   "third argument of `%s' should probably be `char **'");
	      break;
	    }
	}

      /* It is intentional that this message does not mention the third
	 argument because it's only mentioned in an appendix of the
	 standard.  */
      if (argct > 0 && (argct < 2 || argct > 3))
	pedwarn_with_decl (decl1, "`%s' takes only zero or two arguments");

      if (! TREE_PUBLIC (decl1))
	pedwarn_with_decl (decl1, "`%s' is normally a non-static function");
    }

  /* Record the decl so that the function name is defined.
     If we already have a decl for this name, and it is a FUNCTION_DECL,
     use the old decl.  */

  current_function_decl = pushdecl (decl1);

  pushlevel (0);
  declare_parm_level (1);
  current_binding_level->subblocks_tag_transparent = 1;

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

/* Store the parameter declarations into the current function declaration.
   This is called after parsing the parameter declarations, before
   digesting the body of the function.

   For an old-style definition, modify the function's type
   to specify at least the number of arguments.  */

void
store_parm_decls ()
{
  tree fndecl = current_function_decl;
  tree parm;

  /* This is either a chain of PARM_DECLs (if a prototype was used)
     or a list of IDENTIFIER_NODEs (for an old-fashioned C definition).  */
  tree specparms = current_function_parms;

  /* This is a list of types declared among parms in a prototype.  */
  tree parmtags = current_function_parm_tags;

  /* This is a chain of PARM_DECLs from old-style parm declarations.  */
  tree parmdecls = getdecls ();

  /* This is a chain of any other decls that came in among the parm
     declarations.  If a parm is declared with  enum {foo, bar} x;
     then CONST_DECLs for foo and bar are put here.  */
  tree nonparms = 0;

  /* The function containing FNDECL, if any.  */
  tree context = decl_function_context (fndecl);

  /* Nonzero if this definition is written with a prototype.  */
  int prototype = 0;

  int saved_warn_shadow = warn_shadow;

  /* Don't re-emit shadow warnings.  */
  warn_shadow = 0;

  if (specparms != 0 && TREE_CODE (specparms) != TREE_LIST)
    {
      /* This case is when the function was defined with an ANSI prototype.
	 The parms already have decls, so we need not do anything here
	 except record them as in effect
	 and complain if any redundant old-style parm decls were written.  */

      tree next;
      tree others = 0;

      prototype = 1;

      if (parmdecls != 0)
	{
	  tree decl, link;

	  error_with_decl (fndecl,
			   "parm types given both in parmlist and separately");
	  /* Get rid of the erroneous decls; don't keep them on
	     the list of parms, since they might not be PARM_DECLs.  */
	  for (decl = current_binding_level->names;
	       decl; decl = TREE_CHAIN (decl))
	    if (DECL_NAME (decl))
	      IDENTIFIER_LOCAL_VALUE (DECL_NAME (decl)) = 0;
	  for (link = current_binding_level->shadowed;
	       link; link = TREE_CHAIN (link))
	    IDENTIFIER_LOCAL_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);
	  current_binding_level->names = 0;
	  current_binding_level->shadowed = 0;
	}

      specparms = nreverse (specparms);
      for (parm = specparms; parm; parm = next)
	{
	  next = TREE_CHAIN (parm);
	  if (TREE_CODE (parm) == PARM_DECL)
	    {
	      if (DECL_NAME (parm) == 0)
		error_with_decl (parm, "parameter name omitted");
	      else if (TREE_CODE (TREE_TYPE (parm)) != ERROR_MARK
		       && VOID_TYPE_P (TREE_TYPE (parm)))
		{
		  error_with_decl (parm, "parameter `%s' declared void");
		  /* Change the type to error_mark_node so this parameter
		     will be ignored by assign_parms.  */
		  TREE_TYPE (parm) = error_mark_node;
		}
	      pushdecl (parm);
	    }
	  else
	    {
	      /* If we find an enum constant or a type tag,
		 put it aside for the moment.  */
	      TREE_CHAIN (parm) = 0;
	      others = chainon (others, parm);
	    }
	}

      /* Get the decls in their original chain order
	 and record in the function.  */
      DECL_ARGUMENTS (fndecl) = getdecls ();

#if 0
      /* If this function takes a variable number of arguments,
	 add a phony parameter to the end of the parm list,
	 to represent the position of the first unnamed argument.  */
      if (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (fndecl))))
	  != void_type_node)
	{
	  tree dummy = build_decl (PARM_DECL, NULL_TREE, void_type_node);
	  /* Let's hope the address of the unnamed parm
	     won't depend on its type.  */
	  TREE_TYPE (dummy) = integer_type_node;
	  DECL_ARG_TYPE (dummy) = integer_type_node;
	  DECL_ARGUMENTS (fndecl) = chainon (DECL_ARGUMENTS (fndecl), dummy);
	}
#endif

      /* Now pushdecl the enum constants.  */
      for (parm = others; parm; parm = next)
	{
	  next = TREE_CHAIN (parm);
	  if (DECL_NAME (parm) == 0)
	    ;
	  else if (TYPE_MAIN_VARIANT (TREE_TYPE (parm)) == void_type_node)
	    ;
	  else if (TREE_CODE (parm) != PARM_DECL)
	    pushdecl (parm);
	}

      storetags (chainon (parmtags, gettags ()));
    }
  else
    {
      /* SPECPARMS is an identifier list--a chain of TREE_LIST nodes
	 each with a parm name as the TREE_VALUE.

	 PARMDECLS is a chain of declarations for parameters.
	 Warning! It can also contain CONST_DECLs which are not parameters
	 but are names of enumerators of any enum types
	 declared among the parameters.

	 First match each formal parameter name with its declaration.
	 Associate decls with the names and store the decls
	 into the TREE_PURPOSE slots.  */

      /* We use DECL_WEAK as a flag to show which parameters have been
	 seen already since it is not used on PARM_DECL or CONST_DECL.  */
      for (parm = parmdecls; parm; parm = TREE_CHAIN (parm))
	DECL_WEAK (parm) = 0;

      for (parm = specparms; parm; parm = TREE_CHAIN (parm))
	{
	  tree tail, found = NULL;

	  if (TREE_VALUE (parm) == 0)
	    {
	      error_with_decl (fndecl,
			       "parameter name missing from parameter list");
	      TREE_PURPOSE (parm) = 0;
	      continue;
	    }

	  /* See if any of the parmdecls specifies this parm by name.
	     Ignore any enumerator decls.  */
	  for (tail = parmdecls; tail; tail = TREE_CHAIN (tail))
	    if (DECL_NAME (tail) == TREE_VALUE (parm)
		&& TREE_CODE (tail) == PARM_DECL)
	      {
		found = tail;
		break;
	      }

	  /* If declaration already marked, we have a duplicate name.
	     Complain, and don't use this decl twice.  */
	  if (found && DECL_WEAK (found))
	    {
	      error_with_decl (found, "multiple parameters named `%s'");
	      found = 0;
	    }

	  /* If the declaration says "void", complain and ignore it.  */
	  if (found && VOID_TYPE_P (TREE_TYPE (found)))
	    {
	      error_with_decl (found, "parameter `%s' declared void");
	      TREE_TYPE (found) = integer_type_node;
	      DECL_ARG_TYPE (found) = integer_type_node;
	      layout_decl (found, 0);
	    }

	  /* If no declaration found, default to int.  */
	  if (!found)
	    {
	      found = build_decl (PARM_DECL, TREE_VALUE (parm),
				  integer_type_node);
	      DECL_ARG_TYPE (found) = TREE_TYPE (found);
	      DECL_SOURCE_LINE (found) = DECL_SOURCE_LINE (fndecl);
	      DECL_SOURCE_FILE (found) = DECL_SOURCE_FILE (fndecl);
	      if (flag_isoc99)
		pedwarn_with_decl (found, "type of `%s' defaults to `int'");
	      else if (extra_warnings)
		warning_with_decl (found, "type of `%s' defaults to `int'");
	      pushdecl (found);
	    }

	  TREE_PURPOSE (parm) = found;

	  /* Mark this decl as "already found".  */
	  DECL_WEAK (found) = 1;
	}

      /* Put anything which is on the parmdecls chain and which is
	 not a PARM_DECL onto the list NONPARMS.  (The types of
	 non-parm things which might appear on the list include
	 enumerators and NULL-named TYPE_DECL nodes.) Complain about
	 any actual PARM_DECLs not matched with any names.  */

      nonparms = 0;
      for (parm = parmdecls; parm;)
	{
	  tree next = TREE_CHAIN (parm);
	  TREE_CHAIN (parm) = 0;

	  if (TREE_CODE (parm) != PARM_DECL)
	    nonparms = chainon (nonparms, parm);
	  else
	    {
	      /* Complain about args with incomplete types.  */
	      if (!COMPLETE_TYPE_P (TREE_TYPE (parm)))
		{
		  error_with_decl (parm, "parameter `%s' has incomplete type");
		  TREE_TYPE (parm) = error_mark_node;
		}

	      if (! DECL_WEAK (parm))
		{
		  error_with_decl (parm,
				   "declaration for parameter `%s' but no such parameter");
	          /* Pretend the parameter was not missing.
		     This gets us to a standard state and minimizes
		     further error messages.  */
		  specparms
		    = chainon (specparms,
			       tree_cons (parm, NULL_TREE, NULL_TREE));
		}
	    }

	  parm = next;
	}

      /* Chain the declarations together in the order of the list of
         names.  Store that chain in the function decl, replacing the
         list of names.  */
      parm = specparms;
      DECL_ARGUMENTS (fndecl) = 0;
      {
	tree last;
	for (last = 0; parm; parm = TREE_CHAIN (parm))
	  if (TREE_PURPOSE (parm))
	    {
	      if (last == 0)
		DECL_ARGUMENTS (fndecl) = TREE_PURPOSE (parm);
	      else
		TREE_CHAIN (last) = TREE_PURPOSE (parm);
	      last = TREE_PURPOSE (parm);
	      TREE_CHAIN (last) = 0;
	    }
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
		  error_with_file_and_line (current_function_prototype_file,
					    current_function_prototype_line,
					    "prototype declaration");
		  break;
		}
	      /* Type for passing arg must be consistent with that
		 declared for the arg.  ISO C says we take the unqualified
		 type for parameters declared with qualified type.  */
	      if (! comptypes (TYPE_MAIN_VARIANT (DECL_ARG_TYPE (parm)),
			       TYPE_MAIN_VARIANT (TREE_VALUE (type))))
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

		      if (PROMOTE_PROTOTYPES
			  && INTEGRAL_TYPE_P (TREE_TYPE (parm))
			  && TYPE_PRECISION (TREE_TYPE (parm))
			  < TYPE_PRECISION (integer_type_node))
			DECL_ARG_TYPE (parm) = integer_type_node;

		      if (pedantic)
			{
			  pedwarn ("promoted argument `%s' doesn't match prototype",
				   IDENTIFIER_POINTER (DECL_NAME (parm)));
			  warning_with_file_and_line
			    (current_function_prototype_file,
			     current_function_prototype_line,
			     "prototype declaration");
			}
		    }
		  else
		    {
		      error ("argument `%s' doesn't match prototype",
			     IDENTIFIER_POINTER (DECL_NAME (parm)));
		      error_with_file_and_line (current_function_prototype_file,
						current_function_prototype_line,
						"prototype declaration");
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

      /* Now store the final chain of decls for the arguments
	 as the decl-chain of the current lexical scope.
	 Put the enumerators in as well, at the front so that
	 DECL_ARGUMENTS is not modified.  */

      storedecls (chainon (nonparms, DECL_ARGUMENTS (fndecl)));
    }

  /* Make sure the binding level for the top of the function body
     gets a BLOCK if there are any in the function.
     Otherwise, the dbx output is wrong.  */

  keep_next_if_subblocks = 1;

  /* ??? This might be an improvement,
     but needs to be thought about some more.  */
#if 0
  keep_next_level_flag = 1;
#endif

  /* Write a record describing this function definition to the prototypes
     file (if requested).  */

  gen_aux_info_record (fndecl, 1, 0, prototype);

  /* Initialize the RTL code for the function.  */
  init_function_start (fndecl, input_filename, lineno);

  /* Begin the statement tree for this function.  */
  begin_stmt_tree (&DECL_SAVED_TREE (current_function_decl));

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

  warn_shadow = saved_warn_shadow;
}

/* Finish up a function declaration and compile that function
   all the way to assembler language output.  The free the storage
   for the function definition.

   This is called after parsing the body of the function definition.

   NESTED is nonzero if the function being finished is nested in another.
   CAN_DEFER_P is nonzero if the function may be deferred.  */

void
finish_function (nested, can_defer_p)
     int nested;
     int can_defer_p;
{
  tree fndecl = current_function_decl;

#if 0
  /* This caused &foo to be of type ptr-to-const-function which then
     got a warning when stored in a ptr-to-function variable.  */
  TREE_READONLY (fndecl) = 1;
#endif

  poplevel (1, 0, 1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Must mark the RESULT_DECL as being in this function.  */

  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  if (MAIN_NAME_P (DECL_NAME (fndecl)) && flag_hosted)
    {
      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (fndecl)))
	  != integer_type_node)
	{
	  /* If warn_main is 1 (-Wmain) or 2 (-Wall), we have already warned.
	     If warn_main is -1 (-Wno-main) we don't want to be warned.  */
	  if (! warn_main)
	    pedwarn_with_decl (fndecl, "return type of `%s' is not `int'");
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

  /* Clear out memory we no longer need.  */
  free_after_parsing (cfun);
  /* Since we never call rest_of_compilation, we never clear
     CFUN.  Do so explicitly.  */
  free_after_compilation (cfun);
  cfun = NULL;

  if (! nested)
    {
      /* Generate RTL for the body of this function.  */
      c_expand_body (fndecl, nested, can_defer_p);

      /* Let the error reporting routines know that we're outside a
	 function.  For a nested function, this value is used in
	 c_pop_function_context and then reset via pop_function_context.  */
      current_function_decl = NULL;
    }
}

/* Generate the RTL for a deferred function FNDECL.  */

void
c_expand_deferred_function (fndecl)
     tree fndecl;
{
  /* DECL_INLINE or DECL_RESULT might got cleared after the inline
     function was deferred, e.g. in duplicate_decls.  */
  if (DECL_INLINE (fndecl) && DECL_RESULT (fndecl))
    {
      c_expand_body (fndecl, 0, 0);
      current_function_decl = NULL;
    }
}

/* Called to move the SAVE_EXPRs for parameter declarations in a
   nested function into the nested function.  DATA is really the
   nested FUNCTION_DECL.  */

static tree
set_save_expr_context (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees;
     void *data;
{
  if (TREE_CODE (*tp) == SAVE_EXPR && !SAVE_EXPR_CONTEXT (*tp))
    SAVE_EXPR_CONTEXT (*tp) = (tree) data;
  /* Do not walk back into the SAVE_EXPR_CONTEXT; that will cause
     circularity.  */
  else if (DECL_P (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Generate the RTL for the body of FNDECL.  If NESTED_P is nonzero,
   then we are already in the process of generating RTL for another
   function.  If can_defer_p is zero, we won't attempt to defer the
   generation of RTL.  */

static void
c_expand_body (fndecl, nested_p, can_defer_p)
     tree fndecl;
     int nested_p, can_defer_p;
{
  int uninlinable = 1;
  int saved_lineno;
  const char *saved_input_filename;

  /* There's no reason to do any of the work here if we're only doing
     semantic analysis; this code just generates RTL.  */
  if (flag_syntax_only)
    return;

  saved_lineno = lineno;
  saved_input_filename = input_filename;
  lineno = DECL_SOURCE_LINE (fndecl);
  input_filename = DECL_SOURCE_FILE (fndecl);

  if (flag_inline_trees)
    {
      /* First, cache whether the current function is inlinable.  Some
         predicates depend on cfun and current_function_decl to
         function completely.  */
      timevar_push (TV_INTEGRATION);
      uninlinable = ! tree_inlinable_function_p (fndecl);
      
      if (! uninlinable && can_defer_p
	  /* Save function tree for inlining.  Should return 0 if the
             language does not support function deferring or the
             function could not be deferred.  */
	  && defer_fn (fndecl))
	{
	  /* Let the back-end know that this function exists.  */
	  (*debug_hooks->deferred_inline_function) (fndecl);
          timevar_pop (TV_INTEGRATION);
          lineno = saved_lineno;
          input_filename = saved_input_filename;
	  return;
	}
      
      /* Then, inline any functions called in it.  */
      optimize_inline_calls (fndecl);
      timevar_pop (TV_INTEGRATION);
    }

  timevar_push (TV_EXPAND);

  if (nested_p)
    {
      /* Make sure that we will evaluate variable-sized types involved
	 in our function's type.  */
      expand_pending_sizes (DECL_LANG_SPECIFIC (fndecl)->pending_sizes);
      /* Squirrel away our current state.  */
      push_function_context ();
    }

  /* Initialize the RTL code for the function.  */
  current_function_decl = fndecl;
  init_function_start (fndecl, input_filename, DECL_SOURCE_LINE (fndecl));

  /* This function is being processed in whole-function mode.  */
  cfun->x_whole_function_mode_p = 1;

  /* Even though we're inside a function body, we still don't want to
     call expand_expr to calculate the size of a variable-sized array.
     We haven't necessarily assigned RTL to all variables yet, so it's
     not safe to try to expand expressions involving them.  */
  immediate_size_expand = 0;
  cfun->x_dont_save_pending_sizes_p = 1;

  /* Set up parameters and prepare for return, for the function.  */
  expand_function_start (fndecl, 0);

  /* If the function has a variably modified type, there may be
     SAVE_EXPRs in the parameter types.  Their context must be set to
     refer to this function; they cannot be expanded in the containing
     function.  */
  if (decl_function_context (fndecl)
      && variably_modified_type_p (TREE_TYPE (fndecl)))
    walk_tree (&TREE_TYPE (fndecl), set_save_expr_context, fndecl,
	       NULL);
	     
  /* If this function is `main', emit a call to `__main'
     to run global initializers, etc.  */
  if (DECL_NAME (fndecl)
      && MAIN_NAME_P (DECL_NAME (fndecl))
      && DECL_CONTEXT (fndecl) == NULL_TREE)
    expand_main_function ();

  /* Generate the RTL for this function.  */
  expand_stmt (DECL_SAVED_TREE (fndecl));

  /* Keep the function body if it's needed for inlining or dumping.  */
  if (uninlinable && !dump_enabled_p (TDI_all))
    {
      /* Allow the body of the function to be garbage collected.  */
      DECL_SAVED_TREE (fndecl) = NULL_TREE;
    }

  /* We hard-wired immediate_size_expand to zero above.
     expand_function_end will decrement this variable.  So, we set the
     variable to one here, so that after the decrement it will remain
     zero.  */
  immediate_size_expand = 1;

  /* Allow language dialects to perform special processing.  */
  if (lang_expand_function_end)
    (*lang_expand_function_end) ();

  /* Generate rtl for function exit.  */
  expand_function_end (input_filename, lineno, 0);

  /* If this is a nested function, protect the local variables in the stack
     above us from being collected while we're compiling this function.  */
  if (nested_p)
    ggc_push_context ();

  /* Run the optimizers and output the assembler code for this function.  */
  rest_of_compilation (fndecl);

  /* Undo the GC context switch.  */
  if (nested_p)
    ggc_pop_context ();

  /* With just -W, complain only if function returns both with
     and without a value.  */
  if (extra_warnings
      && current_function_returns_value
      && current_function_returns_null)
    warning ("this function may return with or without a value");

  /* If requested, warn about function definitions where the function will
     return a value (usually of some struct or union type) which itself will
     take up a lot of stack space.  */

  if (warn_larger_than && !DECL_EXTERNAL (fndecl) && TREE_TYPE (fndecl))
    {
      tree ret_type = TREE_TYPE (TREE_TYPE (fndecl));

      if (ret_type && TYPE_SIZE_UNIT (ret_type)
	  && TREE_CODE (TYPE_SIZE_UNIT (ret_type)) == INTEGER_CST
	  && 0 < compare_tree_int (TYPE_SIZE_UNIT (ret_type),
				   larger_than_size))
	{
	  unsigned int size_as_int
	    = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (ret_type));

	  if (compare_tree_int (TYPE_SIZE_UNIT (ret_type), size_as_int) == 0)
	    warning_with_decl (fndecl,
			       "size of return value of `%s' is %u bytes",
			       size_as_int);
	  else
	    warning_with_decl (fndecl,
			       "size of return value of `%s' is larger than %d bytes",
			       larger_than_size);
	}
    }

  if (DECL_SAVED_INSNS (fndecl) == 0 && ! nested_p
      && ! flag_inline_trees)
    {
      /* Stop pointing to the local nodes about to be freed.
	 But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.
	 For a nested function, this is done in c_pop_function_context.
	 If rest_of_compilation set this to 0, leave it 0.  */
      if (DECL_INITIAL (fndecl) != 0)
	DECL_INITIAL (fndecl) = error_mark_node;

      DECL_ARGUMENTS (fndecl) = 0;
    }

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

  if (nested_p)
    /* Return to the enclosing function.  */
    pop_function_context ();
  timevar_pop (TV_EXPAND);

  lineno = saved_lineno;
  input_filename = saved_input_filename;
}

/* Check the declarations given in a for-loop for satisfying the C99
   constraints.  */
void
check_for_loop_decls ()
{
  tree t;

  if (!flag_isoc99)
    {
      /* If we get here, declarations have been used in a for loop without
	 the C99 for loop scope.  This doesn't make much sense, so don't
	 allow it.  */
      error ("`for' loop initial declaration used outside C99 mode");
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

  for (t = gettags (); t; t = TREE_CHAIN (t))
    {
      if (TREE_PURPOSE (t) != 0)
        {
          enum tree_code code = TREE_CODE (TREE_VALUE (t));
	  
          if (code == RECORD_TYPE)
            error ("`struct %s' declared in `for' loop initial declaration",
                   IDENTIFIER_POINTER (TREE_PURPOSE (t)));
          else if (code == UNION_TYPE)
            error ("`union %s' declared in `for' loop initial declaration",
                   IDENTIFIER_POINTER (TREE_PURPOSE (t)));
          else
            error ("`enum %s' declared in `for' loop initial declaration",
                   IDENTIFIER_POINTER (TREE_PURPOSE (t)));
        }
    }

  for (t = getdecls (); t; t = TREE_CHAIN (t))
    {
      if (TREE_CODE (t) != VAR_DECL && DECL_NAME (t))
	error_with_decl (t, "declaration of non-variable `%s' in `for' loop initial declaration");
      else if (TREE_STATIC (t))
	error_with_decl (t, "declaration of static variable `%s' in `for' loop initial declaration");
      else if (DECL_EXTERNAL (t))
	error_with_decl (t, "declaration of `extern' variable `%s' in `for' loop initial declaration");
    }
}

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct language_function GTY(())
{
  struct c_language_function base;
  tree named_labels;
  tree shadowed_labels;
  int returns_value;
  int returns_null;
  int returns_abnormally;
  int warn_about_return_type;
  int extern_inline;
  struct binding_level *binding_level;
};

/* Save and reinitialize the variables
   used during compilation of a C function.  */

void
c_push_function_context (f)
     struct function *f;
{
  struct language_function *p;
  p = ((struct language_function *)
       ggc_alloc (sizeof (struct language_function)));
  f->language = p;

  p->base.x_stmt_tree = c_stmt_tree;
  p->base.x_scope_stmt_stack = c_scope_stmt_stack;
  p->named_labels = named_labels;
  p->shadowed_labels = shadowed_labels;
  p->returns_value = current_function_returns_value;
  p->returns_null = current_function_returns_null;
  p->returns_abnormally = current_function_returns_abnormally;
  p->warn_about_return_type = warn_about_return_type;
  p->extern_inline = current_extern_inline;
  p->binding_level = current_binding_level;
}

/* Restore the variables used during compilation of a C function.  */

void
c_pop_function_context (f)
     struct function *f;
{
  struct language_function *p = f->language;
  tree link;

  /* Bring back all the labels that were shadowed.  */
  for (link = shadowed_labels; link; link = TREE_CHAIN (link))
    if (DECL_NAME (TREE_VALUE (link)) != 0)
      IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link)))
	= TREE_VALUE (link);

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
  named_labels = p->named_labels;
  shadowed_labels = p->shadowed_labels;
  current_function_returns_value = p->returns_value;
  current_function_returns_null = p->returns_null;
  current_function_returns_abnormally = p->returns_abnormally;
  warn_about_return_type = p->warn_about_return_type;
  current_extern_inline = p->extern_inline;
  current_binding_level = p->binding_level;

  f->language = NULL;
}

/* Copy the DECL_LANG_SPECIFIC data associated with DECL.  */

void
c_dup_lang_specific_decl (decl)
     tree decl;
{
  struct lang_decl *ld;

  if (!DECL_LANG_SPECIFIC (decl))
    return;

  ld = (struct lang_decl *) ggc_alloc (sizeof (struct lang_decl));
  memcpy ((char *) ld, (char *) DECL_LANG_SPECIFIC (decl),
	  sizeof (struct lang_decl));
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
stmts_are_full_exprs_p ()
{
  return 0;
}

/* Returns the stmt_tree (if any) to which statements are currently
   being added.  If there is no active statement-tree, NULL is
   returned.  */

stmt_tree
current_stmt_tree ()
{
  return &c_stmt_tree;
}

/* Returns the stack of SCOPE_STMTs for the current function.  */

tree *
current_scope_stmt_stack ()
{
  return &c_scope_stmt_stack;
}

/* Nonzero if TYPE is an anonymous union or struct type.  Always 0 in
   C.  */

int
anon_aggr_type_p (node)
     tree node ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Dummy function in place of callback used by C++.  */

void
extract_interface_info ()
{
}

/* Return a new COMPOUND_STMT, after adding it to the current
   statement tree.  */

tree
c_begin_compound_stmt ()
{
  tree stmt;

  /* Create the COMPOUND_STMT.  */
  stmt = add_stmt (build_stmt (COMPOUND_STMT, NULL_TREE));

  return stmt;
}

/* Expand T (a DECL_STMT) if it declares an entity not handled by the
   common code.  */

void
c_expand_decl_stmt (t)
     tree t;
{
  tree decl = DECL_STMT_DECL (t);

  /* Expand nested functions.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_CONTEXT (decl) == current_function_decl
      && DECL_SAVED_TREE (decl))
    c_expand_body (decl, /*nested_p=*/1, /*can_defer_p=*/0);
}

/* Return the IDENTIFIER_GLOBAL_VALUE of T, for use in common code, since
   the definition of IDENTIFIER_GLOBAL_VALUE is different for C and C++.  */

tree
identifier_global_value	(t)
     tree t;
{
  return IDENTIFIER_GLOBAL_VALUE (t);
}

/* Record a builtin type for C.  If NAME is non-NULL, it is the name used;
   otherwise the name is found in ridpointers from RID_INDEX.  */

void
record_builtin_type (rid_index, name, type)
     enum rid rid_index;
     const char *name;
     tree type;
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
build_void_list_node ()
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
make_pointer_declarator (type_quals_attrs, target)
     tree type_quals_attrs, target;
{
  tree quals, attrs;
  tree itarget = target;
  split_specs_attrs (type_quals_attrs, &quals, &attrs);
  if (attrs != NULL_TREE)
    itarget = tree_cons (attrs, target, NULL_TREE);
  return build1 (INDIRECT_REF, quals, itarget);
}

#include "gt-c-decl.h"
