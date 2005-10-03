/* gfortran backend interface
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation,
   Inc.
   Contributed by Paul Brook.

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* f95-lang.c-- GCC backend interface stuff */

/* declare required prototypes: */

#include "config.h"
#include "system.h"
#include "ansidecl.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-gimple.h"
#include "flags.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "timevar.h"
#include "tm.h"
#include "function.h"
#include "ggc.h"
#include "toplev.h"
#include "target.h"
#include "debug.h"
#include "diagnostic.h"
#include "tree-dump.h"
#include "cgraph.h"

#include "gfortran.h"
#include "trans.h"
#include "trans-types.h"
#include "trans-const.h"

/* Language-dependent contents of an identifier.  */

struct lang_identifier
GTY(())
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union lang_tree_node
GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
     chain_next ("(union lang_tree_node *)TREE_CHAIN (&%h.generic)")))
{
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY((tag ("1"))) identifier;
};

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct language_function
GTY(())
{
  /* struct gfc_language_function base; */
  struct binding_level *binding_level;
};

/* We don't have a lex/yacc lexer/parser, but toplev expects these to
   exist anyway.  */
void yyerror (const char *str);
int yylex (void);

static void gfc_init_decl_processing (void);
static void gfc_init_builtin_functions (void);

/* Each front end provides its own.  */
static bool gfc_init (void);
static void gfc_finish (void);
static void gfc_print_identifier (FILE *, tree, int);
static bool gfc_mark_addressable (tree);
void do_function_end (void);
int global_bindings_p (void);
void insert_block (tree);
static void gfc_clear_binding_stack (void);
static void gfc_be_parse_file (int);
static void gfc_expand_function (tree);

#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_FINISH
#undef LANG_HOOKS_INIT_OPTIONS
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PRINT_IDENTIFIER
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_MARK_ADDRESSABLE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_UNSIGNED_TYPE
#undef LANG_HOOKS_SIGNED_TYPE
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#undef LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION
#undef LANG_HOOKS_CLEAR_BINDING_STACK

/* Define lang hooks.  */
#define LANG_HOOKS_NAME                 "GNU F95"
#define LANG_HOOKS_INIT                 gfc_init
#define LANG_HOOKS_FINISH               gfc_finish
#define LANG_HOOKS_INIT_OPTIONS         gfc_init_options
#define LANG_HOOKS_HANDLE_OPTION        gfc_handle_option
#define LANG_HOOKS_POST_OPTIONS		gfc_post_options
#define LANG_HOOKS_PRINT_IDENTIFIER     gfc_print_identifier
#define LANG_HOOKS_PARSE_FILE           gfc_be_parse_file
#define LANG_HOOKS_MARK_ADDRESSABLE        gfc_mark_addressable
#define LANG_HOOKS_TYPE_FOR_MODE           gfc_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE           gfc_type_for_size
#define LANG_HOOKS_UNSIGNED_TYPE           gfc_unsigned_type
#define LANG_HOOKS_SIGNED_TYPE             gfc_signed_type
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE gfc_signed_or_unsigned_type
#define LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION gfc_expand_function
#define LANG_HOOKS_CLEAR_BINDING_STACK     gfc_clear_binding_stack

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */

/* Tree code classes.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const enum tree_code_class tree_code_type[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *const tree_code_name[] = {
#include "tree.def"
};
#undef DEFTREECODE


#define NULL_BINDING_LEVEL (struct binding_level *) NULL

/* A chain of binding_level structures awaiting reuse.  */

static GTY(()) struct binding_level *free_binding_level;

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
tree *ridpointers = NULL;

/* language-specific flags.  */

static void
gfc_expand_function (tree fndecl)
{
  tree_rest_of_compilation (fndecl);
}


/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, boolean_false_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `boolean_type_node'.
   This is much simpler than the corresponding C version because we have a
   distinct boolean type.  */

tree
gfc_truthvalue_conversion (tree expr)
{
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case BOOLEAN_TYPE:
      if (TREE_TYPE (expr) == boolean_type_node)
	return expr;
      else if (COMPARISON_CLASS_P (expr))
	{
	  TREE_TYPE (expr) = boolean_type_node;
	  return expr;
	}
      else if (TREE_CODE (expr) == NOP_EXPR)
        return build1 (NOP_EXPR, boolean_type_node,
	               TREE_OPERAND (expr, 0));
      else
        return build1 (NOP_EXPR, boolean_type_node, expr);

    case INTEGER_TYPE:
      if (TREE_CODE (expr) == INTEGER_CST)
	return integer_zerop (expr) ? boolean_false_node : boolean_true_node;
      else
        return build2 (NE_EXPR, boolean_type_node, expr, integer_zero_node);

    default:
      internal_error ("Unexpected type in truthvalue_conversion");
    }
}

static void
gfc_create_decls (void)
{
  /* GCC builtins.  */
  gfc_init_builtin_functions ();

  /* Runtime/IO library functions.  */
  gfc_build_builtin_function_decls ();

  gfc_init_constants ();
}

static void
gfc_be_parse_file (int set_yydebug ATTRIBUTE_UNUSED)
{
  int errors;
  int warnings;

  gfc_create_decls ();
  gfc_parse_file ();
  gfc_generate_constructors ();

  cgraph_finalize_compilation_unit ();
  cgraph_optimize ();

  /* Tell the frontent about any errors.  */
  gfc_get_errors (&warnings, &errors);
  errorcount += errors;
  warningcount += warnings;
}

/* Initialize everything.  */

static bool
gfc_init (void)
{
#ifdef USE_MAPPED_LOCATION
  linemap_add (&line_table, LC_ENTER, false, gfc_source_file, 1);
  linemap_add (&line_table, LC_RENAME, false, "<built-in>", 0);
#endif

  /* First initialize the backend.  */
  gfc_init_decl_processing ();
  gfc_static_ctors = NULL_TREE;

  /* Then the frontend.  */
  gfc_init_1 ();

  if (gfc_new_file () != SUCCESS)
    fatal_error ("can't open input file: %s", gfc_source_file);
  return true;
}


static void
gfc_finish (void)
{
  gfc_done_1 ();
  gfc_release_include_path ();
  return;
}

static void
gfc_print_identifier (FILE * file ATTRIBUTE_UNUSED,
		      tree node ATTRIBUTE_UNUSED,
		      int indent ATTRIBUTE_UNUSED)
{
  return;
}


/* These functions and variables deal with binding contours.  We only
   need these functions for the list of PARM_DECLs, but we leave the
   functions more general; these are a simplified version of the
   functions from GNAT.  */

/* For each binding contour we allocate a binding_level structure which records
   the entities defined or declared in that contour. Contours include:

        the global one
        one for each subprogram definition
        one for each compound statement (declare block)

   Binding contours are used to create GCC tree BLOCK nodes.  */

struct binding_level
GTY(())
{
  /* A chain of ..._DECL nodes for all variables, constants, functions,
     parameters and type declarations.  These ..._DECL nodes are chained
     through the TREE_CHAIN field. Note that these ..._DECL nodes are stored
     in the reverse of the order supplied to be compatible with the
     back-end.  */
  tree names;
  /* For each level (except the global one), a chain of BLOCK nodes for all
     the levels that were entered and exited one level down from this one.  */
  tree blocks;
  /* The binding level containing this one (the enclosing binding level).  */
  struct binding_level *level_chain;
};

/* The binding level currently in effect.  */
static GTY(()) struct binding_level *current_binding_level = NULL;

/* The outermost binding level. This binding level is created when the
   compiler is started and it will exist through the entire compilation.  */
static GTY(()) struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */
static struct binding_level clear_binding_level = { NULL, NULL, NULL };

/* Return nonzero if we are currently in the global binding level.  */

int
global_bindings_p (void)
{
  return current_binding_level == global_binding_level ? -1 : 0;
}

tree
getdecls (void)
{
  return current_binding_level->names;
}

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void
pushlevel (int ignore ATTRIBUTE_UNUSED)
{
  struct binding_level *newlevel
    = (struct binding_level *) ggc_alloc (sizeof (struct binding_level));

  *newlevel = clear_binding_level;

  /* Add this level to the front of the chain (stack) of levels that are
     active.  */
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
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
poplevel (int keep, int reverse, int functionbody)
{
  /* Points to a BLOCK tree node. This is the BLOCK node constructed for the
     binding level that we are about to exit and which is returned by this
     routine.  */
  tree block_node = NULL_TREE;
  tree decl_chain;
  tree subblock_chain = current_binding_level->blocks;
  tree subblock_node;

  /* Reverse the list of XXXX_DECL nodes if desired.  Note that the ..._DECL
     nodes chained through the `names' field of current_binding_level are in
     reverse order except for PARM_DECL node, which are explicitly stored in
     the right order.  */
  decl_chain = (reverse) ? nreverse (current_binding_level->names)
    : current_binding_level->names;

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */
  if (keep || functionbody)
    block_node = build_block (keep ? decl_chain : 0, subblock_chain, 0, 0);

  /* Record the BLOCK node just built as the subblock its enclosing scope.  */
  for (subblock_node = subblock_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    BLOCK_SUPERCONTEXT (subblock_node) = block_node;

  /* Clear out the meanings of the local variables of this level.  */

  for (subblock_node = decl_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    if (DECL_NAME (subblock_node) != 0)
      /* If the identifier was used or addressed via a local extern decl,
         don't forget that fact.  */
      if (DECL_EXTERNAL (subblock_node))
	{
	  if (TREE_USED (subblock_node))
	    TREE_USED (DECL_NAME (subblock_node)) = 1;
	  if (TREE_ADDRESSABLE (subblock_node))
	    TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (subblock_node)) = 1;
	}

  /* Pop the current level.  */
  current_binding_level = current_binding_level->level_chain;

  if (functionbody)
    {
      /* This is the top level block of a function. The ..._DECL chain stored
         in BLOCK_VARS are the function's parameters (PARM_DECL nodes). Don't
         leave them in the BLOCK because they are found in the FUNCTION_DECL
         instead.  */
      DECL_INITIAL (current_function_decl) = block_node;
      BLOCK_VARS (block_node) = 0;
    }
  else if (block_node)
    {
      current_binding_level->blocks
	= chainon (current_binding_level->blocks, block_node);
    }

  /* If we did not make a block for the level just exited, any blocks made for
     inner levels (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks of something
     else.  */
  else if (subblock_chain)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblock_chain);
  if (block_node)
    TREE_USED (block_node) = 1;

  return block_node;
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

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node.  */

tree
pushdecl (tree decl)
{
  /* External objects aren't nested, other objects may be.  */
  if ((DECL_EXTERNAL (decl)) || (decl == current_function_decl))
    DECL_CONTEXT (decl) = 0;
  else
    DECL_CONTEXT (decl) = current_function_decl;

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later if necessary.  This needs to be
     this way for compatibility with the back-end.  */

  TREE_CHAIN (decl) = current_binding_level->names;
  current_binding_level->names = decl;

  /* For the declaration of a type, set its name if it is not already set.  */

  if (TREE_CODE (decl) == TYPE_DECL && TYPE_NAME (TREE_TYPE (decl)) == 0)
    {
      if (DECL_SOURCE_LINE (decl) == 0)
	TYPE_NAME (TREE_TYPE (decl)) = decl;
      else
	TYPE_NAME (TREE_TYPE (decl)) = DECL_NAME (decl);
    }

  return decl;
}


/* Like pushdecl, only it places X in GLOBAL_BINDING_LEVEL.  */

tree
pushdecl_top_level (tree x)
{
  tree t;
  struct binding_level *b = current_binding_level;

  current_binding_level = global_binding_level;
  t = pushdecl (x);
  current_binding_level = b;
  return t;
}


/* Clear the binding stack.  */
static void
gfc_clear_binding_stack (void)
{
  while (!global_bindings_p ())
    poplevel (0, 0, 0);
}


#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

/* Create tree nodes for the basic scalar types of Fortran 95,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */
static void
gfc_init_decl_processing (void)
{
  current_function_decl = NULL;
  current_binding_level = NULL_BINDING_LEVEL;
  free_binding_level = NULL_BINDING_LEVEL;

  /* Make the binding_level structure for global names. We move all
     variables that are in a COMMON block to this binding level.  */
  pushlevel (0);
  global_binding_level = current_binding_level;

  /* Build common tree nodes. char_type_node is unsigned because we
     only use it for actual characters, not for INTEGER(1). Also, we
     want double_type_node to actually have double precision.  */
  build_common_tree_nodes (false, false);
  set_sizetype (long_unsigned_type_node);
  build_common_tree_nodes_2 (0);

  /* Set up F95 type nodes.  */
  gfc_init_kinds ();
  gfc_init_types ();
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   In Fortran 95 this is only the case for variables with
   the TARGET attribute, but we implement it here for a
   likely future Cray pointer extension.
   Value is 1 if successful.  */
/* TODO: Check/fix mark_addressable.  */
bool
gfc_mark_addressable (tree exp)
{
  register tree x = exp;
  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x) && DECL_NONLOCAL (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error
		  ("global register variable %qs used in nested function",
		   IDENTIFIER_POINTER (DECL_NAME (x)));
		return false;
	      }
	    pedwarn ("register variable %qs used in nested function",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	else if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("address of global register variable %qs requested",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return true;
	      }

#if 0
	    /* If we are making this addressable due to its having
	       volatile components, give a different error message.  Also
	       handle the case of an unnamed parameter by not trying
	       to give the name.  */

	    else if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (x)))
	      {
		error ("cannot put object with volatile field into register");
		return false;
	      }
#endif

	    pedwarn ("address of register variable %qs requested",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }

	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;

      default:
	return true;
      }
}

/* press the big red button - garbage (ggc) collection is on */

int ggc_p = 1;

/* Builtin function initialization.  */

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  If
   ATTRS is nonzero, use that for the function's attribute list.  */

tree
builtin_function (const char *name,
		  tree type,
		  int function_code,
		  enum built_in_class class,
		  const char *library_name,
		  tree attrs)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));
  make_decl_rtl (decl);
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;

  /* Possibly apply some default attributes to this built-in function.  */
  if (attrs)
    {
      /* FORNOW the only supported attribute is "const".  If others need
         to be supported then see the more general solution in procedure
         builtin_function in c-decl.c  */
      if (lookup_attribute ( "const", attrs ))
        TREE_READONLY (decl) = 1;
    }

  return decl;
}


static void
gfc_define_builtin (const char * name,
		    tree type,
		    int code,
		    const char * library_name,
		    bool const_p)
{
  tree decl;

  decl = builtin_function (name, type, code, BUILT_IN_NORMAL,
			   library_name, NULL_TREE);
  if (const_p)
    TREE_READONLY (decl) = 1;

  built_in_decls[code] = decl;
  implicit_built_in_decls[code] = decl;
}


#define DO_DEFINE_MATH_BUILTIN(code, name, argtype, tbase) \
    gfc_define_builtin ("__builtin_" name "l", tbase##longdouble[argtype], \
                       BUILT_IN_ ## code ## L, name "l", true); \
    gfc_define_builtin ("__builtin_" name, tbase##double[argtype], \
			BUILT_IN_ ## code, name, true); \
    gfc_define_builtin ("__builtin_" name "f", tbase##float[argtype], \
			BUILT_IN_ ## code ## F, name "f", true);

#define DEFINE_MATH_BUILTIN(code, name, argtype) \
    DO_DEFINE_MATH_BUILTIN (code, name, argtype, mfunc_)

#define DEFINE_MATH_BUILTIN_C(code, name, argtype) \
    DO_DEFINE_MATH_BUILTIN (code, name, argtype, mfunc_) \
    DO_DEFINE_MATH_BUILTIN (C##code, "c" name, argtype, mfunc_c)


/* Create function types for builtin functions.  */

static void
build_builtin_fntypes (tree * fntype, tree type)
{
  tree tmp;

  /* type (*) (type) */
  tmp = tree_cons (NULL_TREE, float_type_node, void_list_node);
  fntype[0] = build_function_type (type, tmp);
  /* type (*) (type, type) */
  tmp = tree_cons (NULL_TREE, float_type_node, tmp);
  fntype[1] = build_function_type (type, tmp);
  /* type (*) (int, type) */
  tmp = tree_cons (NULL_TREE, integer_type_node, void_list_node);
  tmp = tree_cons (NULL_TREE, type, tmp);
  fntype[2] = build_function_type (type, tmp);
}


/* Initialization of builtin function nodes.  */

static void
gfc_init_builtin_functions (void)
{
  tree mfunc_float[3];
  tree mfunc_double[3];
  tree mfunc_longdouble[3];
  tree mfunc_cfloat[3];
  tree mfunc_cdouble[3];
  tree mfunc_clongdouble[3];
  tree func_cfloat_float;
  tree func_cdouble_double;
  tree func_clongdouble_longdouble;
  tree ftype;
  tree tmp;

  build_builtin_fntypes (mfunc_float, float_type_node);
  build_builtin_fntypes (mfunc_double, double_type_node);
  build_builtin_fntypes (mfunc_longdouble, long_double_type_node);
  build_builtin_fntypes (mfunc_cfloat, complex_float_type_node);
  build_builtin_fntypes (mfunc_cdouble, complex_double_type_node);
  build_builtin_fntypes (mfunc_clongdouble, complex_long_double_type_node);

  tmp = tree_cons (NULL_TREE, complex_float_type_node, void_list_node);
  func_cfloat_float = build_function_type (float_type_node, tmp);

  tmp = tree_cons (NULL_TREE, complex_double_type_node, void_list_node);
  func_cdouble_double = build_function_type (double_type_node, tmp);

  tmp = tree_cons (NULL_TREE, complex_long_double_type_node, void_list_node);
  func_clongdouble_longdouble =
    build_function_type (long_double_type_node, tmp);

#include "mathbuiltins.def"

  /* We define these separately as the fortran versions have different
     semantics (they return an integer type) */
  gfc_define_builtin ("__builtin_roundl", mfunc_longdouble[0], 
		      BUILT_IN_ROUNDL, "roundl", true);
  gfc_define_builtin ("__builtin_round", mfunc_double[0], 
		      BUILT_IN_ROUND, "round", true);
  gfc_define_builtin ("__builtin_roundf", mfunc_float[0], 
		      BUILT_IN_ROUNDF, "roundf", true);

  gfc_define_builtin ("__builtin_truncl", mfunc_longdouble[0],
		      BUILT_IN_TRUNCL, "truncl", true);
  gfc_define_builtin ("__builtin_trunc", mfunc_double[0],
                      BUILT_IN_TRUNC, "trunc", true);
  gfc_define_builtin ("__builtin_truncf", mfunc_float[0],
                      BUILT_IN_TRUNCF, "truncf", true);

  gfc_define_builtin ("__builtin_cabsl", func_clongdouble_longdouble, 
		      BUILT_IN_CABSL, "cabsl", true);
  gfc_define_builtin ("__builtin_cabs", func_cdouble_double, 
		      BUILT_IN_CABS, "cabs", true);
  gfc_define_builtin ("__builtin_cabsf", func_cfloat_float, 
		      BUILT_IN_CABSF, "cabsf", true);
 
  gfc_define_builtin ("__builtin_copysignl", mfunc_longdouble[1], 
		      BUILT_IN_COPYSIGNL, "copysignl", true);
  gfc_define_builtin ("__builtin_copysign", mfunc_double[1], 
		      BUILT_IN_COPYSIGN, "copysign", true);
  gfc_define_builtin ("__builtin_copysignf", mfunc_float[1], 
		      BUILT_IN_COPYSIGNF, "copysignf", true);

  /* These are used to implement the ** operator.  */
  gfc_define_builtin ("__builtin_powl", mfunc_longdouble[1], 
		      BUILT_IN_POWL, "powl", true);
  gfc_define_builtin ("__builtin_pow", mfunc_double[1], 
		      BUILT_IN_POW, "pow", true);
  gfc_define_builtin ("__builtin_powf", mfunc_float[1], 
		      BUILT_IN_POWF, "powf", true);

  /* Other builtin functions we use.  */

  tmp = tree_cons (NULL_TREE, integer_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_clz", ftype, BUILT_IN_CLZ,
		      "__builtin_clz", true);

  tmp = tree_cons (NULL_TREE, long_integer_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_clzl", ftype, BUILT_IN_CLZL,
		      "__builtin_clzl", true);

  tmp = tree_cons (NULL_TREE, long_long_integer_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_clzll", ftype, BUILT_IN_CLZLL,
		      "__builtin_clzll", true);

  tmp = tree_cons (NULL_TREE, long_integer_type_node, void_list_node);
  tmp = tree_cons (NULL_TREE, long_integer_type_node, tmp);
  ftype = build_function_type (long_integer_type_node, tmp);
  gfc_define_builtin ("__builtin_expect", ftype, BUILT_IN_EXPECT,
		      "__builtin_expect", true);

  build_common_builtin_nodes ();
  targetm.init_builtins ();
}

#undef DEFINE_MATH_BUILTIN_C
#undef DEFINE_MATH_BUILTIN

#include "gt-fortran-f95-lang.h"
#include "gtype-fortran.h"
