/* gfortran backend interface
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
   Free Software Foundation, Inc.
   Contributed by Paul Brook.

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

/* f95-lang.c-- GCC backend interface stuff */

/* declare required prototypes: */

#include "config.h"
#include "system.h"
#include "ansidecl.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "gimple.h"
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
/* For gfc_maybe_initialize_eh.  */
#include "libfuncs.h"
#include "expr.h"
#include "except.h"

#include "gfortran.h"
#include "cpp.h"
#include "trans.h"
#include "trans-types.h"
#include "trans-const.h"

/* Language-dependent contents of an identifier.  */

struct GTY(())
lang_identifier {
  struct tree_identifier common;
};

/* The resulting tree type.  */

union GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
     chain_next ("(union lang_tree_node *)TREE_CHAIN (&%h.generic)")))

lang_tree_node {
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY((tag ("1"))) identifier;
};

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct GTY(())
language_function {
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
void do_function_end (void);
int global_bindings_p (void);
static void clear_binding_stack (void);
static void gfc_be_parse_file (int);
static alias_set_type gfc_get_alias_set (tree);
static void gfc_init_ts (void);

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
#undef LANG_HOOKS_GET_ALIAS_SET
#undef LANG_HOOKS_INIT_TS
#undef LANG_HOOKS_OMP_PRIVATIZE_BY_REFERENCE
#undef LANG_HOOKS_OMP_PREDETERMINED_SHARING
#undef LANG_HOOKS_OMP_REPORT_DECL
#undef LANG_HOOKS_OMP_CLAUSE_DEFAULT_CTOR
#undef LANG_HOOKS_OMP_CLAUSE_COPY_CTOR
#undef LANG_HOOKS_OMP_CLAUSE_ASSIGN_OP
#undef LANG_HOOKS_OMP_CLAUSE_DTOR
#undef LANG_HOOKS_OMP_DISREGARD_VALUE_EXPR
#undef LANG_HOOKS_OMP_PRIVATE_DEBUG_CLAUSE
#undef LANG_HOOKS_OMP_PRIVATE_OUTER_REF
#undef LANG_HOOKS_OMP_FIRSTPRIVATIZE_TYPE_SIZES
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GET_ARRAY_DESCR_INFO

/* Define lang hooks.  */
#define LANG_HOOKS_NAME                 "GNU Fortran"
#define LANG_HOOKS_INIT                 gfc_init
#define LANG_HOOKS_FINISH               gfc_finish
#define LANG_HOOKS_INIT_OPTIONS         gfc_init_options
#define LANG_HOOKS_HANDLE_OPTION        gfc_handle_option
#define LANG_HOOKS_POST_OPTIONS		gfc_post_options
#define LANG_HOOKS_PRINT_IDENTIFIER     gfc_print_identifier
#define LANG_HOOKS_PARSE_FILE           gfc_be_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE	gfc_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE	gfc_type_for_size
#define LANG_HOOKS_GET_ALIAS_SET	gfc_get_alias_set
#define LANG_HOOKS_INIT_TS		gfc_init_ts
#define LANG_HOOKS_OMP_PRIVATIZE_BY_REFERENCE	gfc_omp_privatize_by_reference
#define LANG_HOOKS_OMP_PREDETERMINED_SHARING	gfc_omp_predetermined_sharing
#define LANG_HOOKS_OMP_REPORT_DECL		gfc_omp_report_decl
#define LANG_HOOKS_OMP_CLAUSE_DEFAULT_CTOR	gfc_omp_clause_default_ctor
#define LANG_HOOKS_OMP_CLAUSE_COPY_CTOR		gfc_omp_clause_copy_ctor
#define LANG_HOOKS_OMP_CLAUSE_ASSIGN_OP		gfc_omp_clause_assign_op
#define LANG_HOOKS_OMP_CLAUSE_DTOR		gfc_omp_clause_dtor
#define LANG_HOOKS_OMP_DISREGARD_VALUE_EXPR	gfc_omp_disregard_value_expr
#define LANG_HOOKS_OMP_PRIVATE_DEBUG_CLAUSE	gfc_omp_private_debug_clause
#define LANG_HOOKS_OMP_PRIVATE_OUTER_REF	gfc_omp_private_outer_ref
#define LANG_HOOKS_OMP_FIRSTPRIVATIZE_TYPE_SIZES \
  gfc_omp_firstprivatize_type_sizes
#define LANG_HOOKS_BUILTIN_FUNCTION          gfc_builtin_function
#define LANG_HOOKS_GET_ARRAY_DESCR_INFO	     gfc_get_array_descr_info

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#define NULL_BINDING_LEVEL (struct binding_level *) NULL

/* A chain of binding_level structures awaiting reuse.  */

static GTY(()) struct binding_level *free_binding_level;

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
tree *ridpointers = NULL;

/* True means we've initialized exception handling.  */
bool gfc_eh_initialized_p;


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
        return fold_build1 (NOP_EXPR,
			    boolean_type_node, TREE_OPERAND (expr, 0));
      else
        return fold_build1 (NOP_EXPR, boolean_type_node, expr);

    case INTEGER_TYPE:
      if (TREE_CODE (expr) == INTEGER_CST)
	return integer_zerop (expr) ? boolean_false_node : boolean_true_node;
      else
        return fold_build2 (NE_EXPR, boolean_type_node, expr,
			    build_int_cst (TREE_TYPE (expr), 0));

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

  /* Tell the frontend about any errors.  */
  gfc_get_errors (&warnings, &errors);
  errorcount += errors;
  warningcount += warnings;

  clear_binding_stack ();
}


/* Initialize everything.  */

static bool
gfc_init (void)
{
  if (!gfc_cpp_enabled ())
    {
      linemap_add (line_table, LC_ENTER, false, gfc_source_file, 1);
      linemap_add (line_table, LC_RENAME, false, "<built-in>", 0);
    }
  else
    gfc_cpp_init_0 ();

  gfc_init_decl_processing ();
  gfc_static_ctors = NULL_TREE;

  if (gfc_cpp_enabled ())
    gfc_cpp_init ();

  gfc_init_1 ();

  if (gfc_new_file () != SUCCESS)
    fatal_error ("can't open input file: %s", gfc_source_file);

  return true;
}


static void
gfc_finish (void)
{
  gfc_cpp_done ();
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

/* For each binding contour we allocate a binding_level structure which
   records the entities defined or declared in that contour.  Contours
   include:

        the global one
        one for each subprogram definition
        one for each compound statement (declare block)

   Binding contours are used to create GCC tree BLOCK nodes.  */

struct GTY(())
binding_level {
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
    /* This is the top level block of a function. */
    DECL_INITIAL (current_function_decl) = block_node;
  else if (current_binding_level == global_binding_level)
    /* When using gfc_start_block/gfc_finish_block from middle-end hooks,
       don't add newly created BLOCKs as subblocks of global_binding_level.  */
    ;
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
clear_binding_stack (void)
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
  /* x86_64 mingw32 has a sizetype of "unsigned long long", most other hosts
     have a sizetype of "unsigned long". Therefore choose the correct size
     in mostly target independent way.  */
  if (TYPE_MODE (long_unsigned_type_node) == ptr_mode)
    set_sizetype (long_unsigned_type_node);
  else if (TYPE_MODE (long_long_unsigned_type_node) == ptr_mode)
    set_sizetype (long_long_unsigned_type_node);
  else
    set_sizetype (long_unsigned_type_node);
  build_common_tree_nodes_2 (0);
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  /* Set up F95 type nodes.  */
  gfc_init_kinds ();
  gfc_init_types ();
}


/* Return the typed-based alias set for T, which may be an expression
   or a type.  Return -1 if we don't do anything special.  */

static alias_set_type
gfc_get_alias_set (tree t)
{
  tree u;

  /* Permit type-punning when accessing an EQUIVALENCEd variable or
     mixed type entry master's return value.  */
  for (u = t; handled_component_p (u); u = TREE_OPERAND (u, 0))
    if (TREE_CODE (u) == COMPONENT_REF
	&& TREE_CODE (TREE_TYPE (TREE_OPERAND (u, 0))) == UNION_TYPE)
      return 0;

  return -1;
}


/* press the big red button - garbage (ggc) collection is on */

int ggc_p = 1;

/* Builtin function initialization.  */

tree
gfc_builtin_function (tree decl)
{
  make_decl_rtl (decl);
  pushdecl (decl);
  return decl;
}


static void
gfc_define_builtin (const char *name,
		    tree type,
		    int code,
		    const char *library_name,
		    bool const_p)
{
  tree decl;

  decl = add_builtin_function (name, type, code, BUILT_IN_NORMAL,
			       library_name, NULL_TREE);
  if (const_p)
    TREE_READONLY (decl) = 1;
  TREE_NOTHROW (decl) = 1;

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
build_builtin_fntypes (tree *fntype, tree type)
{
  tree tmp;

  /* type (*) (type) */
  tmp = tree_cons (NULL_TREE, type, void_list_node);
  fntype[0] = build_function_type (type, tmp);
  /* type (*) (type, type) */
  tmp = tree_cons (NULL_TREE, type, tmp);
  fntype[1] = build_function_type (type, tmp);
  /* type (*) (int, type) */
  tmp = tree_cons (NULL_TREE, integer_type_node, void_list_node);
  tmp = tree_cons (NULL_TREE, type, tmp);
  fntype[2] = build_function_type (type, tmp);
  /* type (*) (void) */
  fntype[3] = build_function_type (type, void_list_node);
  /* type (*) (type, &int) */
  tmp = tree_cons (NULL_TREE, type, void_list_node);
  tmp = tree_cons (NULL_TREE, build_pointer_type (integer_type_node), tmp);
  fntype[4] = build_function_type (type, tmp);
  /* type (*) (type, int) */
  tmp = tree_cons (NULL_TREE, type, void_list_node);
  tmp = tree_cons (NULL_TREE, integer_type_node, tmp);
  fntype[5] = build_function_type (type, tmp);
}


static tree
builtin_type_for_size (int size, bool unsignedp)
{
  tree type = lang_hooks.types.type_for_size (size, unsignedp);
  return type ? type : error_mark_node;
}

/* Initialization of builtin function nodes.  */

static void
gfc_init_builtin_functions (void)
{
  enum builtin_type
  {
#define DEF_PRIMITIVE_TYPE(NAME, VALUE) NAME,
#define DEF_FUNCTION_TYPE_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) NAME,
#define DEF_FUNCTION_TYPE_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6) NAME,
#define DEF_FUNCTION_TYPE_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_POINTER_TYPE(NAME, TYPE) NAME,
#include "types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_7
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_POINTER_TYPE
    BT_LAST
  };
  typedef enum builtin_type builtin_type;
  enum
  {
    /* So far we need just these 2 attribute types.  */
    ATTR_NOTHROW_LIST,
    ATTR_CONST_NOTHROW_LIST
  };

  tree mfunc_float[6];
  tree mfunc_double[6];
  tree mfunc_longdouble[6];
  tree mfunc_cfloat[6];
  tree mfunc_cdouble[6];
  tree mfunc_clongdouble[6];
  tree func_cfloat_float, func_float_cfloat;
  tree func_cdouble_double, func_double_cdouble;
  tree func_clongdouble_longdouble, func_longdouble_clongdouble;
  tree func_float_floatp_floatp;
  tree func_double_doublep_doublep;
  tree func_longdouble_longdoublep_longdoublep;
  tree ftype, ptype;
  tree tmp, type;
  tree builtin_types[(int) BT_LAST + 1];

  build_builtin_fntypes (mfunc_float, float_type_node);
  build_builtin_fntypes (mfunc_double, double_type_node);
  build_builtin_fntypes (mfunc_longdouble, long_double_type_node);
  build_builtin_fntypes (mfunc_cfloat, complex_float_type_node);
  build_builtin_fntypes (mfunc_cdouble, complex_double_type_node);
  build_builtin_fntypes (mfunc_clongdouble, complex_long_double_type_node);

  tmp = tree_cons (NULL_TREE, complex_float_type_node, void_list_node);
  func_cfloat_float = build_function_type (float_type_node, tmp);

  tmp = tree_cons (NULL_TREE, float_type_node, void_list_node);
  func_float_cfloat = build_function_type (complex_float_type_node, tmp);

  tmp = tree_cons (NULL_TREE, complex_double_type_node, void_list_node);
  func_cdouble_double = build_function_type (double_type_node, tmp);

  tmp = tree_cons (NULL_TREE, double_type_node, void_list_node);
  func_double_cdouble = build_function_type (complex_double_type_node, tmp);

  tmp = tree_cons (NULL_TREE, complex_long_double_type_node, void_list_node);
  func_clongdouble_longdouble =
    build_function_type (long_double_type_node, tmp);

  tmp = tree_cons (NULL_TREE, long_double_type_node, void_list_node);
  func_longdouble_clongdouble =
    build_function_type (complex_long_double_type_node, tmp);

  ptype = build_pointer_type (float_type_node);
  tmp = tree_cons (NULL_TREE, float_type_node,
		   tree_cons (NULL_TREE, ptype,
		   	      tree_cons (NULL_TREE, ptype, void_list_node)));
  func_float_floatp_floatp =
    build_function_type (void_type_node, tmp);

  ptype = build_pointer_type (double_type_node);
  tmp = tree_cons (NULL_TREE, double_type_node,
		   tree_cons (NULL_TREE, ptype,
		   	      tree_cons (NULL_TREE, ptype, void_list_node)));
  func_double_doublep_doublep =
    build_function_type (void_type_node, tmp);

  ptype = build_pointer_type (long_double_type_node);
  tmp = tree_cons (NULL_TREE, long_double_type_node,
		   tree_cons (NULL_TREE, ptype,
		   	      tree_cons (NULL_TREE, ptype, void_list_node)));
  func_longdouble_longdoublep_longdoublep =
    build_function_type (void_type_node, tmp);

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
 
  gfc_define_builtin ("__builtin_nextafterl", mfunc_longdouble[1], 
		      BUILT_IN_NEXTAFTERL, "nextafterl", true);
  gfc_define_builtin ("__builtin_nextafter", mfunc_double[1], 
		      BUILT_IN_NEXTAFTER, "nextafter", true);
  gfc_define_builtin ("__builtin_nextafterf", mfunc_float[1], 
		      BUILT_IN_NEXTAFTERF, "nextafterf", true);
 
  gfc_define_builtin ("__builtin_frexpl", mfunc_longdouble[4], 
		      BUILT_IN_FREXPL, "frexpl", false);
  gfc_define_builtin ("__builtin_frexp", mfunc_double[4], 
		      BUILT_IN_FREXP, "frexp", false);
  gfc_define_builtin ("__builtin_frexpf", mfunc_float[4], 
		      BUILT_IN_FREXPF, "frexpf", false);
 
  gfc_define_builtin ("__builtin_fabsl", mfunc_longdouble[0], 
		      BUILT_IN_FABSL, "fabsl", true);
  gfc_define_builtin ("__builtin_fabs", mfunc_double[0], 
		      BUILT_IN_FABS, "fabs", true);
  gfc_define_builtin ("__builtin_fabsf", mfunc_float[0], 
		      BUILT_IN_FABSF, "fabsf", true);
 
  gfc_define_builtin ("__builtin_scalbnl", mfunc_longdouble[5], 
		      BUILT_IN_SCALBNL, "scalbnl", true);
  gfc_define_builtin ("__builtin_scalbn", mfunc_double[5], 
		      BUILT_IN_SCALBN, "scalbn", true);
  gfc_define_builtin ("__builtin_scalbnf", mfunc_float[5], 
		      BUILT_IN_SCALBNF, "scalbnf", true);
 
  gfc_define_builtin ("__builtin_fmodl", mfunc_longdouble[1], 
		      BUILT_IN_FMODL, "fmodl", true);
  gfc_define_builtin ("__builtin_fmod", mfunc_double[1], 
		      BUILT_IN_FMOD, "fmod", true);
  gfc_define_builtin ("__builtin_fmodf", mfunc_float[1], 
		      BUILT_IN_FMODF, "fmodf", true);

  gfc_define_builtin ("__builtin_huge_vall", mfunc_longdouble[3], 
		      BUILT_IN_HUGE_VALL, "__builtin_huge_vall", true);
  gfc_define_builtin ("__builtin_huge_val", mfunc_double[3], 
		      BUILT_IN_HUGE_VAL, "__builtin_huge_val", true);
  gfc_define_builtin ("__builtin_huge_valf", mfunc_float[3], 
		      BUILT_IN_HUGE_VALF, "__builtin_huge_valf", true);

  /* lround{f,,l} and llround{f,,l} */
  type = tree_cons (NULL_TREE, float_type_node, void_list_node);
  tmp = build_function_type (long_integer_type_node, type); 
  gfc_define_builtin ("__builtin_lroundf", tmp, BUILT_IN_LROUNDF,
		      "lroundf", true);
  tmp = build_function_type (long_long_integer_type_node, type); 
  gfc_define_builtin ("__builtin_llroundf", tmp, BUILT_IN_LLROUNDF,
		      "llroundf", true);

  type = tree_cons (NULL_TREE, double_type_node, void_list_node);
  tmp = build_function_type (long_integer_type_node, type); 
  gfc_define_builtin ("__builtin_lround", tmp, BUILT_IN_LROUND,
		      "lround", true);
  tmp = build_function_type (long_long_integer_type_node, type); 
  gfc_define_builtin ("__builtin_llround", tmp, BUILT_IN_LLROUND,
		      "llround", true);

  type = tree_cons (NULL_TREE, long_double_type_node, void_list_node);
  tmp = build_function_type (long_integer_type_node, type); 
  gfc_define_builtin ("__builtin_lroundl", tmp, BUILT_IN_LROUNDL,
		      "lroundl", true);
  tmp = build_function_type (long_long_integer_type_node, type); 
  gfc_define_builtin ("__builtin_llroundl", tmp, BUILT_IN_LLROUNDL,
		      "llroundl", true);

  /* These are used to implement the ** operator.  */
  gfc_define_builtin ("__builtin_powl", mfunc_longdouble[1], 
		      BUILT_IN_POWL, "powl", true);
  gfc_define_builtin ("__builtin_pow", mfunc_double[1], 
		      BUILT_IN_POW, "pow", true);
  gfc_define_builtin ("__builtin_powf", mfunc_float[1], 
		      BUILT_IN_POWF, "powf", true);
  gfc_define_builtin ("__builtin_cpowl", mfunc_clongdouble[1], 
		      BUILT_IN_CPOWL, "cpowl", true);
  gfc_define_builtin ("__builtin_cpow", mfunc_cdouble[1], 
		      BUILT_IN_CPOW, "cpow", true);
  gfc_define_builtin ("__builtin_cpowf", mfunc_cfloat[1], 
		      BUILT_IN_CPOWF, "cpowf", true);
  gfc_define_builtin ("__builtin_powil", mfunc_longdouble[2], 
		      BUILT_IN_POWIL, "powil", true);
  gfc_define_builtin ("__builtin_powi", mfunc_double[2], 
		      BUILT_IN_POWI, "powi", true);
  gfc_define_builtin ("__builtin_powif", mfunc_float[2], 
		      BUILT_IN_POWIF, "powif", true);


  if (TARGET_C99_FUNCTIONS)
    {
      gfc_define_builtin ("__builtin_cbrtl", mfunc_longdouble[0],
			  BUILT_IN_CBRTL, "cbrtl", true);
      gfc_define_builtin ("__builtin_cbrt", mfunc_double[0],
			  BUILT_IN_CBRT, "cbrt", true);
      gfc_define_builtin ("__builtin_cbrtf", mfunc_float[0],
			  BUILT_IN_CBRTF, "cbrtf", true);
      gfc_define_builtin ("__builtin_cexpil", func_longdouble_clongdouble, 
		          BUILT_IN_CEXPIL, "cexpil", true);
      gfc_define_builtin ("__builtin_cexpi", func_double_cdouble,
		          BUILT_IN_CEXPI, "cexpi", true);
      gfc_define_builtin ("__builtin_cexpif", func_float_cfloat,
		          BUILT_IN_CEXPIF, "cexpif", true);
    }

  if (TARGET_HAS_SINCOS)
    {
      gfc_define_builtin ("__builtin_sincosl",
			  func_longdouble_longdoublep_longdoublep,
		          BUILT_IN_SINCOSL, "sincosl", false);
      gfc_define_builtin ("__builtin_sincos", func_double_doublep_doublep,
		          BUILT_IN_SINCOS, "sincos", false);
      gfc_define_builtin ("__builtin_sincosf", func_float_floatp_floatp,
		          BUILT_IN_SINCOSF, "sincosf", false);
    }

  /* For LEADZ / TRAILZ.  */
  tmp = tree_cons (NULL_TREE, unsigned_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_clz", ftype, BUILT_IN_CLZ,
		      "__builtin_clz", true);

  tmp = tree_cons (NULL_TREE, long_unsigned_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_clzl", ftype, BUILT_IN_CLZL,
		      "__builtin_clzl", true);

  tmp = tree_cons (NULL_TREE, long_long_unsigned_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_clzll", ftype, BUILT_IN_CLZLL,
		      "__builtin_clzll", true);

  tmp = tree_cons (NULL_TREE, unsigned_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_ctz", ftype, BUILT_IN_CTZ,
		      "__builtin_ctz", true);

  tmp = tree_cons (NULL_TREE, long_unsigned_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_ctzl", ftype, BUILT_IN_CTZL,
		      "__builtin_ctzl", true);

  tmp = tree_cons (NULL_TREE, long_long_unsigned_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_ctzll", ftype, BUILT_IN_CTZLL,
		      "__builtin_ctzll", true);

  /* Other builtin functions we use.  */

  tmp = tree_cons (NULL_TREE, long_integer_type_node, void_list_node);
  tmp = tree_cons (NULL_TREE, long_integer_type_node, tmp);
  ftype = build_function_type (long_integer_type_node, tmp);
  gfc_define_builtin ("__builtin_expect", ftype, BUILT_IN_EXPECT,
		      "__builtin_expect", true);

  tmp = tree_cons (NULL_TREE, pvoid_type_node, void_list_node);
  ftype = build_function_type (void_type_node, tmp);
  gfc_define_builtin ("__builtin_free", ftype, BUILT_IN_FREE,
		      "free", false);

  tmp = tree_cons (NULL_TREE, size_type_node, void_list_node);
  ftype = build_function_type (pvoid_type_node, tmp);
  gfc_define_builtin ("__builtin_malloc", ftype, BUILT_IN_MALLOC,
		      "malloc", false);
  DECL_IS_MALLOC (built_in_decls[BUILT_IN_MALLOC]) = 1;

  tmp = tree_cons (NULL_TREE, pvoid_type_node, void_list_node);
  tmp = tree_cons (NULL_TREE, size_type_node, tmp);
  ftype = build_function_type (pvoid_type_node, tmp);
  gfc_define_builtin ("__builtin_realloc", ftype, BUILT_IN_REALLOC,
		      "realloc", false);

  tmp = tree_cons (NULL_TREE, void_type_node, void_list_node);
  ftype = build_function_type (integer_type_node, tmp);
  gfc_define_builtin ("__builtin_isnan", ftype, BUILT_IN_ISNAN,
		      "__builtin_isnan", true);

#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) \
  builtin_types[(int) ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN)		\
  builtin_types[(int) ENUM]				\
    = build_function_type (builtin_types[(int) RETURN],	\
			   void_list_node);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1)				\
  builtin_types[(int) ENUM]						\
    = build_function_type (builtin_types[(int) RETURN],			\
			   tree_cons (NULL_TREE,			\
				      builtin_types[(int) ARG1],	\
				      void_list_node));
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2)	\
  builtin_types[(int) ENUM]				\
    = build_function_type				\
      (builtin_types[(int) RETURN],			\
       tree_cons (NULL_TREE,				\
		  builtin_types[(int) ARG1],		\
		  tree_cons (NULL_TREE,			\
			     builtin_types[(int) ARG2],	\
			     void_list_node)));
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3)		 \
  builtin_types[(int) ENUM]						 \
    = build_function_type						 \
      (builtin_types[(int) RETURN],					 \
       tree_cons (NULL_TREE,						 \
		  builtin_types[(int) ARG1],				 \
		  tree_cons (NULL_TREE,					 \
			     builtin_types[(int) ARG2],			 \
			     tree_cons (NULL_TREE,			 \
					builtin_types[(int) ARG3],	 \
					void_list_node))));
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4)	\
  builtin_types[(int) ENUM]						\
    = build_function_type						\
      (builtin_types[(int) RETURN],					\
       tree_cons (NULL_TREE,						\
		  builtin_types[(int) ARG1],				\
		  tree_cons (NULL_TREE,					\
			     builtin_types[(int) ARG2],			\
			     tree_cons					\
			     (NULL_TREE,				\
			      builtin_types[(int) ARG3],		\
			      tree_cons (NULL_TREE,			\
					 builtin_types[(int) ARG4],	\
					 void_list_node)))));
#define DEF_FUNCTION_TYPE_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5)	\
  builtin_types[(int) ENUM]						\
    = build_function_type						\
      (builtin_types[(int) RETURN],					\
       tree_cons (NULL_TREE,						\
		  builtin_types[(int) ARG1],				\
		  tree_cons (NULL_TREE,					\
			     builtin_types[(int) ARG2],			\
			     tree_cons					\
			     (NULL_TREE,				\
			      builtin_types[(int) ARG3],		\
			      tree_cons (NULL_TREE,			\
					 builtin_types[(int) ARG4],	\
					 tree_cons (NULL_TREE,		\
					      builtin_types[(int) ARG5],\
					      void_list_node))))));
#define DEF_FUNCTION_TYPE_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6)					\
  builtin_types[(int) ENUM]						\
    = build_function_type						\
      (builtin_types[(int) RETURN],					\
       tree_cons (NULL_TREE,						\
		  builtin_types[(int) ARG1],				\
		  tree_cons (NULL_TREE,					\
			     builtin_types[(int) ARG2],			\
			     tree_cons					\
			     (NULL_TREE,				\
			      builtin_types[(int) ARG3],		\
			      tree_cons					\
			      (NULL_TREE,				\
			       builtin_types[(int) ARG4],		\
			       tree_cons (NULL_TREE,			\
					 builtin_types[(int) ARG5],	\
					 tree_cons (NULL_TREE,		\
					      builtin_types[(int) ARG6],\
					      void_list_node)))))));
#define DEF_FUNCTION_TYPE_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7)					\
  builtin_types[(int) ENUM]						\
    = build_function_type						\
      (builtin_types[(int) RETURN],					\
       tree_cons (NULL_TREE,						\
		  builtin_types[(int) ARG1],				\
		  tree_cons (NULL_TREE,					\
			     builtin_types[(int) ARG2],			\
			     tree_cons					\
			     (NULL_TREE,				\
			      builtin_types[(int) ARG3],		\
			      tree_cons					\
			      (NULL_TREE,				\
			       builtin_types[(int) ARG4],		\
			       tree_cons (NULL_TREE,			\
					 builtin_types[(int) ARG5],	\
					 tree_cons (NULL_TREE,		\
					      builtin_types[(int) ARG6],\
					 tree_cons (NULL_TREE,		\
					      builtin_types[(int) ARG6], \
					      void_list_node))))))));
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN)				\
  builtin_types[(int) ENUM]						\
    = build_function_type (builtin_types[(int) RETURN], NULL_TREE);
#define DEF_POINTER_TYPE(ENUM, TYPE)			\
  builtin_types[(int) ENUM]				\
    = build_pointer_type (builtin_types[(int) TYPE]);
#include "types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_POINTER_TYPE
  builtin_types[(int) BT_LAST] = NULL_TREE;

  /* Initialize synchronization builtins.  */
#undef DEF_SYNC_BUILTIN
#define DEF_SYNC_BUILTIN(code, name, type, attr) \
    gfc_define_builtin (name, builtin_types[type], code, name, \
			attr == ATTR_CONST_NOTHROW_LIST);
#include "../sync-builtins.def"
#undef DEF_SYNC_BUILTIN

  if (gfc_option.flag_openmp || flag_tree_parallelize_loops)
    {
#undef DEF_GOMP_BUILTIN
#define DEF_GOMP_BUILTIN(code, name, type, attr) \
      gfc_define_builtin ("__builtin_" name, builtin_types[type], \
			  code, name, attr == ATTR_CONST_NOTHROW_LIST);
#include "../omp-builtins.def"
#undef DEF_GOMP_BUILTIN
    }

  gfc_define_builtin ("__builtin_trap", builtin_types[BT_FN_VOID],
		      BUILT_IN_TRAP, NULL, false);
  TREE_THIS_VOLATILE (built_in_decls[BUILT_IN_TRAP]) = 1;

  gfc_define_builtin ("__emutls_get_address",
		      builtin_types[BT_FN_PTR_PTR], BUILT_IN_EMUTLS_GET_ADDRESS,
		      "__emutls_get_address", true);
  gfc_define_builtin ("__emutls_register_common",
		      builtin_types[BT_FN_VOID_PTR_WORD_WORD_PTR],
		      BUILT_IN_EMUTLS_REGISTER_COMMON,
		      "__emutls_register_common", false);

  build_common_builtin_nodes ();
  targetm.init_builtins ();
}

#undef DEFINE_MATH_BUILTIN_C
#undef DEFINE_MATH_BUILTIN

static void
gfc_init_ts (void)
{
  tree_contains_struct[NAMESPACE_DECL][TS_DECL_NON_COMMON] = 1;
  tree_contains_struct[NAMESPACE_DECL][TS_DECL_WITH_VIS] = 1;
  tree_contains_struct[NAMESPACE_DECL][TS_DECL_WRTL] = 1;
  tree_contains_struct[NAMESPACE_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[NAMESPACE_DECL][TS_DECL_MINIMAL] = 1;
}

void
gfc_maybe_initialize_eh (void)
{
  if (!flag_exceptions || gfc_eh_initialized_p)
    return;

  gfc_eh_initialized_p = true;
  using_eh_for_cleanups ();
}


#include "gt-fortran-f95-lang.h"
#include "gtype-fortran.h"
