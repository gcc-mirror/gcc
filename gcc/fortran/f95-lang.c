/* gfortran backend interface
   Copyright (C) 2000-2021 Free Software Foundation, Inc.
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
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "tree.h"
#include "gfortran.h"
#include "trans.h"
#include "stringpool.h"
#include "diagnostic.h" /* For errorcount/warningcount */
#include "langhooks.h"
#include "langhooks-def.h"
#include "toplev.h"
#include "debug.h"
#include "cpp.h"
#include "trans-types.h"
#include "trans-const.h"

/* Language-dependent contents of an identifier.  */

struct GTY(())
lang_identifier {
  struct tree_identifier common;
};

/* The resulting tree type.  */

union GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
     chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
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

static void gfc_init_decl_processing (void);
static void gfc_init_builtin_functions (void);
static bool global_bindings_p (void);

/* Each front end provides its own.  */
static bool gfc_init (void);
static void gfc_finish (void);
static void gfc_be_parse_file (void);
static void gfc_init_ts (void);
static tree gfc_builtin_function (tree);

/* Handle an "omp declare target" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
gfc_handle_omp_declare_target_attribute (tree *, tree, tree, int, bool *)
{
  return NULL_TREE;
}

/* Table of valid Fortran attributes.  */
static const struct attribute_spec gfc_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "omp declare target", 0, -1, true,  false, false, false,
    gfc_handle_omp_declare_target_attribute, NULL },
  { "omp declare target link", 0, 0, true,  false, false, false,
    gfc_handle_omp_declare_target_attribute, NULL },
  { "oacc function", 0, -1, true,  false, false, false,
    gfc_handle_omp_declare_target_attribute, NULL },
  { NULL,		  0, 0, false, false, false, false, NULL, NULL }
};

#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_FINISH
#undef LANG_HOOKS_OPTION_LANG_MASK
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#undef LANG_HOOKS_INIT_OPTIONS
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_MARK_ADDRESSABLE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_INIT_TS
#undef LANG_HOOKS_OMP_ARRAY_DATA
#undef LANG_HOOKS_OMP_IS_ALLOCATABLE_OR_PTR
#undef LANG_HOOKS_OMP_CHECK_OPTIONAL_ARGUMENT
#undef LANG_HOOKS_OMP_PRIVATIZE_BY_REFERENCE
#undef LANG_HOOKS_OMP_PREDETERMINED_SHARING
#undef LANG_HOOKS_OMP_PREDETERMINED_MAPPING
#undef LANG_HOOKS_OMP_REPORT_DECL
#undef LANG_HOOKS_OMP_CLAUSE_DEFAULT_CTOR
#undef LANG_HOOKS_OMP_CLAUSE_COPY_CTOR
#undef LANG_HOOKS_OMP_CLAUSE_ASSIGN_OP
#undef LANG_HOOKS_OMP_CLAUSE_LINEAR_CTOR
#undef LANG_HOOKS_OMP_CLAUSE_DTOR
#undef LANG_HOOKS_OMP_FINISH_CLAUSE
#undef LANG_HOOKS_OMP_ALLOCATABLE_P
#undef LANG_HOOKS_OMP_SCALAR_TARGET_P
#undef LANG_HOOKS_OMP_SCALAR_P
#undef LANG_HOOKS_OMP_DISREGARD_VALUE_EXPR
#undef LANG_HOOKS_OMP_PRIVATE_DEBUG_CLAUSE
#undef LANG_HOOKS_OMP_PRIVATE_OUTER_REF
#undef LANG_HOOKS_OMP_FIRSTPRIVATIZE_TYPE_SIZES
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GET_ARRAY_DESCR_INFO
#undef LANG_HOOKS_ATTRIBUTE_TABLE

/* Define lang hooks.  */
#define LANG_HOOKS_NAME                 "GNU Fortran"
#define LANG_HOOKS_INIT                 gfc_init
#define LANG_HOOKS_FINISH               gfc_finish
#define LANG_HOOKS_OPTION_LANG_MASK	gfc_option_lang_mask
#define LANG_HOOKS_INIT_OPTIONS_STRUCT  gfc_init_options_struct
#define LANG_HOOKS_INIT_OPTIONS         gfc_init_options
#define LANG_HOOKS_HANDLE_OPTION        gfc_handle_option
#define LANG_HOOKS_POST_OPTIONS		gfc_post_options
#define LANG_HOOKS_PARSE_FILE           gfc_be_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE	gfc_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE	gfc_type_for_size
#define LANG_HOOKS_INIT_TS		gfc_init_ts
#define LANG_HOOKS_OMP_ARRAY_DATA		gfc_omp_array_data
#define LANG_HOOKS_OMP_IS_ALLOCATABLE_OR_PTR	gfc_omp_is_allocatable_or_ptr
#define LANG_HOOKS_OMP_CHECK_OPTIONAL_ARGUMENT	gfc_omp_check_optional_argument
#define LANG_HOOKS_OMP_PRIVATIZE_BY_REFERENCE	gfc_omp_privatize_by_reference
#define LANG_HOOKS_OMP_PREDETERMINED_SHARING	gfc_omp_predetermined_sharing
#define LANG_HOOKS_OMP_PREDETERMINED_MAPPING	gfc_omp_predetermined_mapping
#define LANG_HOOKS_OMP_REPORT_DECL		gfc_omp_report_decl
#define LANG_HOOKS_OMP_CLAUSE_DEFAULT_CTOR	gfc_omp_clause_default_ctor
#define LANG_HOOKS_OMP_CLAUSE_COPY_CTOR		gfc_omp_clause_copy_ctor
#define LANG_HOOKS_OMP_CLAUSE_ASSIGN_OP		gfc_omp_clause_assign_op
#define LANG_HOOKS_OMP_CLAUSE_LINEAR_CTOR	gfc_omp_clause_linear_ctor
#define LANG_HOOKS_OMP_CLAUSE_DTOR		gfc_omp_clause_dtor
#define LANG_HOOKS_OMP_FINISH_CLAUSE		gfc_omp_finish_clause
#define LANG_HOOKS_OMP_ALLOCATABLE_P		gfc_omp_allocatable_p
#define LANG_HOOKS_OMP_SCALAR_P			gfc_omp_scalar_p
#define LANG_HOOKS_OMP_SCALAR_TARGET_P		gfc_omp_scalar_target_p
#define LANG_HOOKS_OMP_DISREGARD_VALUE_EXPR	gfc_omp_disregard_value_expr
#define LANG_HOOKS_OMP_PRIVATE_DEBUG_CLAUSE	gfc_omp_private_debug_clause
#define LANG_HOOKS_OMP_PRIVATE_OUTER_REF	gfc_omp_private_outer_ref
#define LANG_HOOKS_OMP_FIRSTPRIVATIZE_TYPE_SIZES \
  gfc_omp_firstprivatize_type_sizes
#define LANG_HOOKS_BUILTIN_FUNCTION	gfc_builtin_function
#define LANG_HOOKS_GET_ARRAY_DESCR_INFO	gfc_get_array_descr_info
#define LANG_HOOKS_ATTRIBUTE_TABLE	gfc_attribute_table

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#define NULL_BINDING_LEVEL (struct binding_level *) NULL

/* A chain of binding_level structures awaiting reuse.  */

static GTY(()) struct binding_level *free_binding_level;

/* True means we've initialized exception handling.  */
static bool gfc_eh_initialized_p;

/* The current translation unit.  */
static GTY(()) tree current_translation_unit;


static void
gfc_create_decls (void)
{
  /* GCC builtins.  */
  gfc_init_builtin_functions ();

  /* Runtime/IO library functions.  */
  gfc_build_builtin_function_decls ();

  gfc_init_constants ();

  /* Build our translation-unit decl.  */
  current_translation_unit
    = build_translation_unit_decl (get_identifier (main_input_filename));
  debug_hooks->register_main_translation_unit (current_translation_unit);
}


static void
gfc_be_parse_file (void)
{
  gfc_create_decls ();
  gfc_parse_file ();
  gfc_generate_constructors ();

  /* Clear the binding level stack.  */
  while (!global_bindings_p ())
    poplevel (0, 0);

  /* Finalize all of the globals.

     Emulated tls lowering needs to see all TLS variables before we
     call finalize_compilation_unit.  The C/C++ front ends manage this
     by calling decl_rest_of_compilation on each global and static
     variable as they are seen.  The Fortran front end waits until
     here.  */
  for (tree decl = getdecls (); decl ; decl = DECL_CHAIN (decl))
    rest_of_decl_compilation (decl, true, true);

  /* Switch to the default tree diagnostics here, because there may be
     diagnostics before gfc_finish().  */
  gfc_diagnostics_finish ();

  global_decl_processing ();
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

  /* Calls exit in case of a fail. */
  gfc_new_file ();

  if (flag_preprocess_only)
    return false;

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
     through the DECL_CHAIN field.  */
  tree names;
  /* For each level (except the global one), a chain of BLOCK nodes for all
     the levels that were entered and exited one level down from this one.  */
  tree blocks;
  /* The binding level containing this one (the enclosing binding level).  */
  struct binding_level *level_chain;
  /* True if nreverse has been already called on names; if false, names
     are ordered from newest declaration to oldest one.  */
  bool reversed;
};

/* The binding level currently in effect.  */
static GTY(()) struct binding_level *current_binding_level = NULL;

/* The outermost binding level. This binding level is created when the
   compiler is started and it will exist through the entire compilation.  */
static GTY(()) struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */
static struct binding_level clear_binding_level = { NULL, NULL, NULL, false };


/* Return true if we are in the global binding level.  */

bool
global_bindings_p (void)
{
  return current_binding_level == global_binding_level;
}

tree
getdecls (void)
{
  if (!current_binding_level->reversed)
    {
      current_binding_level->reversed = true;
      current_binding_level->names = nreverse (current_binding_level->names);
    }
  return current_binding_level->names;
}

/* Enter a new binding level.  */

void
pushlevel (void)
{
  struct binding_level *newlevel = ggc_alloc<binding_level> ();

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
   label names.  */

tree
poplevel (int keep, int functionbody)
{
  /* Points to a BLOCK tree node. This is the BLOCK node constructed for the
     binding level that we are about to exit and which is returned by this
     routine.  */
  tree block_node = NULL_TREE;
  tree decl_chain = getdecls ();
  tree subblock_chain = current_binding_level->blocks;
  tree subblock_node;

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */
  if (keep || functionbody)
    block_node = build_block (keep ? decl_chain : 0, subblock_chain, 0, 0);

  /* Record the BLOCK node just built as the subblock its enclosing scope.  */
  for (subblock_node = subblock_chain; subblock_node;
       subblock_node = BLOCK_CHAIN (subblock_node))
    BLOCK_SUPERCONTEXT (subblock_node) = block_node;

  /* Clear out the meanings of the local variables of this level.  */

  for (subblock_node = decl_chain; subblock_node;
       subblock_node = DECL_CHAIN (subblock_node))
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
    /* This is the top level block of a function.  */
    DECL_INITIAL (current_function_decl) = block_node;
  else if (current_binding_level == global_binding_level)
    /* When using gfc_start_block/gfc_finish_block from middle-end hooks,
       don't add newly created BLOCKs as subblocks of global_binding_level.  */
    ;
  else if (block_node)
    {
      current_binding_level->blocks
	= block_chainon (current_binding_level->blocks, block_node);
    }

  /* If we did not make a block for the level just exited, any blocks made for
     inner levels (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks of something
     else.  */
  else if (subblock_chain)
    current_binding_level->blocks
      = block_chainon (current_binding_level->blocks, subblock_chain);
  if (block_node)
    TREE_USED (block_node) = 1;

  return block_node;
}


/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node.  */

tree
pushdecl (tree decl)
{
  if (global_bindings_p ())
    DECL_CONTEXT (decl) = current_translation_unit;
  else
    {
      /* External objects aren't nested.  For debug info insert a copy
         of the decl into the binding level.  */
      if (DECL_EXTERNAL (decl))
	{
	  tree orig = decl;
	  decl = copy_node (decl);
	  DECL_CONTEXT (orig) = NULL_TREE;
	}
      DECL_CONTEXT (decl) = current_function_decl;
    }

  /* Put the declaration on the list.  */
  DECL_CHAIN (decl) = current_binding_level->names;
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
  pushlevel ();
  global_binding_level = current_binding_level;

  /* Build common tree nodes. char_type_node is unsigned because we
     only use it for actual characters, not for INTEGER(1).  */
  build_common_tree_nodes (false);

  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  /* Set up F95 type nodes.  */
  gfc_init_kinds ();
  gfc_init_types ();
  gfc_init_c_interop_kinds ();
}


/* Builtin function initialization.  */

static tree
gfc_builtin_function (tree decl)
{
  pushdecl (decl);
  return decl;
}

/* So far we need just these 10 attribute types.  */
#define ATTR_NULL			0
#define ATTR_LEAF_LIST			(ECF_LEAF)
#define ATTR_NOTHROW_LEAF_LIST		(ECF_NOTHROW | ECF_LEAF)
#define ATTR_NOTHROW_LEAF_MALLOC_LIST	(ECF_NOTHROW | ECF_LEAF | ECF_MALLOC)
#define ATTR_CONST_NOTHROW_LEAF_LIST	(ECF_NOTHROW | ECF_LEAF | ECF_CONST)
#define ATTR_PURE_NOTHROW_LEAF_LIST	(ECF_NOTHROW | ECF_LEAF | ECF_PURE)
#define ATTR_NOTHROW_LIST		(ECF_NOTHROW)
#define ATTR_CONST_NOTHROW_LIST		(ECF_NOTHROW | ECF_CONST)
#define ATTR_ALLOC_WARN_UNUSED_RESULT_SIZE_2_NOTHROW_LIST \
					(ECF_NOTHROW)
#define ATTR_COLD_NORETURN_NOTHROW_LEAF_LIST \
					(ECF_COLD | ECF_NORETURN | \
					 ECF_NOTHROW | ECF_LEAF)

static void
gfc_define_builtin (const char *name, tree type, enum built_in_function code,
		    const char *library_name, int attr)
{
  tree decl;

  decl = add_builtin_function (name, type, code, BUILT_IN_NORMAL,
			       library_name, NULL_TREE);
  set_call_expr_flags (decl, attr);

  set_builtin_decl (code, decl, true);
}


#define DO_DEFINE_MATH_BUILTIN(code, name, argtype, tbase) \
    gfc_define_builtin ("__builtin_" name "l", tbase##longdouble[argtype], \
			BUILT_IN_ ## code ## L, name "l", \
			ATTR_CONST_NOTHROW_LEAF_LIST); \
    gfc_define_builtin ("__builtin_" name, tbase##double[argtype], \
			BUILT_IN_ ## code, name, \
			ATTR_CONST_NOTHROW_LEAF_LIST); \
    gfc_define_builtin ("__builtin_" name "f", tbase##float[argtype], \
			BUILT_IN_ ## code ## F, name "f", \
			ATTR_CONST_NOTHROW_LEAF_LIST);

#define DEFINE_MATH_BUILTIN(code, name, argtype) \
    DO_DEFINE_MATH_BUILTIN (code, name, argtype, mfunc_)

#define DEFINE_MATH_BUILTIN_C(code, name, argtype) \
    DO_DEFINE_MATH_BUILTIN (code, name, argtype, mfunc_) \
    DO_DEFINE_MATH_BUILTIN (C##code, "c" name, argtype, mfunc_c)


/* Create function types for builtin functions.  */

static void
build_builtin_fntypes (tree *fntype, tree type)
{
  /* type (*) (type) */
  fntype[0] = build_function_type_list (type, type, NULL_TREE);
  /* type (*) (type, type) */
  fntype[1] = build_function_type_list (type, type, type, NULL_TREE);
  /* type (*) (type, int) */
  fntype[2] = build_function_type_list (type,
                                        type, integer_type_node, NULL_TREE);
  /* type (*) (void) */
  fntype[3] = build_function_type_list (type, NULL_TREE);
  /* type (*) (type, &int) */
  fntype[4] = build_function_type_list (type, type,
                                        build_pointer_type (integer_type_node),
                                        NULL_TREE);
  /* type (*) (int, type) */
  fntype[5] = build_function_type_list (type,
                                        integer_type_node, type, NULL_TREE);
}


static tree
builtin_type_for_size (int size, bool unsignedp)
{
  tree type = gfc_type_for_size (size, unsignedp);
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
#define DEF_FUNCTION_TYPE_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6) NAME,
#define DEF_FUNCTION_TYPE_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_8(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8) NAME,
#define DEF_FUNCTION_TYPE_9(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9) NAME,
#define DEF_FUNCTION_TYPE_10(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10) NAME,
#define DEF_FUNCTION_TYPE_11(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10, ARG11) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_VAR_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				 ARG6) NAME,
#define DEF_FUNCTION_TYPE_VAR_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7) NAME,
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
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_POINTER_TYPE
    BT_LAST
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
  tree builtin_types[(int) BT_LAST + 1];

  int attr;

  build_builtin_fntypes (mfunc_float, float_type_node);
  build_builtin_fntypes (mfunc_double, double_type_node);
  build_builtin_fntypes (mfunc_longdouble, long_double_type_node);
  build_builtin_fntypes (mfunc_cfloat, complex_float_type_node);
  build_builtin_fntypes (mfunc_cdouble, complex_double_type_node);
  build_builtin_fntypes (mfunc_clongdouble, complex_long_double_type_node);

  func_cfloat_float = build_function_type_list (float_type_node,
                                                complex_float_type_node,
                                                NULL_TREE);

  func_float_cfloat = build_function_type_list (complex_float_type_node,
                                                float_type_node, NULL_TREE);

  func_cdouble_double = build_function_type_list (double_type_node,
                                                  complex_double_type_node,
                                                  NULL_TREE);

  func_double_cdouble = build_function_type_list (complex_double_type_node,
                                                  double_type_node, NULL_TREE);

  func_clongdouble_longdouble =
    build_function_type_list (long_double_type_node,
                              complex_long_double_type_node, NULL_TREE);

  func_longdouble_clongdouble =
    build_function_type_list (complex_long_double_type_node,
                              long_double_type_node, NULL_TREE);

  ptype = build_pointer_type (float_type_node);
  func_float_floatp_floatp =
    build_function_type_list (void_type_node, ptype, ptype, NULL_TREE);

  ptype = build_pointer_type (double_type_node);
  func_double_doublep_doublep =
    build_function_type_list (void_type_node, ptype, ptype, NULL_TREE);

  ptype = build_pointer_type (long_double_type_node);
  func_longdouble_longdoublep_longdoublep =
    build_function_type_list (void_type_node, ptype, ptype, NULL_TREE);

/* Non-math builtins are defined manually, so they're not included here.  */
#define OTHER_BUILTIN(ID,NAME,TYPE,CONST)

#include "mathbuiltins.def"

  gfc_define_builtin ("__builtin_roundl", mfunc_longdouble[0], 
		      BUILT_IN_ROUNDL, "roundl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_round", mfunc_double[0], 
		      BUILT_IN_ROUND, "round", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_roundf", mfunc_float[0], 
		      BUILT_IN_ROUNDF, "roundf", ATTR_CONST_NOTHROW_LEAF_LIST);

  gfc_define_builtin ("__builtin_truncl", mfunc_longdouble[0],
		      BUILT_IN_TRUNCL, "truncl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_trunc", mfunc_double[0],
		      BUILT_IN_TRUNC, "trunc", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_truncf", mfunc_float[0],
		      BUILT_IN_TRUNCF, "truncf", ATTR_CONST_NOTHROW_LEAF_LIST);

  gfc_define_builtin ("__builtin_cabsl", func_clongdouble_longdouble, 
		      BUILT_IN_CABSL, "cabsl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_cabs", func_cdouble_double, 
		      BUILT_IN_CABS, "cabs", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_cabsf", func_cfloat_float, 
		      BUILT_IN_CABSF, "cabsf", ATTR_CONST_NOTHROW_LEAF_LIST);
 
  gfc_define_builtin ("__builtin_copysignl", mfunc_longdouble[1], 
		      BUILT_IN_COPYSIGNL, "copysignl",
		      ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_copysign", mfunc_double[1], 
		      BUILT_IN_COPYSIGN, "copysign",
		      ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_copysignf", mfunc_float[1], 
		      BUILT_IN_COPYSIGNF, "copysignf",
		      ATTR_CONST_NOTHROW_LEAF_LIST);
 
  gfc_define_builtin ("__builtin_nextafterl", mfunc_longdouble[1], 
		      BUILT_IN_NEXTAFTERL, "nextafterl",
		      ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_nextafter", mfunc_double[1], 
		      BUILT_IN_NEXTAFTER, "nextafter",
		      ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_nextafterf", mfunc_float[1], 
		      BUILT_IN_NEXTAFTERF, "nextafterf",
		      ATTR_CONST_NOTHROW_LEAF_LIST);
 
  /* Some built-ins depend on rounding mode. Depending on compilation options, they
     will be "pure" or "const".  */
  attr = flag_rounding_math ? ATTR_PURE_NOTHROW_LEAF_LIST : ATTR_CONST_NOTHROW_LEAF_LIST;

  gfc_define_builtin ("__builtin_rintl", mfunc_longdouble[0], 
		      BUILT_IN_RINTL, "rintl", attr);
  gfc_define_builtin ("__builtin_rint", mfunc_double[0], 
		      BUILT_IN_RINT, "rint", attr);
  gfc_define_builtin ("__builtin_rintf", mfunc_float[0], 
		      BUILT_IN_RINTF, "rintf", attr);

  gfc_define_builtin ("__builtin_remainderl", mfunc_longdouble[1], 
		      BUILT_IN_REMAINDERL, "remainderl", attr);
  gfc_define_builtin ("__builtin_remainder", mfunc_double[1], 
		      BUILT_IN_REMAINDER, "remainder", attr);
  gfc_define_builtin ("__builtin_remainderf", mfunc_float[1], 
		      BUILT_IN_REMAINDERF, "remainderf", attr);
 
  gfc_define_builtin ("__builtin_logbl", mfunc_longdouble[0], 
		      BUILT_IN_LOGBL, "logbl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_logb", mfunc_double[0], 
		      BUILT_IN_LOGB, "logb", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_logbf", mfunc_float[0], 
		      BUILT_IN_LOGBF, "logbf", ATTR_CONST_NOTHROW_LEAF_LIST);


  gfc_define_builtin ("__builtin_frexpl", mfunc_longdouble[4], 
		      BUILT_IN_FREXPL, "frexpl", ATTR_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_frexp", mfunc_double[4], 
		      BUILT_IN_FREXP, "frexp", ATTR_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_frexpf", mfunc_float[4], 
		      BUILT_IN_FREXPF, "frexpf", ATTR_NOTHROW_LEAF_LIST);
 
  gfc_define_builtin ("__builtin_fabsl", mfunc_longdouble[0], 
		      BUILT_IN_FABSL, "fabsl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_fabs", mfunc_double[0], 
		      BUILT_IN_FABS, "fabs", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_fabsf", mfunc_float[0], 
		      BUILT_IN_FABSF, "fabsf", ATTR_CONST_NOTHROW_LEAF_LIST);
 
  gfc_define_builtin ("__builtin_scalbnl", mfunc_longdouble[2],
		      BUILT_IN_SCALBNL, "scalbnl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_scalbn", mfunc_double[2],
		      BUILT_IN_SCALBN, "scalbn", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_scalbnf", mfunc_float[2],
		      BUILT_IN_SCALBNF, "scalbnf", ATTR_CONST_NOTHROW_LEAF_LIST);
 
  gfc_define_builtin ("__builtin_fmodl", mfunc_longdouble[1], 
		      BUILT_IN_FMODL, "fmodl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_fmod", mfunc_double[1], 
		      BUILT_IN_FMOD, "fmod", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_fmodf", mfunc_float[1], 
		      BUILT_IN_FMODF, "fmodf", ATTR_CONST_NOTHROW_LEAF_LIST);

  /* iround{f,,l}, lround{f,,l} and llround{f,,l} */
  ftype = build_function_type_list (integer_type_node,
                                    float_type_node, NULL_TREE); 
  gfc_define_builtin("__builtin_iroundf", ftype, BUILT_IN_IROUNDF,
		     "iroundf", ATTR_CONST_NOTHROW_LEAF_LIST);
  ftype = build_function_type_list (long_integer_type_node,
                                    float_type_node, NULL_TREE); 
  gfc_define_builtin ("__builtin_lroundf", ftype, BUILT_IN_LROUNDF,
		      "lroundf", ATTR_CONST_NOTHROW_LEAF_LIST);
  ftype = build_function_type_list (long_long_integer_type_node,
                                    float_type_node, NULL_TREE); 
  gfc_define_builtin ("__builtin_llroundf", ftype, BUILT_IN_LLROUNDF,
		      "llroundf", ATTR_CONST_NOTHROW_LEAF_LIST);

  ftype = build_function_type_list (integer_type_node,
                                    double_type_node, NULL_TREE); 
  gfc_define_builtin("__builtin_iround", ftype, BUILT_IN_IROUND,
		     "iround", ATTR_CONST_NOTHROW_LEAF_LIST);
  ftype = build_function_type_list (long_integer_type_node,
                                    double_type_node, NULL_TREE); 
  gfc_define_builtin ("__builtin_lround", ftype, BUILT_IN_LROUND,
		      "lround", ATTR_CONST_NOTHROW_LEAF_LIST);
  ftype = build_function_type_list (long_long_integer_type_node,
                                    double_type_node, NULL_TREE); 
  gfc_define_builtin ("__builtin_llround", ftype, BUILT_IN_LLROUND,
		      "llround", ATTR_CONST_NOTHROW_LEAF_LIST);

  ftype = build_function_type_list (integer_type_node,
                                    long_double_type_node, NULL_TREE); 
  gfc_define_builtin("__builtin_iroundl", ftype, BUILT_IN_IROUNDL,
		     "iroundl", ATTR_CONST_NOTHROW_LEAF_LIST);
  ftype = build_function_type_list (long_integer_type_node,
                                    long_double_type_node, NULL_TREE); 
  gfc_define_builtin ("__builtin_lroundl", ftype, BUILT_IN_LROUNDL,
		      "lroundl", ATTR_CONST_NOTHROW_LEAF_LIST);
  ftype = build_function_type_list (long_long_integer_type_node,
                                    long_double_type_node, NULL_TREE); 
  gfc_define_builtin ("__builtin_llroundl", ftype, BUILT_IN_LLROUNDL,
		      "llroundl", ATTR_CONST_NOTHROW_LEAF_LIST);

  /* These are used to implement the ** operator.  */
  gfc_define_builtin ("__builtin_powl", mfunc_longdouble[1], 
		      BUILT_IN_POWL, "powl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_pow", mfunc_double[1], 
		      BUILT_IN_POW, "pow", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_powf", mfunc_float[1], 
		      BUILT_IN_POWF, "powf", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_cpowl", mfunc_clongdouble[1], 
		      BUILT_IN_CPOWL, "cpowl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_cpow", mfunc_cdouble[1], 
		      BUILT_IN_CPOW, "cpow", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_cpowf", mfunc_cfloat[1], 
		      BUILT_IN_CPOWF, "cpowf", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_powil", mfunc_longdouble[2],
		      BUILT_IN_POWIL, "powil", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_powi", mfunc_double[2],
		      BUILT_IN_POWI, "powi", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_powif", mfunc_float[2],
		      BUILT_IN_POWIF, "powif", ATTR_CONST_NOTHROW_LEAF_LIST);


  if (targetm.libc_has_function (function_c99_math_complex, NULL_TREE))
    {
      gfc_define_builtin ("__builtin_cbrtl", mfunc_longdouble[0],
			  BUILT_IN_CBRTL, "cbrtl",
			  ATTR_CONST_NOTHROW_LEAF_LIST);
      gfc_define_builtin ("__builtin_cbrt", mfunc_double[0],
			  BUILT_IN_CBRT, "cbrt",
			  ATTR_CONST_NOTHROW_LEAF_LIST);
      gfc_define_builtin ("__builtin_cbrtf", mfunc_float[0],
			  BUILT_IN_CBRTF, "cbrtf",
			  ATTR_CONST_NOTHROW_LEAF_LIST);
      gfc_define_builtin ("__builtin_cexpil", func_longdouble_clongdouble, 
			  BUILT_IN_CEXPIL, "cexpil",
			  ATTR_CONST_NOTHROW_LEAF_LIST);
      gfc_define_builtin ("__builtin_cexpi", func_double_cdouble,
			  BUILT_IN_CEXPI, "cexpi",
			  ATTR_CONST_NOTHROW_LEAF_LIST);
      gfc_define_builtin ("__builtin_cexpif", func_float_cfloat,
			  BUILT_IN_CEXPIF, "cexpif",
			  ATTR_CONST_NOTHROW_LEAF_LIST);
    }

  if (targetm.libc_has_function (function_sincos, NULL_TREE))
    {
      gfc_define_builtin ("__builtin_sincosl",
			  func_longdouble_longdoublep_longdoublep,
			  BUILT_IN_SINCOSL, "sincosl", ATTR_NOTHROW_LEAF_LIST);
      gfc_define_builtin ("__builtin_sincos", func_double_doublep_doublep,
			  BUILT_IN_SINCOS, "sincos", ATTR_NOTHROW_LEAF_LIST);
      gfc_define_builtin ("__builtin_sincosf", func_float_floatp_floatp,
			  BUILT_IN_SINCOSF, "sincosf", ATTR_NOTHROW_LEAF_LIST);
    }

  /* For LEADZ, TRAILZ, POPCNT and POPPAR.  */
  ftype = build_function_type_list (integer_type_node,
                                    unsigned_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_clz", ftype, BUILT_IN_CLZ,
		      "__builtin_clz", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_ctz", ftype, BUILT_IN_CTZ,
		      "__builtin_ctz", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_parity", ftype, BUILT_IN_PARITY,
		      "__builtin_parity", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_popcount", ftype, BUILT_IN_POPCOUNT,
		      "__builtin_popcount", ATTR_CONST_NOTHROW_LEAF_LIST);

  ftype = build_function_type_list (integer_type_node,
                                    long_unsigned_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_clzl", ftype, BUILT_IN_CLZL,
		      "__builtin_clzl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_ctzl", ftype, BUILT_IN_CTZL,
		      "__builtin_ctzl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_parityl", ftype, BUILT_IN_PARITYL,
		      "__builtin_parityl", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_popcountl", ftype, BUILT_IN_POPCOUNTL,
		      "__builtin_popcountl", ATTR_CONST_NOTHROW_LEAF_LIST);

  ftype = build_function_type_list (integer_type_node,
                                    long_long_unsigned_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_clzll", ftype, BUILT_IN_CLZLL,
		      "__builtin_clzll", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_ctzll", ftype, BUILT_IN_CTZLL,
		      "__builtin_ctzll", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_parityll", ftype, BUILT_IN_PARITYLL,
		      "__builtin_parityll", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_popcountll", ftype, BUILT_IN_POPCOUNTLL,
		      "__builtin_popcountll", ATTR_CONST_NOTHROW_LEAF_LIST);

  /* Other builtin functions we use.  */

  ftype = build_function_type_list (long_integer_type_node,
                                    long_integer_type_node,
                                    long_integer_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_expect", ftype, BUILT_IN_EXPECT,
		      "__builtin_expect", ATTR_CONST_NOTHROW_LEAF_LIST);

  ftype = build_function_type_list (void_type_node,
                                    pvoid_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_free", ftype, BUILT_IN_FREE,
		      "free", ATTR_NOTHROW_LEAF_LIST);

  ftype = build_function_type_list (pvoid_type_node,
                                    size_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_malloc", ftype, BUILT_IN_MALLOC,
		      "malloc", ATTR_NOTHROW_LEAF_MALLOC_LIST);

  ftype = build_function_type_list (pvoid_type_node, size_type_node,
				    size_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_calloc", ftype, BUILT_IN_CALLOC,
		      "calloc", ATTR_NOTHROW_LEAF_MALLOC_LIST);
  DECL_IS_MALLOC (builtin_decl_explicit (BUILT_IN_CALLOC)) = 1;

  ftype = build_function_type_list (pvoid_type_node,
                                    size_type_node, pvoid_type_node,
                                    NULL_TREE);
  gfc_define_builtin ("__builtin_realloc", ftype, BUILT_IN_REALLOC,
		      "realloc", ATTR_NOTHROW_LEAF_LIST);

  /* Type-generic floating-point classification built-ins.  */

  ftype = build_function_type (integer_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_isfinite", ftype, BUILT_IN_ISFINITE,
		      "__builtin_isfinite", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_isinf", ftype, BUILT_IN_ISINF,
		      "__builtin_isinf", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_isinf_sign", ftype, BUILT_IN_ISINF_SIGN,
		      "__builtin_isinf_sign", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_isnan", ftype, BUILT_IN_ISNAN,
		      "__builtin_isnan", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_isnormal", ftype, BUILT_IN_ISNORMAL,
		      "__builtin_isnormal", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_signbit", ftype, BUILT_IN_SIGNBIT,
		      "__builtin_signbit", ATTR_CONST_NOTHROW_LEAF_LIST);

  ftype = build_function_type (integer_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_isless", ftype, BUILT_IN_ISLESS,
		      "__builtin_isless", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_islessequal", ftype, BUILT_IN_ISLESSEQUAL,
		      "__builtin_islessequal", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_islessgreater", ftype, BUILT_IN_ISLESSGREATER,
		      "__builtin_islessgreater", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_isgreater", ftype, BUILT_IN_ISGREATER,
		      "__builtin_isgreater", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_isgreaterequal", ftype,
		      BUILT_IN_ISGREATEREQUAL, "__builtin_isgreaterequal",
		      ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__builtin_isunordered", ftype, BUILT_IN_ISUNORDERED,
		      "__builtin_isunordered", ATTR_CONST_NOTHROW_LEAF_LIST);


#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) \
  builtin_types[(int) ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN)                       \
  builtin_types[(int) ENUM]                                     \
    = build_function_type_list (builtin_types[(int) RETURN],	\
                                NULL_TREE);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1)				\
  builtin_types[(int) ENUM]						\
    = build_function_type_list (builtin_types[(int) RETURN],            \
                                builtin_types[(int) ARG1],              \
                                NULL_TREE);
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2)           \
  builtin_types[(int) ENUM]                                     \
    = build_function_type_list (builtin_types[(int) RETURN],    \
                                builtin_types[(int) ARG1],      \
                                builtin_types[(int) ARG2],      \
                                NULL_TREE);
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3)             \
  builtin_types[(int) ENUM]                                             \
    = build_function_type_list (builtin_types[(int) RETURN],            \
                                builtin_types[(int) ARG1],              \
                                builtin_types[(int) ARG2],              \
                                builtin_types[(int) ARG3],              \
                                NULL_TREE);
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4)	\
  builtin_types[(int) ENUM]						\
    = build_function_type_list (builtin_types[(int) RETURN],            \
                                builtin_types[(int) ARG1],              \
                                builtin_types[(int) ARG2],              \
                                builtin_types[(int) ARG3],		\
                                builtin_types[(int) ARG4],              \
                                NULL_TREE);
#define DEF_FUNCTION_TYPE_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5)	\
  builtin_types[(int) ENUM]						\
    = build_function_type_list (builtin_types[(int) RETURN],            \
                                builtin_types[(int) ARG1],              \
                                builtin_types[(int) ARG2],              \
                                builtin_types[(int) ARG3],		\
                                builtin_types[(int) ARG4],              \
                                builtin_types[(int) ARG5],              \
                                NULL_TREE);
#define DEF_FUNCTION_TYPE_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6)					\
  builtin_types[(int) ENUM]						\
    = build_function_type_list (builtin_types[(int) RETURN],            \
                                builtin_types[(int) ARG1],              \
                                builtin_types[(int) ARG2],              \
                                builtin_types[(int) ARG3],		\
                                builtin_types[(int) ARG4],		\
                                builtin_types[(int) ARG5],              \
                                builtin_types[(int) ARG6],              \
                                NULL_TREE);
#define DEF_FUNCTION_TYPE_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7)					\
  builtin_types[(int) ENUM]						\
    = build_function_type_list (builtin_types[(int) RETURN],            \
                                builtin_types[(int) ARG1],              \
                                builtin_types[(int) ARG2],              \
                                builtin_types[(int) ARG3],		\
                                builtin_types[(int) ARG4],		\
                                builtin_types[(int) ARG5],              \
                                builtin_types[(int) ARG6],              \
                                builtin_types[(int) ARG7],              \
                                NULL_TREE);
#define DEF_FUNCTION_TYPE_8(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8)				\
  builtin_types[(int) ENUM]						\
    = build_function_type_list (builtin_types[(int) RETURN],		\
				builtin_types[(int) ARG1],		\
				builtin_types[(int) ARG2],		\
				builtin_types[(int) ARG3],		\
				builtin_types[(int) ARG4],		\
				builtin_types[(int) ARG5],		\
				builtin_types[(int) ARG6],		\
				builtin_types[(int) ARG7],		\
				builtin_types[(int) ARG8],		\
				NULL_TREE);
#define DEF_FUNCTION_TYPE_9(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9)			\
  builtin_types[(int) ENUM]						\
    = build_function_type_list (builtin_types[(int) RETURN],		\
				builtin_types[(int) ARG1],		\
				builtin_types[(int) ARG2],		\
				builtin_types[(int) ARG3],		\
				builtin_types[(int) ARG4],		\
				builtin_types[(int) ARG5],		\
				builtin_types[(int) ARG6],		\
				builtin_types[(int) ARG7],		\
				builtin_types[(int) ARG8],		\
				builtin_types[(int) ARG9],		\
				NULL_TREE);
#define DEF_FUNCTION_TYPE_10(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4,	\
			     ARG5, ARG6, ARG7, ARG8, ARG9, ARG10)	\
  builtin_types[(int) ENUM]						\
    = build_function_type_list (builtin_types[(int) RETURN],		\
				builtin_types[(int) ARG1],		\
				builtin_types[(int) ARG2],		\
				builtin_types[(int) ARG3],		\
				builtin_types[(int) ARG4],		\
				builtin_types[(int) ARG5],		\
				builtin_types[(int) ARG6],		\
				builtin_types[(int) ARG7],		\
				builtin_types[(int) ARG8],		\
				builtin_types[(int) ARG9],		\
				builtin_types[(int) ARG10],		\
				NULL_TREE);
#define DEF_FUNCTION_TYPE_11(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4,	\
			     ARG5, ARG6, ARG7, ARG8, ARG9, ARG10, ARG11)\
  builtin_types[(int) ENUM]						\
    = build_function_type_list (builtin_types[(int) RETURN],		\
				builtin_types[(int) ARG1],		\
				builtin_types[(int) ARG2],		\
				builtin_types[(int) ARG3],		\
				builtin_types[(int) ARG4],		\
				builtin_types[(int) ARG5],		\
				builtin_types[(int) ARG6],		\
				builtin_types[(int) ARG7],		\
				builtin_types[(int) ARG8],		\
				builtin_types[(int) ARG9],		\
				builtin_types[(int) ARG10],		\
				builtin_types[(int) ARG11],		\
				NULL_TREE);
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN)				\
  builtin_types[(int) ENUM]						\
    = build_varargs_function_type_list (builtin_types[(int) RETURN],    \
                                        NULL_TREE);
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1)			\
  builtin_types[(int) ENUM]						\
    = build_varargs_function_type_list (builtin_types[(int) RETURN],    \
					builtin_types[(int) ARG1],     	\
					NULL_TREE);
#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2)		\
  builtin_types[(int) ENUM]						\
    = build_varargs_function_type_list (builtin_types[(int) RETURN],   	\
					builtin_types[(int) ARG1],     	\
					builtin_types[(int) ARG2],     	\
					NULL_TREE);
#define DEF_FUNCTION_TYPE_VAR_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6)	\
  builtin_types[(int) ENUM]						\
    = build_varargs_function_type_list (builtin_types[(int) RETURN],   	\
					builtin_types[(int) ARG1],     	\
					builtin_types[(int) ARG2],     	\
					builtin_types[(int) ARG3],	\
					builtin_types[(int) ARG4],	\
					builtin_types[(int) ARG5],	\
					builtin_types[(int) ARG6],	\
					NULL_TREE);
#define DEF_FUNCTION_TYPE_VAR_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7)				\
  builtin_types[(int) ENUM]						\
    = build_varargs_function_type_list (builtin_types[(int) RETURN],   	\
					builtin_types[(int) ARG1],     	\
					builtin_types[(int) ARG2],     	\
					builtin_types[(int) ARG3],	\
					builtin_types[(int) ARG4],	\
					builtin_types[(int) ARG5],	\
					builtin_types[(int) ARG6],	\
					builtin_types[(int) ARG7],	\
					NULL_TREE);
#define DEF_POINTER_TYPE(ENUM, TYPE)			\
  builtin_types[(int) ENUM]				\
    = build_pointer_type (builtin_types[(int) TYPE]);
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
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_POINTER_TYPE
  builtin_types[(int) BT_LAST] = NULL_TREE;

  /* Initialize synchronization builtins.  */
#undef DEF_SYNC_BUILTIN
#define DEF_SYNC_BUILTIN(code, name, type, attr) \
    gfc_define_builtin (name, builtin_types[type], code, name, \
			attr);
#include "../sync-builtins.def"
#undef DEF_SYNC_BUILTIN

  if (flag_openacc)
    {
#undef DEF_GOACC_BUILTIN
#define DEF_GOACC_BUILTIN(code, name, type, attr) \
      gfc_define_builtin ("__builtin_" name, builtin_types[type], \
			  code, name, attr);
#undef DEF_GOACC_BUILTIN_COMPILER
#define DEF_GOACC_BUILTIN_COMPILER(code, name, type, attr) \
      gfc_define_builtin (name, builtin_types[type], code, name, attr);
#undef DEF_GOACC_BUILTIN_ONLY
#define DEF_GOACC_BUILTIN_ONLY(code, name, type, attr) \
      gfc_define_builtin ("__builtin_" name, builtin_types[type], code, NULL, \
			  attr);
#undef DEF_GOMP_BUILTIN
#define DEF_GOMP_BUILTIN(code, name, type, attr) /* ignore */
#include "../omp-builtins.def"
#undef DEF_GOACC_BUILTIN
#undef DEF_GOACC_BUILTIN_COMPILER
#undef DEF_GOMP_BUILTIN
    }

  if (flag_openmp || flag_openmp_simd || flag_tree_parallelize_loops)
    {
#undef DEF_GOACC_BUILTIN
#define DEF_GOACC_BUILTIN(code, name, type, attr) /* ignore */
#undef DEF_GOACC_BUILTIN_COMPILER
#define DEF_GOACC_BUILTIN_COMPILER(code, name, type, attr)  /* ignore */
#undef DEF_GOMP_BUILTIN
#define DEF_GOMP_BUILTIN(code, name, type, attr) \
      gfc_define_builtin ("__builtin_" name, builtin_types[type], \
			  code, name, attr);
#include "../omp-builtins.def"
#undef DEF_GOACC_BUILTIN
#undef DEF_GOACC_BUILTIN_COMPILER
#undef DEF_GOMP_BUILTIN
      tree gomp_alloc = builtin_decl_explicit (BUILT_IN_GOMP_ALLOC);
      tree two = build_int_cst (integer_type_node, 2);
      DECL_ATTRIBUTES (gomp_alloc)
	= tree_cons (get_identifier ("warn_unused_result"), NULL_TREE,
		     tree_cons (get_identifier ("alloc_size"),
				build_tree_list (NULL_TREE, two),
				DECL_ATTRIBUTES (gomp_alloc)));
    }

  gfc_define_builtin ("__builtin_trap", builtin_types[BT_FN_VOID],
		      BUILT_IN_TRAP, NULL, ATTR_NOTHROW_LEAF_LIST);
  TREE_THIS_VOLATILE (builtin_decl_explicit (BUILT_IN_TRAP)) = 1;

  ftype = build_varargs_function_type_list (ptr_type_node, const_ptr_type_node,
					    size_type_node, NULL_TREE);
  gfc_define_builtin ("__builtin_assume_aligned", ftype,
		      BUILT_IN_ASSUME_ALIGNED,
		      "__builtin_assume_aligned",
		      ATTR_CONST_NOTHROW_LEAF_LIST);

  gfc_define_builtin ("__emutls_get_address",
		      builtin_types[BT_FN_PTR_PTR],
		      BUILT_IN_EMUTLS_GET_ADDRESS,
		      "__emutls_get_address", ATTR_CONST_NOTHROW_LEAF_LIST);
  gfc_define_builtin ("__emutls_register_common",
		      builtin_types[BT_FN_VOID_PTR_WORD_WORD_PTR],
		      BUILT_IN_EMUTLS_REGISTER_COMMON,
		      "__emutls_register_common", ATTR_NOTHROW_LEAF_LIST);

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
