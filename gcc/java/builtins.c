/* Built-in and inline functions for gcj
   Copyright (C) 2001, 2003, 2004, 2005
   Free Software Foundation, Inc.

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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Tom Tromey <tromey@redhat.com>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "ggc.h"
#include "flags.h"
#include "langhooks.h"
#include "java-tree.h"


static tree max_builtin (tree, tree);
static tree min_builtin (tree, tree);
static tree abs_builtin (tree, tree);

static tree java_build_function_call_expr (tree, tree);



/* Functions of this type are used to inline a given call.  Such a
   function should either return an expression, if the call is to be
   inlined, or NULL_TREE if a real call should be emitted.  Arguments
   are method return type and arguments to call.  */
typedef tree builtin_creator_function (tree, tree);

/* Hold a char*, before initialization, or a tree, after
   initialization.  */
union string_or_tree GTY(())
{
  const char * GTY ((tag ("0"))) s;
  tree GTY ((tag ("1"))) t;
};

/* Used to hold a single builtin record.  */
struct builtin_record GTY(())
{
  union string_or_tree GTY ((desc ("1"))) class_name;
  union string_or_tree GTY ((desc ("1"))) method_name;
  builtin_creator_function * GTY((skip)) creator;
  enum built_in_function builtin_code;
};

static GTY(()) struct builtin_record java_builtins[] =
{
  { { "java.lang.Math" }, { "min" }, min_builtin, 0 },
  { { "java.lang.Math" }, { "max" }, max_builtin, 0 },
  { { "java.lang.Math" }, { "abs" }, abs_builtin, 0 },
  { { "java.lang.Math" }, { "acos" }, NULL, BUILT_IN_ACOS },
  { { "java.lang.Math" }, { "asin" }, NULL, BUILT_IN_ASIN },
  { { "java.lang.Math" }, { "atan" }, NULL, BUILT_IN_ATAN },
  { { "java.lang.Math" }, { "atan2" }, NULL, BUILT_IN_ATAN2 },
  { { "java.lang.Math" }, { "ceil" }, NULL, BUILT_IN_CEIL },
  { { "java.lang.Math" }, { "cos" }, NULL, BUILT_IN_COS },
  { { "java.lang.Math" }, { "exp" }, NULL, BUILT_IN_EXP },
  { { "java.lang.Math" }, { "floor" }, NULL, BUILT_IN_FLOOR },
  { { "java.lang.Math" }, { "log" }, NULL, BUILT_IN_LOG },
  { { "java.lang.Math" }, { "pow" }, NULL, BUILT_IN_POW },
  { { "java.lang.Math" }, { "sin" }, NULL, BUILT_IN_SIN },
  { { "java.lang.Math" }, { "sqrt" }, NULL, BUILT_IN_SQRT },
  { { "java.lang.Math" }, { "tan" }, NULL, BUILT_IN_TAN },
  { { NULL }, { NULL }, NULL, END_BUILTINS }
};


/* Internal functions which implement various builtin conversions.  */

static tree
max_builtin (tree method_return_type, tree method_arguments)
{
  return fold (build2 (MAX_EXPR, method_return_type,
		       TREE_VALUE (method_arguments),
		       TREE_VALUE (TREE_CHAIN (method_arguments))));
}

static tree
min_builtin (tree method_return_type, tree method_arguments)
{
  return fold (build2 (MIN_EXPR, method_return_type,
		       TREE_VALUE (method_arguments),
		       TREE_VALUE (TREE_CHAIN (method_arguments))));
}

static tree
abs_builtin (tree method_return_type, tree method_arguments)
{
  return fold (build1 (ABS_EXPR, method_return_type,
		       TREE_VALUE (method_arguments)));
}

/* Mostly copied from ../builtins.c.  */
static tree
java_build_function_call_expr (tree fn, tree arglist)
{
  tree call_expr;

  call_expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fn)), fn);
  call_expr = build3 (CALL_EXPR, TREE_TYPE (TREE_TYPE (fn)),
		      call_expr, arglist, NULL_TREE);
  TREE_SIDE_EFFECTS (call_expr) = 1;
  return fold (call_expr);
}



#define BUILTIN_NOTHROW 1
#define BUILTIN_CONST 2
/* Define a single builtin.  */
static void
define_builtin (enum built_in_function val,
		const char *name,
		tree type,
		const char *libname,
		int flags)
{
  tree decl;

  decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  SET_DECL_ASSEMBLER_NAME (decl, get_identifier (libname));
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = BUILT_IN_NORMAL;
  DECL_FUNCTION_CODE (decl) = val;
  if (flags & BUILTIN_NOTHROW)
    TREE_NOTHROW (decl) = 1;
  if (flags & BUILTIN_CONST)
    TREE_READONLY (decl) = 1;

  implicit_built_in_decls[val] = decl;
  built_in_decls[val] = decl;
}



/* Initialize the builtins.  */
void
initialize_builtins (void)
{
  tree double_ftype_double, double_ftype_double_double;
  tree float_ftype_float, float_ftype_float_float;
  tree boolean_ftype_boolean_boolean;
  tree t;
  int i;

  for (i = 0; java_builtins[i].builtin_code != END_BUILTINS; ++i)
    {
      tree klass_id = get_identifier (java_builtins[i].class_name.s);
      tree m = get_identifier (java_builtins[i].method_name.s);

      java_builtins[i].class_name.t = klass_id;
      java_builtins[i].method_name.t = m;
    }

  void_list_node = end_params_node;

  t = tree_cons (NULL_TREE, float_type_node, end_params_node);
  float_ftype_float = build_function_type (float_type_node, t);
  t = tree_cons (NULL_TREE, float_type_node, t);
  float_ftype_float_float = build_function_type (float_type_node, t);

  t = tree_cons (NULL_TREE, double_type_node, end_params_node);
  double_ftype_double = build_function_type (double_type_node, t);
  t = tree_cons (NULL_TREE, double_type_node, t);
  double_ftype_double_double = build_function_type (double_type_node, t);

  define_builtin (BUILT_IN_FMOD, "__builtin_fmod",
		  double_ftype_double_double, "fmod", BUILTIN_CONST);
  define_builtin (BUILT_IN_FMODF, "__builtin_fmodf",
		  float_ftype_float_float, "fmodf", BUILTIN_CONST);

  define_builtin (BUILT_IN_ACOS, "__builtin_acos",
		  double_ftype_double, "_ZN4java4lang4Math4acosEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_ASIN, "__builtin_asin",
		  double_ftype_double, "_ZN4java4lang4Math4asinEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_ATAN, "__builtin_atan",
		  double_ftype_double, "_ZN4java4lang4Math4atanEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_ATAN2, "__builtin_atan2",
		  double_ftype_double_double, "_ZN4java4lang4Math5atan2Edd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_CEIL, "__builtin_ceil",
		  double_ftype_double, "_ZN4java4lang4Math4ceilEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_COS, "__builtin_cos",
		  double_ftype_double, "_ZN4java4lang4Math3cosEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_EXP, "__builtin_exp",
		  double_ftype_double, "_ZN4java4lang4Math3expEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_FLOOR, "__builtin_floor",
		  double_ftype_double, "_ZN4java4lang4Math5floorEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_LOG, "__builtin_log",
		  double_ftype_double, "_ZN4java4lang4Math3logEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_POW, "__builtin_pow",
		  double_ftype_double_double, "_ZN4java4lang4Math3powEdd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_SIN, "__builtin_sin",
		  double_ftype_double, "_ZN4java4lang4Math3sinEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_SQRT, "__builtin_sqrt",
		  double_ftype_double, "_ZN4java4lang4Math4sqrtEd",
		  BUILTIN_CONST);
  define_builtin (BUILT_IN_TAN, "__builtin_tan",
		  double_ftype_double, "_ZN4java4lang4Math3tanEd",
		  BUILTIN_CONST);
  
  t = tree_cons (NULL_TREE, boolean_type_node, end_params_node);
  t = tree_cons (NULL_TREE, boolean_type_node, t);
  boolean_ftype_boolean_boolean = build_function_type (boolean_type_node, t);
  define_builtin (BUILT_IN_EXPECT, "__builtin_expect", 
		  boolean_ftype_boolean_boolean,
		  "__builtin_expect",
		  BUILTIN_CONST | BUILTIN_NOTHROW);
		  
  build_common_builtin_nodes ();
}

/* If the call matches a builtin, return the
   appropriate builtin expression instead.  */
tree
check_for_builtin (tree method, tree call)
{
  if (! flag_emit_class_files && optimize && TREE_CODE (call) == CALL_EXPR)
    {
      int i;
      tree method_arguments = TREE_OPERAND (call, 1);
      tree method_class = DECL_NAME (TYPE_NAME (DECL_CONTEXT (method)));
      tree method_name = DECL_NAME (method);
      tree method_return_type = TREE_TYPE (TREE_TYPE (method));

      for (i = 0; java_builtins[i].builtin_code != END_BUILTINS; ++i)
	{
	  if (method_class == java_builtins[i].class_name.t
	      && method_name == java_builtins[i].method_name.t)
	    {
	      tree fn;

	      if (java_builtins[i].creator != NULL)
		return (*java_builtins[i].creator) (method_return_type,
						    method_arguments);
	      fn = built_in_decls[java_builtins[i].builtin_code];
	      if (fn == NULL_TREE)
		return NULL_TREE;
	      return java_build_function_call_expr (fn, method_arguments);
	    }
	}
    }
  return call;
}

#include "gt-java-builtins.h"
