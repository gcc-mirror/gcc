/* Built-in and inline functions for gcj
   Copyright (C) 2001
   Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Tom Tromey <tromey@redhat.com>.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "ggc.h"
#include "flags.h"
#include "langhooks.h"
#include "java-tree.h"

enum builtin_type 
{
#define DEF_PRIMITIVE_TYPE(NAME, VALUE) NAME,
#define DEF_FUNCTION_TYPE_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_VAR_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_POINTER_TYPE(NAME, TYPE) NAME,
#include "builtin-types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_POINTER_TYPE
  BT_LAST
};

static tree max_builtin PARAMS ((tree, tree));
static tree min_builtin PARAMS ((tree, tree));
static tree abs_builtin PARAMS ((tree, tree));
static tree cos_builtin PARAMS ((tree, tree));
static tree sin_builtin PARAMS ((tree, tree));
static tree sqrt_builtin PARAMS ((tree, tree));

static tree build_function_call_expr PARAMS ((tree, tree));
static void define_builtin PARAMS ((enum built_in_function,
				    const char *,
				    enum built_in_class,
				    tree, int));
static tree define_builtin_type PARAMS ((int, int, int, int, int));



/* Functions of this type are used to inline a given call.  Such a
   function should either return an expression, if the call is to be
   inlined, or NULL_TREE if a real call should be emitted.  Arguments
   are method return type and arguments to call.  */
typedef tree builtin_creator_function PARAMS ((tree, tree));

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
  builtin_creator_function * GTY((skip (""))) creator;
};

static GTY(()) struct builtin_record java_builtins[] =
{
  { { "java.lang.Math" }, { "min" }, min_builtin },
  { { "java.lang.Math" }, { "max" }, max_builtin },
  { { "java.lang.Math" }, { "abs" }, abs_builtin },
  { { "java.lang.Math" }, { "cos" }, cos_builtin },
  { { "java.lang.Math" }, { "sin" }, sin_builtin },
  { { "java.lang.Math" }, { "sqrt" }, sqrt_builtin },
  { { NULL }, { NULL }, NULL }
};

/* This is only used transiently, so we don't mark it as roots for the
   GC.  */
static tree builtin_types[(int) BT_LAST];


/* Internal functions which implement various builtin conversions.  */

static tree
max_builtin (method_return_type, method_arguments)
     tree method_return_type, method_arguments;
{
  return build (MAX_EXPR, method_return_type,
		TREE_VALUE (method_arguments),
		TREE_VALUE (TREE_CHAIN (method_arguments)));
}

static tree
min_builtin (method_return_type, method_arguments)
     tree method_return_type, method_arguments;
{
  return build (MIN_EXPR, method_return_type,
		TREE_VALUE (method_arguments),
		TREE_VALUE (TREE_CHAIN (method_arguments)));
}

static tree
abs_builtin (method_return_type, method_arguments)
     tree method_return_type, method_arguments;
{
  return build1 (ABS_EXPR, method_return_type,
		 TREE_VALUE (method_arguments));
}

/* Mostly copied from ../builtins.c.  */
static tree
build_function_call_expr (tree fn, tree arglist)
{
  tree call_expr;

  call_expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fn)), fn);
  call_expr = build (CALL_EXPR, TREE_TYPE (TREE_TYPE (fn)),
		     call_expr, arglist);
  TREE_SIDE_EFFECTS (call_expr) = 1;
  return call_expr;
}

static tree
cos_builtin (method_return_type, method_arguments)
     tree method_return_type ATTRIBUTE_UNUSED, method_arguments;
{
  /* FIXME: this assumes that jdouble and double are the same.  */
  tree fn = built_in_decls[BUILT_IN_COS];
  if (fn == NULL_TREE)
    return NULL_TREE;
  return build_function_call_expr (fn, method_arguments);
}

static tree
sin_builtin (method_return_type, method_arguments)
     tree method_return_type ATTRIBUTE_UNUSED, method_arguments;
{
  /* FIXME: this assumes that jdouble and double are the same.  */
  tree fn = built_in_decls[BUILT_IN_SIN];
  if (fn == NULL_TREE)
    return NULL_TREE;
  return build_function_call_expr (fn, method_arguments);
}

static tree
sqrt_builtin (method_return_type, method_arguments)
     tree method_return_type ATTRIBUTE_UNUSED, method_arguments;
{
  /* FIXME: this assumes that jdouble and double are the same.  */
  tree fn = built_in_decls[BUILT_IN_SQRT];
  if (fn == NULL_TREE)
    return NULL_TREE;
  return build_function_call_expr (fn, method_arguments);
}



/* Define a single builtin.  */
static void
define_builtin (val, name, class, type, fallback_p)
     enum built_in_function val;
     const char *name;
     enum built_in_class class;
     tree type;
     int fallback_p;
{
  tree decl;

  if (! name || ! type)
    return;

  if (strncmp (name, "__builtin_", strlen ("__builtin_")) != 0)
    abort ();
  decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (fallback_p)
    SET_DECL_ASSEMBLER_NAME (decl,
			     get_identifier (name + strlen ("__builtin_")));
  make_decl_rtl (decl, NULL);
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = val;
  built_in_decls[val] = decl;
}

/* Compute the type for a builtin.  */
static tree
define_builtin_type (ret, arg1, arg2, arg3, arg4)
     int ret, arg1, arg2, arg3, arg4;
{
  tree args;

  if (builtin_types[ret] == NULL_TREE)
    return NULL_TREE;

  args = void_list_node;

  if (arg4 != -1)
    {
      if (builtin_types[arg4] == NULL_TREE)
	return NULL_TREE;
      args = tree_cons (NULL_TREE, builtin_types[arg4], args);
    }
  if (arg3 != -1)
    {
      if (builtin_types[arg3] == NULL_TREE)
	return NULL_TREE;
      args = tree_cons (NULL_TREE, builtin_types[arg3], args);
    }
  if (arg2 != -1)
    {
      if (builtin_types[arg2] == NULL_TREE)
	return NULL_TREE;
      args = tree_cons (NULL_TREE, builtin_types[arg2], args);
    }
  if (arg1 != -1)
    {
      if (builtin_types[arg1] == NULL_TREE)
	return NULL_TREE;
      args = tree_cons (NULL_TREE, builtin_types[arg1], args);
    }
  
  return build_function_type (builtin_types[ret], args);
}



/* Initialize the builtins.  */
void
initialize_builtins ()
{
  int i;

  for (i = 0; java_builtins[i].creator != NULL; ++i)
    {
      tree klass_id = get_identifier (java_builtins[i].class_name.s);
      tree m = get_identifier (java_builtins[i].method_name.s);

      java_builtins[i].class_name.t = klass_id;
      java_builtins[i].method_name.t = m;
    }

  void_list_node = end_params_node;

  /* Work around C-specific junk in builtin-types.def.  */
#define intmax_type_node NULL_TREE
#define c_size_type_node NULL_TREE
#define const_string_type_node NULL_TREE
#define va_list_ref_type_node NULL_TREE
#define va_list_arg_type_node NULL_TREE
#define flag_isoc99 0

#define DEF_PRIMITIVE_TYPE(ENUM, VALUE)					      \
  builtin_types[(int) ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN)		\
  builtin_types[(int) ENUM]				\
    = define_builtin_type (RETURN, -1, -1, -1, -1);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1)				\
  builtin_types[(int) ENUM]						\
    = define_builtin_type (RETURN, ARG1, -1, -1, -1);
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2)	\
  builtin_types[(int) ENUM]				\
    = define_builtin_type (RETURN, ARG1, ARG2, -1, -1);
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3)		 \
  builtin_types[(int) ENUM]						 \
    = define_builtin_type (RETURN, ARG1, ARG2, ARG3, -1);
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4)	\
  builtin_types[(int) ENUM]						\
    = define_builtin_type (RETURN, ARG1, ARG2, ARG3, ARG4);
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN)			\
  builtin_types[(int) ENUM] = NULL_TREE;
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1)		\
   builtin_types[(int) ENUM] = NULL_TREE;
#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2)	\
   builtin_types[(int) ENUM] = NULL_TREE;
#define DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, ARG1, ARG2, ARG3)	\
   builtin_types[(int) ENUM] = NULL_TREE;
#define DEF_POINTER_TYPE(ENUM, TYPE)			\
  builtin_types[(int) ENUM] = NULL_TREE;

#include "builtin-types.def"

#define DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P, \
                    FALLBACK_P, NONANSI_P, ATTRS) \
  define_builtin (ENUM, NAME, CLASS, builtin_types[TYPE], FALLBACK_P);
#include "builtins.def"
}

/* If the call matches a builtin, return the
   appropriate builtin expression instead.  */
tree
check_for_builtin (method, call)
     tree method;
     tree call;
{
  if (! flag_emit_class_files && optimize && TREE_CODE (call) == CALL_EXPR)
    {
      int i;
      tree method_arguments = TREE_OPERAND (call, 1);
      tree method_class = DECL_NAME (TYPE_NAME (DECL_CONTEXT (method)));
      tree method_name = DECL_NAME (method);
      tree method_return_type = TREE_TYPE (TREE_TYPE (method));

      for (i = 0; java_builtins[i].creator != NULL; ++i)
	{
	  if (method_class == java_builtins[i].class_name.t
	      && method_name == java_builtins[i].method_name.t)
	    {
	      return (*java_builtins[i].creator) (method_return_type,
						  method_arguments);
	    }
	}
    }
  return call;
}

#include "gt-java-builtins.h"
