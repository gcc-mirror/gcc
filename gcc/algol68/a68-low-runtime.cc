/* Libcalls to Algol 68 run-time functions.
   Copyright (C) 2006-2025 Free Software Foundation, Inc.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.
   Adapted from gcc/d/runtime.cc.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* The lowering pass may generate expressions to call various runtime library
   functions.  Most of these functions are implemented in libga68.  This file
   provides facilities to compile libcalls to runtime functions.  The file
   a68-low-runtime.def contains a database of available runtime library
   functions.  */

enum a68_libcall_type
{
  LCT_VOID,
  LCT_CHAR,
  LCT_CONSTCHARPTR,
  LCT_VOIDPTR,
  LCT_UNISTR,
  LCT_UNISTRPTR,
  LCT_SIZE,
  LCT_SSIZE,
  LCT_SIZEPTR,
  LCT_UINT,
  LCT_INT,
  LCT_LONGLONGINT,
  LCT_FLOAT,
  LCT_DOUBLE,
  LCT_LONGDOUBLE,
  LCT_END
};

/* An array of all types that are used by the runtime functions we need.  */

static tree libcall_types[LCT_END];

/* Internal list of library functions.  */

static tree libcall_decls[A68_LIBCALL_LAST];

/* Return the TREE type that is described by TYPE.  */

static tree
get_libcall_type (a68_libcall_type type)
{
  if (libcall_types[type])
    return libcall_types[type];

  if (type == LCT_VOID)
    libcall_types[type] = void_type_node;
  else if (type == LCT_CHAR)
    libcall_types[type] = uint32_type_node;
  else if (type == LCT_CONSTCHARPTR)
    libcall_types[type] = build_pointer_type (build_qualified_type (char_type_node,
								    TYPE_QUAL_CONST));
  else if (type == LCT_VOIDPTR)
    libcall_types[type] = ptr_type_node;
  else if (type == LCT_UNISTR)
    libcall_types[type] = build_pointer_type (a68_char_type);
  else if (type == LCT_UNISTRPTR)
    libcall_types[type] = build_pointer_type (build_pointer_type (a68_char_type));
  else if (type == LCT_SIZE)
    libcall_types[type] = sizetype;
  else if (type == LCT_SSIZE)
    libcall_types[type] = ssizetype;
  else if (type == LCT_SIZEPTR)
    libcall_types[type] = build_pointer_type (sizetype);
  else if (type == LCT_UINT)
    libcall_types[type] = unsigned_type_node;
  else if (type == LCT_INT)
    libcall_types[type] = integer_type_node;
  else if (type == LCT_LONGLONGINT)
    libcall_types[type] = long_long_integer_type_node;
  else if (type == LCT_FLOAT)
    libcall_types[type] = float_type_node;
  else if (type == LCT_DOUBLE)
    libcall_types[type] = double_type_node;
  else if (type == LCT_LONGDOUBLE)
    libcall_types[type] = long_double_type_node;
  else
    gcc_unreachable ();

  return libcall_types[type];
}

/* Build and return a function declaration named NAME.  The RETURN_TYPE is the
   type returned, FLAGS are the expression call flags, and NPARAMS is the
   number of arguments, the types of which are provided in `...'.  */

static tree
build_libcall_decl (const char *name, a68_libcall_type return_type,
		    int flags, int nparams, ...)
{
  tree *args = XALLOCAVEC (tree, nparams);
  bool varargs = false;
  tree fntype;

  /* Add parameter types, using `void' as the last parameter type
     to mean this function accepts a variable list of arguments.  */
  va_list ap;
  va_start (ap, nparams);

  for (int i = 0; i < nparams; i++)
    {
      a68_libcall_type ptype = (a68_libcall_type) va_arg (ap, int);
      tree type = get_libcall_type (ptype);

      if (type == void_type_node)
	{
	  varargs = true;
	  nparams = i;
	}
      else
	args[i] = type;
    }

  va_end (ap);

  /* Build the function.  */
  tree tret = get_libcall_type (return_type);
  if (varargs)
    fntype = build_varargs_function_type_array (tret, nparams, args);
  else
    fntype = build_function_type_array (tret, nparams, args);

  tree decl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL,
			  get_identifier (name), fntype);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;

  /* Set any attributes on the function, such as malloc or noreturn.  */
  set_call_expr_flags (decl, flags);
  return decl;
}

/* Return or create the runtime library function declaration for LIBCALL.
   Library functions are generated as needed.  This could probably be changed
   in the future to be done in the compiler init stage, like GCC builtin trees
   are.  */

tree
a68_get_libcall (a68_libcall_fn libcall)
{
  if (libcall_decls[libcall])
    return libcall_decls[libcall];

  switch (libcall)
    {
#define DEF_A68_RUNTIME(CODE, NAME, TYPE, PARAMS, FLAGS) \
    case A68_LIBCALL_ ## CODE:	\
      libcall_decls[libcall] = build_libcall_decl (NAME, TYPE, FLAGS, PARAMS); \
      break;
#include "a68-low-runtime.def"
#undef DEF_A68_RUNTIME
    default:
      gcc_unreachable ();
    }

  return libcall_decls[libcall];
}

/* Generate a call to LIBCALL, returning the result as TYPE.  NARGS is the
   number of call arguments, the expressions of which are provided in `...'.
   This does not perform conversions or promotions on the arguments.  */

tree
a68_build_libcall (a68_libcall_fn libcall, tree type ATTRIBUTE_UNUSED,
		   int nargs, ...)
{
  /* Build the call expression to the runtime function.  */
  tree decl = a68_get_libcall (libcall);
  tree *args = XALLOCAVEC (tree, nargs);
  va_list ap;

  va_start (ap, nargs);
  for (int i = 0; i < nargs; i++)
    args[i] = va_arg (ap, tree);
  va_end (ap);

  tree result = build_call_expr_loc_array (input_location, decl, nargs, args);

  /* Assumes caller knows what it is doing.  */
  return result;
}
