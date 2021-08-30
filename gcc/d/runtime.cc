/* runtime.cc -- D runtime functions called by generated code.
   Copyright (C) 2006-2021 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/aggregate.h"
#include "dmd/mtype.h"

#include "tree.h"
#include "fold-const.h"
#include "stringpool.h"

#include "d-tree.h"


/* During the codegen pass, the compiler may do lowering of expressions to call
   various runtime library functions.  Most are implemented in the `rt' package.
   We represent them in the frontend here, however there's no guarantee that
   the compiler implementation actually matches the actual implementation.  */

enum d_libcall_type
{
  LCT_VOID,		    /* void		    */
  LCT_BYTE,		    /* byte		    */
  LCT_INT,		    /* int		    */
  LCT_UINT,		    /* uint		    */
  LCT_BOOL,		    /* bool		    */
  LCT_DCHAR,		    /* dchar		    */
  LCT_VOIDPTR,		    /* void*		    */
  LCT_STRING,		    /* string		    */
  LCT_WSTRING,		    /* wstring		    */
  LCT_DSTRING,		    /* dstring		    */
  LCT_SIZE_T,		    /* size_t		    */
  LCT_ASSOCARRAY,	    /* void[void]	    */
  LCT_ARRAY_VOID,	    /* void[]		    */
  LCT_ARRAY_SIZE_T,	    /* size_t[]		    */
  LCT_ARRAY_BYTE,	    /* byte[]		    */
  LCT_IMMUTABLE_CHARPTR,    /* immutable(char*)	    */
  LCT_ARRAY_STRING,	    /* string[]		    */
  LCT_ARRAY_WSTRING,	    /* wstring[]	    */
  LCT_ARRAY_DSTRING,	    /* dstring[]	    */
  LCT_ARRAYARRAY_BYTE,	    /* byte[][]		    */
  LCT_POINTER_ASSOCARRAY,   /* void[void]*	    */
  LCT_POINTER_VOIDPTR,	    /* void**		    */
  LCT_ARRAYPTR_VOID,	    /* void[]*		    */
  LCT_ARRAYPTR_BYTE,	    /* byte[]*		    */
  LCT_TYPEINFO,		    /* TypeInfo		    */
  LCT_CLASSINFO,	    /* TypeInfo_Class	    */
  LCT_OBJECT,		    /* Object		    */
  LCT_CONST_TYPEINFO,	    /* const(TypeInfo)	    */
  LCT_CONST_CLASSINFO,	    /* const(ClassInfo)	    */
  LCT_END
};

/* An array of all types that are used by the runtime functions we need.  */

static Type *libcall_types[LCT_END];

/* Our internal list of library functions.  */

static tree libcall_decls[LIBCALL_LAST];


/* Return the frontend Type that is described by TYPE.  Most are readily cached
   by the frontend proper, and likewise the use of pointerTo(), constOf(), and
   arrayOf() will return cached types if they have been requested before.  */

static Type *
get_libcall_type (d_libcall_type type)
{
  if (libcall_types[type])
    return libcall_types[type];

  switch (type)
    {
    case LCT_VOID:
      libcall_types[type] = Type::tvoid;
      break;

    case LCT_BYTE:
      libcall_types[type] = Type::tint8;
      break;

    case LCT_INT:
      libcall_types[type] = Type::tint32;
      break;

    case LCT_UINT:
      libcall_types[type] = Type::tuns32;
      break;

    case LCT_BOOL:
      libcall_types[type] = Type::tbool;
      break;

    case LCT_DCHAR:
      libcall_types[type] = Type::tdchar;
      break;

    case LCT_VOIDPTR:
      libcall_types[type] = Type::tvoidptr;
      break;

    case LCT_STRING:
      libcall_types[type] = Type::tstring;
      break;

    case LCT_WSTRING:
      libcall_types[type] = Type::twstring;
      break;

    case LCT_DSTRING:
      libcall_types[type] = Type::tdstring;
      break;

    case LCT_SIZE_T:
      libcall_types[type] = Type::tsize_t;
      break;

    case LCT_ASSOCARRAY:
      libcall_types[type] = TypeAArray::create (Type::tvoid, Type::tvoid);
      break;

    case LCT_TYPEINFO:
      libcall_types[type] = Type::dtypeinfo->type;
      break;

    case LCT_CLASSINFO:
      libcall_types[type] = Type::typeinfoclass->type;
      break;

    case LCT_OBJECT:
      libcall_types[type] = get_object_type ();
      break;

    case LCT_CONST_TYPEINFO:
      libcall_types[type] = Type::dtypeinfo->type->constOf ();
      break;

    case LCT_CONST_CLASSINFO:
      libcall_types[type] = Type::typeinfoclass->type->constOf ();
      break;

    case LCT_ARRAY_VOID:
      libcall_types[type] = Type::tvoid->arrayOf ();
      break;

    case LCT_ARRAY_SIZE_T:
      libcall_types[type] = Type::tsize_t->arrayOf ();
      break;

    case LCT_ARRAY_BYTE:
      libcall_types[type] = Type::tint8->arrayOf ();
      break;

    case LCT_ARRAY_STRING:
      libcall_types[type] = Type::tstring->arrayOf ();
      break;

    case LCT_ARRAY_WSTRING:
      libcall_types[type] = Type::twstring->arrayOf ();
      break;

    case LCT_ARRAY_DSTRING:
      libcall_types[type] = Type::tdstring->arrayOf ();
      break;

    case LCT_ARRAYARRAY_BYTE:
      libcall_types[type] = Type::tint8->arrayOf ()->arrayOf ();
      break;

    case LCT_POINTER_ASSOCARRAY:
      libcall_types[type] = get_libcall_type (LCT_ASSOCARRAY)->pointerTo ();
      break;

    case LCT_POINTER_VOIDPTR:
      libcall_types[type] = Type::tvoidptr->arrayOf ();
      break;

    case LCT_ARRAYPTR_VOID:
      libcall_types[type] = Type::tvoid->arrayOf ()->pointerTo ();
      break;

    case LCT_ARRAYPTR_BYTE:
      libcall_types[type] = Type::tint8->arrayOf ()->pointerTo ();
      break;

    case LCT_IMMUTABLE_CHARPTR:
      libcall_types[type] = Type::tchar->pointerTo ()->immutableOf ();
      break;

    default:
      gcc_unreachable ();
    }

  return libcall_types[type];
}

/* Builds and returns function declaration named NAME.  The RETURN_TYPE is
   the type returned, FLAGS are the expression call flags, and NPARAMS is
   the number of arguments, the types of which are provided in `...'.  */

static tree
build_libcall_decl (const char *name, d_libcall_type return_type,
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
      d_libcall_type ptype = (d_libcall_type) va_arg (ap, int);
      Type *type = get_libcall_type (ptype);

      if (type == Type::tvoid)
	{
	  varargs = true;
	  nparams = i;
	}
      else
	args[i] = build_ctype (type);
    }

  va_end (ap);

  /* Build the function.  */
  tree tret = build_ctype (get_libcall_type (return_type));
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
   Library functions are generated as needed.  This could probably be changed in
   the future to be done in the compiler init stage, like GCC builtin trees are,
   however we depend on run-time initialization of types whose definitions are
   in the library such as `Object' or `TypeInfo'.  */

static tree
get_libcall (libcall_fn libcall)
{
  if (libcall_decls[libcall])
    return libcall_decls[libcall];

  switch (libcall)
    {
#define DEF_D_RUNTIME(CODE, NAME, TYPE, PARAMS, FLAGS) \
    case LIBCALL_ ## CODE:	\
      libcall_decls[libcall] = build_libcall_decl (NAME, TYPE, FLAGS, PARAMS); \
      break;

#include "runtime.def"

#undef DEF_D_RUNTIME

    default:
      gcc_unreachable ();
    }

  return libcall_decls[libcall];
}

/* Generate a call to LIBCALL, returning the result as TYPE.  NARGS is the
   number of call arguments, the expressions of which are provided in `...'.
   This does not perform conversions or promotions on the arguments.  */

tree
build_libcall (libcall_fn libcall, Type *type, int nargs, ...)
{
  /* Build the call expression to the runtime function.  */
  tree decl = get_libcall (libcall);
  tree *args = XALLOCAVEC (tree, nargs);
  va_list ap;

  va_start (ap, nargs);
  for (int i = 0; i < nargs; i++)
    args[i] = va_arg (ap, tree);
  va_end (ap);

  tree result = build_call_expr_loc_array (input_location, decl, nargs, args);

  /* Assumes caller knows what it is doing.  */
  return convert (build_ctype (type), result);
}
