/* jit-builtins.c -- Handling of builtin functions during JIT-compilation.
   Copyright (C) 2014-2021 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "jit-playback.h"
#include "stringpool.h"

#include "jit-builtins.h"

namespace gcc {

namespace jit {

const char *const prefix = "__builtin_";
const size_t prefix_len = strlen (prefix);

/* Create "builtin_data", a const table of the data within builtins.def.  */
struct builtin_data
{
  const char *name;
  enum built_in_class fnclass;
  enum jit_builtin_type type;
  bool both_p;
  bool fallback_p;
  enum built_in_attribute attr;
  bool implicit_p;

  const char *get_asm_name () const
  {
    if (both_p && fallback_p)
      return name + prefix_len;
    else
      return name;
  }
};

#define DEF_BUILTIN(X, NAME, CLASS, TYPE, LT, BOTH_P, FALLBACK_P, \
		    NONANSI_P, ATTRS, IMPLICIT, COND)		  \
  {NAME, CLASS, TYPE, BOTH_P, FALLBACK_P, ATTRS, IMPLICIT},
static const struct builtin_data builtin_data[] =
{
#include "builtins.def"
};

/* Helper function for find_builtin_by_name.  */

static bool
matches_builtin (const char *in_name,
		 const struct builtin_data& bd)
{
  const bool debug = 0;

  /* Ignore entries with a NULL name.  */
  if (!bd.name)
    return false;

  if (debug)
    fprintf (stderr, "seen builtin: %s\n", bd.name);

  if (strcmp (bd.name, in_name) == 0)
    return true;

  if (bd.both_p)
    {
      /* Then the macros in builtins.def gave a "__builtin_"
	 prefix to bd.name, but we should also recognize the form
	 without the prefix.  */
      gcc_assert (strncmp (bd.name, prefix, prefix_len) == 0);
      if (debug)
	fprintf (stderr, "testing without prefix as: %s\n",
		 bd.name + prefix_len);
      if (strcmp (bd.name + prefix_len, in_name) == 0)
	return true;
    }

  return false;
}

/* Locate the built-in function that matches name IN_NAME,
   writing the result to OUT_ID and returning true if found,
   or returning false if not found.  */

static bool
find_builtin_by_name (const char *in_name,
		      enum built_in_function *out_id)
{
  /* Locate builtin.  This currently works by performing repeated
     strcmp against every possible candidate, which is likely to
     inefficient.

     We start at index 1 to skip the initial entry (BUILT_IN_NONE), which
     has a NULL name.  */
  for (unsigned int i = 1;
       i < sizeof (builtin_data) / sizeof (builtin_data[0]);
       i++)
    {
      const struct builtin_data& bd = builtin_data[i];
      if (matches_builtin (in_name, bd))
	{
	  /* Found a match.  */
	  *out_id = static_cast<enum built_in_function> (i);
	  return true;
	}
    }

  /* Not found.  */
  return false;
}

// class builtins_manager

/* Constructor for gcc::jit::builtins_manager.  */

builtins_manager::builtins_manager (recording::context *ctxt)
  : m_ctxt (ctxt)
{
  memset (m_types, 0, sizeof (m_types));
  memset (m_builtin_functions, 0, sizeof (m_builtin_functions));
  memset (m_attributes, 0, sizeof (m_attributes));
}

/* Locate a builtin function by name.
   Create a recording::function of the appropriate type, reusing them
   if they've already been seen.  */

recording::function *
builtins_manager::get_builtin_function (const char *name)
{
  enum built_in_function builtin_id;
  if (!find_builtin_by_name (name, &builtin_id))
    {
      m_ctxt->add_error (NULL, "builtin \"%s\" not found", name);
      return NULL;
    }

  return get_builtin_function_by_id (builtin_id);
}

/* Locate a builtin function by id.
   Create a recording::function of the appropriate type, reusing them
   if they've already been seen.  */

recording::function *
builtins_manager::get_builtin_function_by_id (enum built_in_function builtin_id)
{
  gcc_assert (builtin_id > BUILT_IN_NONE);
  gcc_assert (builtin_id < END_BUILTINS);

  /* Lazily build the functions, caching them so that repeated calls for
     the same id on a context give back the same object.  */
  if (!m_builtin_functions[builtin_id])
    {
      recording::function *fn = make_builtin_function (builtin_id);
      if (fn)
	{
	  m_builtin_functions[builtin_id] = fn;
	  m_ctxt->record (fn);
	}
    }

  return m_builtin_functions[builtin_id];
}

/* Create the recording::function for a given builtin function, by ID.  */

recording::function *
builtins_manager::make_builtin_function (enum built_in_function builtin_id)
{
  const struct builtin_data& bd = builtin_data[builtin_id];
  enum jit_builtin_type type_id = bd.type;
  recording::type *t = get_type (type_id);
  if (!t)
    return NULL;
  recording::function_type *func_type = t->as_a_function_type ();
  if (!func_type)
    return NULL;

  vec<recording::type *> param_types = func_type->get_param_types ();
  recording::param **params = new recording::param *[param_types.length ()];

  int i;
  recording::type *param_type;
  FOR_EACH_VEC_ELT (param_types, i, param_type)
    {
      char buf[16];
      snprintf (buf, 16, "arg%d", i);
      params[i] = m_ctxt->new_param (NULL,
				     param_type,
				     buf);
    }
  const char *asm_name = bd.get_asm_name ();
  recording::function *result =
    new recording::function (m_ctxt,
			     NULL,
			     GCC_JIT_FUNCTION_IMPORTED, // FIXME
			     func_type->get_return_type (),
			     m_ctxt->new_string (asm_name),
			     param_types.length (),
			     params,
			     func_type->is_variadic (),
			     builtin_id);
  delete[] params;

  /* PR/64020 - If the client code is using builtin cos or sin,
     tree-ssa-math-opt.c's execute_cse_sincos_1 may attempt
     to optimize them to use __builtin_cexpi; for this,
     BUILT_IN_CEXPI needs to exist.

     Hence query the cache for BUILT_IN_CEXPI to ensure it gets
     built.  */
  if (builtin_id == BUILT_IN_COS || builtin_id == BUILT_IN_SIN)
    (void)get_builtin_function_by_id (BUILT_IN_CEXPI);

  /* builtins.c:expand_builtin_cexpi can optimize the various
     CEXP builtins to SINCOS builtins, and hence we may require
     SINCOS builtins latter.

     Ensure the appropriate SINCOS builtin exists.  */
  if (builtin_id == BUILT_IN_CEXPIF)
    (void)get_builtin_function_by_id (BUILT_IN_SINCOSF);
  else if (builtin_id == BUILT_IN_CEXPI)
    (void)get_builtin_function_by_id (BUILT_IN_SINCOS);
  else if (builtin_id == BUILT_IN_CEXPIL)
    (void)get_builtin_function_by_id (BUILT_IN_SINCOSL);

  return result;
}

/* Build an array of type names for use by get_string_for_type_id.  */

static const char * const type_names[] = {
#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) #ENUM,
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN) #ENUM,
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1) #ENUM,
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2) #ENUM,
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3) #ENUM,
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) #ENUM,
#define DEF_FUNCTION_TYPE_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) #ENUM,
#define DEF_FUNCTION_TYPE_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6)					\
					  #ENUM,
#define DEF_FUNCTION_TYPE_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7)					\
					  #ENUM,
#define DEF_FUNCTION_TYPE_8(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8)				\
					  #ENUM,
#define DEF_FUNCTION_TYPE_9(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9)			\
					  #ENUM,
#define DEF_FUNCTION_TYPE_10(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10)		 \
					  #ENUM,
#define DEF_FUNCTION_TYPE_11(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10, ARG11)	 \
					  #ENUM,
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN) #ENUM,
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1) #ENUM,
#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2) #ENUM,
#define DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, ARG1, ARG2, ARG3) #ENUM,
#define DEF_FUNCTION_TYPE_VAR_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) #ENUM,
#define DEF_FUNCTION_TYPE_VAR_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
					  #ENUM,
#define DEF_FUNCTION_TYPE_VAR_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6)					\
					  #ENUM,
#define DEF_FUNCTION_TYPE_VAR_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7)				\
					  #ENUM,
#define DEF_POINTER_TYPE(ENUM, TYPE) #ENUM,

#include "builtin-types.def"

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
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_POINTER_TYPE
};

/* Get a string for TYPE_ID suitable for use in logs and error messages
   (e.g. "BT_PID").  */

static const char *
get_string_for_type_id (enum jit_builtin_type type_id)
{
  gcc_assert (type_id < sizeof (type_names)/sizeof(type_names[0]));
  return type_names[type_id];
}

/* Get the recording::type for a given type of builtin function,
   by ID, creating it if it doesn't already exist.  */

recording::type *
builtins_manager::get_type (enum jit_builtin_type type_id)
{
  if (!m_types[type_id])
    m_types[type_id] = make_type (type_id);
  return m_types[type_id];
}

/* Create the recording::type for a given type of builtin function.  */

recording::type *
builtins_manager::make_type (enum jit_builtin_type type_id)
{
  /* Use builtin-types.def to construct a switch statement, with each
     case deferring to one of the methods below:
       - DEF_PRIMITIVE_TYPE is handled as a call to make_primitive_type.
       - the various DEF_FUNCTION_TYPE_n are handled by variadic calls
	 to make_fn_type.
       - similarly for DEF_FUNCTION_TYPE_VAR_n, but setting the
	"is_variadic" argument.
       - DEF_POINTER_TYPE is handled by make_ptr_type.
     That should handle everything, but just in case we also suppy a
     gcc_unreachable default clause.  */
  switch (type_id)
    {
#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) \
      case ENUM: return make_primitive_type (ENUM);
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN) \
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 0);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1) \
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 1, ARG1);
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2) \
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 4, ARG1, ARG2, ARG3, \
				      ARG4);
#define DEF_FUNCTION_TYPE_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 5, ARG1, ARG2, ARG3, \
				      ARG4, ARG5);
#define DEF_FUNCTION_TYPE_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6)					\
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 6, ARG1, ARG2, ARG3, \
				      ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7)					\
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 7, ARG1, ARG2, ARG3, \
				      ARG4, ARG5, ARG6, ARG7);
#define DEF_FUNCTION_TYPE_8(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8)				\
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 8, ARG1, ARG2, ARG3, \
				      ARG4, ARG5, ARG6, ARG7, ARG8);
#define DEF_FUNCTION_TYPE_9(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9)			\
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 9, ARG1, ARG2, ARG3, \
				      ARG4, ARG5, ARG6, ARG7, ARG8, ARG9);
#define DEF_FUNCTION_TYPE_10(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10)		 \
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 10, ARG1, ARG2, ARG3, \
				      ARG4, ARG5, ARG6, ARG7, ARG8, ARG9, \
				      ARG10);
#define DEF_FUNCTION_TYPE_11(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10, ARG11)	 \
      case ENUM: return make_fn_type (ENUM, RETURN, 0, 11, ARG1, ARG2, ARG3, \
				      ARG4, ARG5, ARG6, ARG7, ARG8, ARG9, \
				      ARG10, ARG11);
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN) \
      case ENUM: return make_fn_type (ENUM, RETURN, 1, 0);
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1) \
      case ENUM: return make_fn_type (ENUM, RETURN, 1, 1, ARG1);
#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2) \
      case ENUM: return make_fn_type (ENUM, RETURN, 1, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
      case ENUM: return make_fn_type (ENUM, RETURN, 1, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_VAR_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
      case ENUM: return make_fn_type (ENUM, RETURN, 1, 4, ARG1, ARG2, ARG3, \
				      ARG4);
#define DEF_FUNCTION_TYPE_VAR_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
      case ENUM: return make_fn_type (ENUM, RETURN, 1, 5, ARG1, ARG2, ARG3, \
				      ARG4, ARG5);
#define DEF_FUNCTION_TYPE_VAR_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6)					\
      case ENUM: return make_fn_type (ENUM, RETURN, 1, 6, ARG1, ARG2, ARG3, \
				      ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_VAR_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7)				\
      case ENUM: return make_fn_type (ENUM, RETURN, 1, 7, ARG1, ARG2, ARG3, \
				      ARG4, ARG5, ARG6, ARG7);
#define DEF_POINTER_TYPE(ENUM, TYPE) \
      case ENUM: return make_ptr_type (ENUM, TYPE);

#include "builtin-types.def"

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
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_POINTER_TYPE

    default:
      gcc_unreachable ();
    }
}

/* Create the recording::type for a given primitive type within the
   builtin system.

   Only some types are currently supported.  */

recording::type*
builtins_manager::make_primitive_type (enum jit_builtin_type type_id)
{
  switch (type_id)
    {
    default:
      // only some of these types are implemented so far:
      m_ctxt->add_error (NULL,
			 "unimplemented primitive type for builtin (type: %s)",
			 get_string_for_type_id (type_id));
      return NULL;

    case BT_VOID: return m_ctxt->get_type (GCC_JIT_TYPE_VOID);
    case BT_BOOL: return m_ctxt->get_type (GCC_JIT_TYPE_BOOL);
    case BT_INT: return m_ctxt->get_type (GCC_JIT_TYPE_INT);
    case BT_UINT: return m_ctxt->get_type (GCC_JIT_TYPE_UNSIGNED_INT);
    case BT_LONG: return m_ctxt->get_type (GCC_JIT_TYPE_LONG);
    case BT_ULONG: return m_ctxt->get_type (GCC_JIT_TYPE_UNSIGNED_LONG);
    case BT_LONGLONG: return m_ctxt->get_type (GCC_JIT_TYPE_LONG_LONG);
    case BT_ULONGLONG:
      return m_ctxt->get_type (GCC_JIT_TYPE_UNSIGNED_LONG_LONG);
    // case BT_INTMAX:
    // case BT_UINTMAX:
    case BT_INT8: return m_ctxt->get_int_type (1, true);
    case BT_INT16: return m_ctxt->get_int_type (2, true);
    case BT_UINT8: return m_ctxt->get_int_type (1, false);
    case BT_UINT16: return m_ctxt->get_int_type (2, false);
    case BT_UINT32: return m_ctxt->get_int_type (4, false);
    case BT_UINT64: return m_ctxt->get_int_type (8, false);
    // case BT_WORD:
    // case BT_UNWINDWORD:
    case BT_FLOAT: return m_ctxt->get_type (GCC_JIT_TYPE_FLOAT);
    case BT_DOUBLE: return m_ctxt->get_type (GCC_JIT_TYPE_DOUBLE);
    case BT_LONGDOUBLE: return m_ctxt->get_type (GCC_JIT_TYPE_LONG_DOUBLE);
    // case BT_FLOAT16:
    // case BT_FLOAT32:
    // case BT_FLOAT64:
    // case BT_FLOAT128:
    // case BT_FLOAT32X:
    // case BT_FLOAT64X:
    // case BT_FLOAT128X:
    case BT_COMPLEX_FLOAT:
      return m_ctxt->get_type (GCC_JIT_TYPE_COMPLEX_FLOAT);
    case BT_COMPLEX_DOUBLE:
      return m_ctxt->get_type (GCC_JIT_TYPE_COMPLEX_DOUBLE);
    case BT_COMPLEX_LONGDOUBLE:
      return m_ctxt->get_type (GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE);
    case BT_PTR: return m_ctxt->get_type (GCC_JIT_TYPE_VOID_PTR);
    case BT_FILEPTR: return m_ctxt->get_type (GCC_JIT_TYPE_FILE_PTR);
    // case BT_CONST_TM_PTR:
    // case BT_FENV_T_PTR:
    // case BT_CONST_FENV_T_PTR:
    // case BT_FEXCEPT_T_PTR:
    // case BT_CONST_FEXCEPT_T_PTR:
    case BT_CONST_PTR:
      return m_ctxt->get_type (GCC_JIT_TYPE_VOID)->get_const ()->get_pointer ();
    case BT_VOLATILE_PTR:
      return (m_ctxt->get_type (GCC_JIT_TYPE_VOID)->get_volatile ()
	      ->get_pointer ());
    case BT_CONST_VOLATILE_PTR:
      return (m_ctxt->get_type (GCC_JIT_TYPE_VOID)->get_const ()
	      ->get_volatile ()->get_pointer ());
    // case BT_PTRMODE:
    case BT_INT_PTR:
      return m_ctxt->get_type (GCC_JIT_TYPE_INT)->get_pointer ();
    case BT_FLOAT_PTR:
      return m_ctxt->get_type (GCC_JIT_TYPE_FLOAT)->get_pointer ();
    case BT_DOUBLE_PTR:
      return m_ctxt->get_type (GCC_JIT_TYPE_DOUBLE)->get_pointer ();
    case BT_CONST_DOUBLE_PTR:
      return (m_ctxt->get_type (GCC_JIT_TYPE_DOUBLE)->get_const ()
	      ->get_pointer ());
    // case BT_LONGDOUBLE_PTR:
    // case BT_PID:
    case BT_SIZE:
      return m_ctxt->get_type (GCC_JIT_TYPE_SIZE_T);
    case BT_CONST_SIZE:
      return m_ctxt->get_type (GCC_JIT_TYPE_SIZE_T)->get_const ();
    // case BT_SSIZE:
    // case BT_WINT:
    // case BT_STRING:
    case BT_CONST_STRING: return m_ctxt->get_type (GCC_JIT_TYPE_CONST_CHAR_PTR);
    // case BT_DFLOAT32:
    // case BT_DFLOAT64:
    // case BT_DFLOAT128:
    // case BT_VALIST_REF:
    // case BT_VALIST_ARG:
    // case BT_I1:
    // case BT_I2:
    // case BT_I4:
    // case BT_I8:
    // case BT_I16:
    // case BT_PTR_CONST_STRING:
    }
}

/* Create the recording::function_type for a given function type
   signature.  */

recording::function_type *
builtins_manager::make_fn_type (enum jit_builtin_type,
				enum jit_builtin_type return_type_id,
				bool is_variadic,
				int num_args, ...)
{
  va_list list;
  int i;
  recording::type **param_types = new recording::type *[num_args];
  recording::type *return_type = NULL;
  recording::function_type *result = NULL;

  va_start (list, num_args);
  for (i = 0; i < num_args; ++i)
    {
      enum jit_builtin_type arg_type_id =
	(enum jit_builtin_type) va_arg (list, int);
      param_types[i] = get_type (arg_type_id);
      if (!param_types[i])
	goto error;
    }
  va_end (list);

  return_type = get_type (return_type_id);
  if (!return_type)
    goto error;

  result = m_ctxt->new_function_type (return_type,
				      num_args,
				      param_types,
				      is_variadic);

 error:
  delete[] param_types;
  return result;
}

/* Handler for DEF_POINTER_TYPE within builtins_manager::make_type.  */

recording::type *
builtins_manager::make_ptr_type (enum jit_builtin_type,
				 enum jit_builtin_type other_type_id)
{
  recording::type *base_type = get_type (other_type_id);
  return base_type->get_pointer ();
}

/* Ensure that builtins that could be needed during optimization
   get created ahead of time.  */

void
builtins_manager::ensure_optimization_builtins_exist ()
{
  /* build_common_builtin_nodes does most of this, but not all.
     We can't loop through all of the builtin_data array, we don't
     support all types yet.  */
  (void)get_builtin_function_by_id (BUILT_IN_TRAP);
}

/* Playback support.  */

/* A builtins_manager is associated with a recording::context
   and might be reused for multiple compiles on various
   playback::contexts, perhaps with different options.

   Purge any playback state.  Currently this is just the table of
   attributes.  */

void
builtins_manager::finish_playback (void)
{
  memset (m_attributes, 0, sizeof (m_attributes));
}

/* Get the enum built_in_class for BUILTIN_ID.  */

enum built_in_class
builtins_manager::get_class (enum built_in_function builtin_id)
{
  return builtin_data[builtin_id].fnclass;
}

/* Is BUILTIN_ID implicit?  */

bool
builtins_manager::implicit_p (enum built_in_function builtin_id)
{
  return builtin_data[builtin_id].implicit_p;
}

/* Get any attributes (in tree form) for the function declaration
   for BUILTIN_ID.

   These are created on-demand, and cached within the m_attributes
   array, until finish_playback.  */

tree
builtins_manager::get_attrs_tree (enum built_in_function builtin_id)
{
  enum built_in_attribute attr = builtin_data[builtin_id].attr;
  return get_attrs_tree (attr);
}

/* As above, but for an enum built_in_attribute.  */

tree
builtins_manager::get_attrs_tree (enum built_in_attribute attr)
{
  gcc_assert (attr < ATTR_LAST);
  if (!m_attributes [attr])
    m_attributes [attr] = make_attrs_tree (attr);
  return m_attributes [attr];
}

/* Handle a cache-miss within the m_attributes array by
   generating the attributes for enum built_in_attribute
   in tree form.  */

tree
builtins_manager::make_attrs_tree (enum built_in_attribute attr)
{
  switch (attr)
    {
      /* Generate cases from builtin-attrs.def.  */
#define DEF_ATTR_NULL_TREE(ENUM)				\
      case ENUM: return NULL_TREE;
#define DEF_ATTR_INT(ENUM, VALUE)				\
      case ENUM: return build_int_cst (integer_type_node, VALUE);
#define DEF_ATTR_STRING(ENUM, VALUE)				\
      case ENUM: return build_string (strlen (VALUE), VALUE);
#define DEF_ATTR_IDENT(ENUM, STRING)				\
      case ENUM: return get_identifier (STRING);
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN)	\
      case ENUM: return tree_cons (get_attrs_tree (PURPOSE),	\
				   get_attrs_tree (VALUE),	\
				   get_attrs_tree (CHAIN));
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST

    default:
      /* We somehow got a value not covered by the autogenerated
	 cases.  */
      gcc_unreachable ();
      return NULL;
    }
}

} // namespace jit
} // namespace gcc
