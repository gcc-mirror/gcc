/* d-target.cc -- Target interface for the D front end.
   Copyright (C) 2013-2023 Free Software Foundation, Inc.

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
#include "dmd/declaration.h"
#include "dmd/expression.h"
#include "dmd/mangle.h"
#include "dmd/mtype.h"
#include "dmd/tokens.h"
#include "dmd/target.h"

#include "tree.h"
#include "memmodel.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "stor-layout.h"
#include "tm.h"
#include "tm_p.h"
#include "target.h"
#include "calls.h"

#include "d-tree.h"
#include "d-target.h"

/* Implements the Target interface defined by the front end.
   Used for retrieving target-specific information.  */

/* Internal key handlers for `__traits(getTargetInfo)'.  */
static tree d_handle_target_cpp_std (void);
static tree d_handle_target_cpp_runtime_library (void);
static tree d_handle_target_object_format (void);

/* In [traits/getTargetInfo], a reliable subset of getTargetInfo keys exists
   which are always available.  */
static const struct d_target_info_spec d_language_target_info[] =
{
  /* { name, handler } */
  { "cppStd", d_handle_target_cpp_std },
  { "cppRuntimeLibrary", d_handle_target_cpp_runtime_library },
  { "floatAbi", NULL },
  { "objectFormat", d_handle_target_object_format },
  { NULL, NULL },
};

/* Table `__traits(getTargetInfo)' keys.  */
static vec<d_target_info_spec> d_target_info_table;


/* Initialize the floating-point constants for TYPE.  */

template <typename T>
static void
define_float_constants (T &f, tree type)
{
  const double log10_2 = 0.30102999566398119521;
  char buf[128];

  /* Get back-end real mode format.  */
  const machine_mode mode = TYPE_MODE (type);
  const real_format *fmt = REAL_MODE_FORMAT (mode);

  /* The largest representable value that's not infinity.  */
  get_max_float (fmt, buf, sizeof (buf), false);
  real_from_string (&f.max.rv (), buf);

  /* The smallest representable normalized value that's not 0.  */
  snprintf (buf, sizeof (buf), "0x1p%d", fmt->emin - 1);
  real_from_string (&f.min_normal.rv (), buf);

  /* Floating-point NaN.  */
  real_nan (&f.nan.rv (), "", 1, mode);

  /* Floating-point +Infinity if the target supports infinities.  */
  real_inf (&f.infinity.rv ());

  /* The smallest increment to the value 1.  */
  if (fmt->pnan < fmt->p)
    snprintf (buf, sizeof (buf), "0x1p%d", fmt->emin - fmt->p);
  else
    snprintf (buf, sizeof (buf), "0x1p%d", 1 - fmt->p);
  real_from_string (&f.epsilon.rv (), buf);

  /* The number of decimal digits of precision.  */
  f.dig = (fmt->p - 1) * log10_2;

  /* The number of bits in mantissa.  */
  f.mant_dig = fmt->p;

  /* The maximum int value such that 2** (value-1) is representable.  */
  f.max_exp = fmt->emax;

  /* The minimum int value such that 2** (value-1) is representable as a
     normalized value.  */
  f.min_exp = fmt->emin;

  /* The maximum int value such that 10**value is representable.  */
  f.max_10_exp = fmt->emax * log10_2;

  /* The minimum int value such that 10**value is representable as a
     normalized value.  */
  f.min_10_exp = (fmt->emin - 1) * log10_2;
}

/* Initialize all variables of the Target structure.  */

void
Target::_init (const Param &)
{
  /* Map D frontend type and sizes to GCC back-end types.  */
  this->ptrsize = (POINTER_SIZE / BITS_PER_UNIT);
  this->realsize = int_size_in_bytes (long_double_type_node);
  this->realpad = (this->realsize -
		   (TYPE_PRECISION (long_double_type_node) / BITS_PER_UNIT));
  this->realalignsize = TYPE_ALIGN_UNIT (long_double_type_node);

  /* Much of the dmd front-end uses ints for sizes and offsets, and cannot
     handle any larger data type without some pervasive rework.  */
  this->maxStaticDataSize = tree_to_shwi (TYPE_MAX_VALUE (integer_type_node));

  /* Define what type to use for size_t, ptrdiff_t.  */
  if (this->ptrsize == 8)
    {
      this->isLP64 = true;
      Type::tsize_t = Type::basic[(int)TY::Tuns64];
      Type::tptrdiff_t = Type::basic[(int)TY::Tint64];
    }
  else if (this->ptrsize == 4)
    {
      Type::tsize_t = Type::basic[(int)TY::Tuns32];
      Type::tptrdiff_t = Type::basic[(int)TY::Tint32];
    }
  else if (this->ptrsize == 2)
    {
      Type::tsize_t = Type::basic[(int)TY::Tuns16];
      Type::tptrdiff_t = Type::basic[(int)TY::Tint16];
    }
  else
    sorry ("D does not support pointers on this target.");

  Type::thash_t = Type::tsize_t;

  /* Set-up target C ABI.  */
  this->c.boolsize = (BOOL_TYPE_SIZE / BITS_PER_UNIT);
  this->c.shortsize = (SHORT_TYPE_SIZE / BITS_PER_UNIT);
  this->c.intsize = (INT_TYPE_SIZE / BITS_PER_UNIT);
  this->c.longsize = (LONG_TYPE_SIZE / BITS_PER_UNIT);
  this->c.long_longsize = (LONG_LONG_TYPE_SIZE / BITS_PER_UNIT);
  this->c.long_doublesize = (LONG_DOUBLE_TYPE_SIZE / BITS_PER_UNIT);
  this->c.wchar_tsize = (WCHAR_TYPE_SIZE / BITS_PER_UNIT);

  this->c.bitFieldStyle = targetm.ms_bitfield_layout_p (unknown_type_node)
    ? TargetC::BitFieldStyle::MS : TargetC::BitFieldStyle::Gcc_Clang;

  /* Set-up target C++ ABI.  */
  this->cpp.reverseOverloads = false;
  this->cpp.exceptions = true;
  this->cpp.twoDtorInVtable = true;

  /* Set-up target Objective-C ABI.  */
  this->objc.supported = false;

  /* Set-up environmental settings.  */
  this->obj_ext = "o";
  this->lib_ext = "a";
  this->dll_ext = "so";
  this->run_noext = true;

  /* Initialize all compile-time properties for floating-point types.
     Should ensure that our real_t type is able to represent real_value.  */
  gcc_assert (sizeof (real_t) >= sizeof (real_value));

  define_float_constants (this->FloatProperties, float_type_node);
  define_float_constants (this->DoubleProperties, double_type_node);
  define_float_constants (this->RealProperties, long_double_type_node);

  /* Commonly used floating-point constants.  */
  const machine_mode mode = TYPE_MODE (long_double_type_node);
  real_convert (&CTFloat::zero.rv (), mode, &dconst0);
  real_convert (&CTFloat::one.rv (), mode, &dconst1);
  real_convert (&CTFloat::minusone.rv (), mode, &dconstm1);
  real_convert (&CTFloat::half.rv (), mode, &dconsthalf);

  /* Initialize target info tables, the keys required by the language are added
     last, so that the OS and CPU handlers can override.  */
  targetdm.d_register_cpu_target_info ();
  targetdm.d_register_os_target_info ();
  d_add_target_info_handlers (d_language_target_info);
}

/* Return GCC memory alignment size for type TYPE.  */

unsigned
Target::alignsize (Type *type)
{
  gcc_assert (type->isTypeBasic ());
  return min_align_of_type (build_ctype (type));
}

/* Return GCC field alignment size for type TYPE.  */

unsigned
Target::fieldalign (Type *type)
{
  /* Work out the correct alignment for the field decl.  */
  unsigned int align = type->alignsize () * BITS_PER_UNIT;

#ifdef BIGGEST_FIELD_ALIGNMENT
  align = MIN (align, (unsigned) BIGGEST_FIELD_ALIGNMENT);
#endif

#ifdef ADJUST_FIELD_ALIGN
  if (type->isTypeBasic ())
    align = ADJUST_FIELD_ALIGN (NULL_TREE, build_ctype (type), align);
#endif

  /* Also controlled by -fpack-struct=  */
  if (maximum_field_alignment)
    align = MIN (align, maximum_field_alignment);

  return align / BITS_PER_UNIT;
}

/* Returns a Type for the va_list type of the target.  */

Type *
Target::va_listType (const Loc &, Scope *)
{
  if (this->tvalist)
    return this->tvalist;

  /* Build the "standard" abi va_list.  */
  this->tvalist = build_frontend_type (va_list_type_node);
  if (!this->tvalist)
    sorry ("cannot represent built-in %<va_list%> type in D");

  /* Map the va_list type to the D frontend Type.  This is to prevent both
     errors in gimplification or an ICE in targetm.canonical_va_list_type.  */
  this->tvalist->ctype = va_list_type_node;
  TYPE_LANG_SPECIFIC (va_list_type_node) = build_lang_type (this->tvalist);

  return this->tvalist;
}

/* Checks whether the target supports a vector type with total size SZ
   (in bytes) and element type TYPE.  */

int
Target::isVectorTypeSupported (int sz, Type *type)
{
  /* Size must be greater than zero, and a power of two.  */
  if (sz <= 0 || sz & (sz - 1))
    return 3;

  /* __vector(void[]) is treated same as __vector(ubyte[])  */
  if (type == Type::tvoid)
    type = Type::tuns8;

  /* No support for non-trivial types, complex types, or booleans.  */
  if (!type->isTypeBasic () || type->iscomplex () || type->ty == TY::Tbool)
    return 2;

  /* In [simd/vector extensions], which vector types are supported depends on
     the target.  The implementation is expected to only support the vector
     types that are implemented in the target's hardware.  */
  unsigned HOST_WIDE_INT nunits = sz / type->size ();
  tree ctype = build_vector_type (build_ctype (type), nunits);

  if (!targetm.vector_mode_supported_p (TYPE_MODE (ctype)))
    return 2;

  return 0;
}

/* Checks whether the target supports operation OP for vectors of type TYPE.
   For binary ops T2 is the type of the right-hand operand.
   Returns true if the operation is supported or type is not a vector.  */

bool
Target::isVectorOpSupported (Type *type, EXP op, Type *)
{
  if (type->ty != TY::Tvector)
    return true;

  /* Don't support if type is non-scalar, such as __vector(void[]).  */
  if (!type->isscalar ())
    return false;

  /* Don't support if expression cannot be represented.  */
  switch (op)
    {
    case EXP::pow:
    case EXP::powAssign:
      /* pow() is lowered as a function call.  */
      return false;

    case EXP::mod:
    case EXP::modAssign:
      /* fmod() is lowered as a function call.  */
      if (type->isfloating ())
	return false;
      break;

    case EXP::andAnd:
    case EXP::orOr:
      /* Logical operators must have a result type of bool.  */
      return false;

    default:
      break;
    }

  return true;
}

/* Return the symbol mangling of S for C++ linkage.  */

const char *
TargetCPP::toMangle (Dsymbol *s)
{
  return toCppMangleItanium (s);
}

/* Return the symbol mangling of CD for C++ linkage.  */

const char *
TargetCPP::typeInfoMangle (ClassDeclaration *cd)
{
  return cppTypeInfoMangleItanium (cd);
}

/* Get mangle name of a this-adjusting thunk to the function declaration FD
   at call offset OFFSET for C++ linkage.  */

const char *
TargetCPP::thunkMangle (FuncDeclaration *fd, int offset)
{
  return cppThunkMangleItanium (fd, offset);
}

/* For a vendor-specific type, return a string containing the C++ mangling.
   In all other cases, return NULL.  */

const char *
TargetCPP::typeMangle (Type *type)
{
  if (type->isTypeBasic () || type->ty == TY::Tvector
      || type->ty == TY::Tstruct)
    {
      tree ctype = build_ctype (type);
      return targetm.mangle_type (ctype);
    }

  return NULL;
}

/* Return the type that will really be used for passing the given parameter
   ARG to an extern(C++) function.  */

Type *
TargetCPP::parameterType (Type *type)
{
  /* Could be a va_list, which we mangle as a pointer.  */
  Type *tvalist = target.va_listType (Loc (), NULL);
  if (type->ty == TY::Tsarray && tvalist->ty == TY::Tsarray)
    {
      Type *tb = type->toBasetype ()->mutableOf ();
      if (tb == tvalist)
	{
	  tb = type->nextOf ()->pointerTo ();
	  type = tb->castMod (type->mod);
	}
    }

  return type;
}

/* Checks whether TYPE is a vendor-specific fundamental type.  Stores the result
   in IS_FUNDAMENTAL and returns true if the parameter was set.  */

bool
TargetCPP::fundamentalType (const Type *, bool &)
{
  return false;
}

/* Get the starting offset position for fields of an `extern(C++)` class
   that is derived from the given BASE_CLASS.  */

unsigned
TargetCPP::derivedClassOffset(ClassDeclaration *base_class)
{
  return base_class->structsize;
}

/* Return the default `extern (System)' linkage for the target.  */

LINK
Target::systemLinkage (void)
{
  unsigned link_system, link_windows;

  if (targetdm.d_has_stdcall_convention (&link_system, &link_windows))
    {
      /* In [attribute/linkage], `System' is the same as `Windows' on Windows
	 platforms, and `C' on other platforms.  */
      if (link_system)
	return LINK::windows;
    }

  return LINK::c;
}

/* Generate a TypeTuple of the equivalent types used to determine if a
   function argument of the given type can be passed in registers.
   The results of this are highly platform dependent, and intended
   primarly for use in implementing va_arg() with RTTI.  */

TypeTuple *
Target::toArgTypes (Type *)
{
  /* Not implemented, however this is not currently used anywhere.  */
  return NULL;
}

/* Determine return style of function, whether in registers or through a
   hidden pointer to the caller's stack.  */

bool
Target::isReturnOnStack (TypeFunction *tf, bool)
{
  /* Need the back-end type to determine this, but this is called from the
     frontend before semantic processing is finished.  An accurate value
     is not currently needed anyway.  */
  if (tf->isref ())
    return false;

  Type *tn = tf->next->toBasetype ();
  if (tn->size () == SIZE_INVALID)
    return false;

  return (tn->ty == TY::Tstruct || tn->ty == TY::Tsarray);
}

/* Add all target info in HANDLERS to D_TARGET_INFO_TABLE for use by
   Target::getTargetInfo().  */

void
d_add_target_info_handlers (const d_target_info_spec *handlers)
{
  gcc_assert (handlers != NULL);

  if (d_target_info_table.is_empty ())
    d_target_info_table.create (8);

  for (size_t i = 0; handlers[i].name != NULL; i++)
    d_target_info_table.safe_push (handlers[i]);
}

/* Handle a call to `__traits(getTargetInfo, "cppStd")'.  */

tree
d_handle_target_cpp_std (void)
{
  return build_integer_cst (global.params.cplusplus);
}

/* Handle a call to `__traits(getTargetInfo, "cppRuntimeLibrary")'.  */

tree
d_handle_target_cpp_runtime_library (void)
{
  /* The driver only ever optionally links to libstdc++.  */
  const char *libstdcxx = "libstdc++";
  return build_string_literal (strlen (libstdcxx) + 1, libstdcxx);
}

/* Handle a call to `__traits(getTargetInfo, "objectFormat")'.  */

tree
d_handle_target_object_format (void)
{
  const char *objfmt;

#ifdef OBJECT_FORMAT_ELF
  objfmt = "elf";
#else
  if (TARGET_COFF || TARGET_PECOFF)
    objfmt = "coff";
  else
    objfmt = "";
#endif

  return build_string_literal (strlen (objfmt) + 1, objfmt);
}

/* Look up the target info KEY in the available getTargetInfo tables, and return
   the result as an Expression, or NULL if KEY is not found.  When the key must
   always exist, but is not supported, an empty string expression is returned.
   LOC is the location to use for the returned expression.  */

Expression *
Target::getTargetInfo (const char *key, const Loc &loc)
{
  unsigned ix;
  d_target_info_spec *spec;

  FOR_EACH_VEC_ELT (d_target_info_table, ix, spec)
    {
      tree result;

      if (strcmp (key, spec->name) != 0)
	continue;

      /* Get the requested information, or empty string if unhandled.  */
      if (spec->handler)
	{
	  result = (spec->handler) ();
	  /* Handler didn't return a result, meaning it really does not support
	     the key in the current target configuration.  Check whether there
	     are any other handlers which may recognize the key.  */
	  if (result == NULL_TREE)
	    continue;
	}
      else
	result = build_string_literal (1, "");

      gcc_assert (result);
      return d_eval_constant_expression (loc, result);
    }

  return NULL;
}

/* Returns true if the callee invokes destructors for arguments.  */

bool
Target::isCalleeDestroyingArgs (TypeFunction *tf)
{
  return tf->linkage == LINK::d;
}

/* Returns true if the implementation for object monitors is always defined
   in the D runtime library (rt/monitor_.d).  */

bool
Target::libraryObjectMonitors (FuncDeclaration *, Statement *)
{
  return true;
}

/* Returns true if the target supports `pragma(linkerDirective)'.  */

bool
Target::supportsLinkerDirective (void) const
{
  return false;
}

/* Decides whether an `in' parameter of the specified POD type PARAM_TYPE is to
   be passed by reference or by valie.  This is used only when compiling with
   `-fpreview=in' enabled.  */

bool
Target::preferPassByRef (Type *param_type)
{
  if (param_type->size () == SIZE_INVALID)
    return false;

  tree type = build_ctype (param_type);

  /* Prefer a `ref' if the type is an aggregate, and its size is greater than
     its alignment.  */
  if (AGGREGATE_TYPE_P (type)
      && (!valid_constant_size_p (TYPE_SIZE_UNIT (type))
	  || compare_tree_int (TYPE_SIZE_UNIT (type), TYPE_ALIGN (type)) > 0))
    return true;

  /* If the back-end is always going to pass this by invisible reference.  */
  if (pass_by_reference (NULL, function_arg_info (type, true)))
    return true;

  /* If returning the parameter means the caller will do RVO.  */
  if (targetm.calls.return_in_memory (type, NULL_TREE))
    return true;

  return false;
}
