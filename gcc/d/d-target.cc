/* d-target.cc -- Target interface for the D front end.
   Copyright (C) 2013-2020 Free Software Foundation, Inc.

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

#include "d-tree.h"
#include "d-target.h"

/* Implements the Target interface defined by the front end.
   Used for retrieving target-specific information.  */

/* Type size information used by frontend.  */
int Target::ptrsize;
int Target::c_longsize;
int Target::realsize;
int Target::realpad;
int Target::realalignsize;
bool Target::reverseCppOverloads;
bool Target::cppExceptions;
int Target::classinfosize;
unsigned long long Target::maxStaticDataSize;

/* Floating-point constants for .max, .min, and other properties.  */
template <typename T> real_t Target::FPTypeProperties<T>::max;
template <typename T> real_t Target::FPTypeProperties<T>::min_normal;
template <typename T> real_t Target::FPTypeProperties<T>::nan;
template <typename T> real_t Target::FPTypeProperties<T>::snan;
template <typename T> real_t Target::FPTypeProperties<T>::infinity;
template <typename T> real_t Target::FPTypeProperties<T>::epsilon;
template <typename T> d_int64 Target::FPTypeProperties<T>::dig;
template <typename T> d_int64 Target::FPTypeProperties<T>::mant_dig;
template <typename T> d_int64 Target::FPTypeProperties<T>::max_exp;
template <typename T> d_int64 Target::FPTypeProperties<T>::min_exp;
template <typename T> d_int64 Target::FPTypeProperties<T>::max_10_exp;
template <typename T> d_int64 Target::FPTypeProperties<T>::min_10_exp;


/* Initialize the floating-point constants for TYPE.  */

template <typename T>
static void
define_float_constants (tree type)
{
  const double log10_2 = 0.30102999566398119521;
  char buf[128];

  /* Get back-end real mode format.  */
  const machine_mode mode = TYPE_MODE (type);
  const real_format *fmt = REAL_MODE_FORMAT (mode);

  /* The largest representable value that's not infinity.  */
  get_max_float (fmt, buf, sizeof (buf), false);
  real_from_string (&T::max.rv (), buf);

  /* The smallest representable normalized value that's not 0.  */
  snprintf (buf, sizeof (buf), "0x1p%d", fmt->emin - 1);
  real_from_string (&T::min_normal.rv (), buf);

  /* Floating-point NaN.  */
  real_nan (&T::nan.rv (), "", 1, mode);

  /* Signalling floating-point NaN.  */
  real_nan (&T::snan.rv (), "", 0, mode);

  /* Floating-point +Infinity if the target supports infinities.  */
  real_inf (&T::infinity.rv ());

  /* The smallest increment to the value 1.  */
  if (fmt->pnan < fmt->p)
    snprintf (buf, sizeof (buf), "0x1p%d", fmt->emin - fmt->p);
  else
    snprintf (buf, sizeof (buf), "0x1p%d", 1 - fmt->p);
  real_from_string (&T::epsilon.rv (), buf);

  /* The number of decimal digits of precision.  */
  T::dig = (fmt->p - 1) * log10_2;

  /* The number of bits in mantissa.  */
  T::mant_dig = fmt->p;

  /* The maximum int value such that 2** (value-1) is representable.  */
  T::max_exp = fmt->emax;

  /* The minimum int value such that 2** (value-1) is representable as a
     normalized value.  */
  T::min_exp = fmt->emin;

  /* The maximum int value such that 10**value is representable.  */
  T::max_10_exp = fmt->emax * log10_2;

  /* The minimum int value such that 10**value is representable as a
     normalized value.  */
  T::min_10_exp = (fmt->emin - 1) * log10_2;
}

/* Initialize all variables of the Target structure.  */

void
Target::_init (void)
{
  /* Map D frontend type and sizes to GCC back-end types.  */
  Target::ptrsize = (POINTER_SIZE / BITS_PER_UNIT);
  Target::realsize = int_size_in_bytes (long_double_type_node);
  Target::realpad = (Target::realsize -
		     (TYPE_PRECISION (long_double_type_node) / BITS_PER_UNIT));
  Target::realalignsize = TYPE_ALIGN_UNIT (long_double_type_node);

  /* Size of run-time TypeInfo object.  */
  Target::classinfosize = 19 * Target::ptrsize;

  /* Much of the dmd front-end uses ints for sizes and offsets, and cannot
     handle any larger data type without some pervasive rework.  */
  Target::maxStaticDataSize = tree_to_shwi (TYPE_MAX_VALUE (integer_type_node));

  /* Define what type to use for size_t, ptrdiff_t.  */
  if (Target::ptrsize == 8)
    {
      global.params.isLP64 = true;
      Tsize_t = Tuns64;
      Tptrdiff_t = Tint64;
    }
  else if (Target::ptrsize == 4)
    {
      Tsize_t = Tuns32;
      Tptrdiff_t = Tint32;
    }
  else if (Target::ptrsize == 2)
    {
      Tsize_t = Tuns16;
      Tptrdiff_t = Tint16;
    }
  else
    sorry ("D does not support pointers on this target.");

  Type::tsize_t = Type::basic[Tsize_t];
  Type::tptrdiff_t = Type::basic[Tptrdiff_t];
  Type::thash_t = Type::tsize_t;

  /* Set-up target C ABI.  */
  Target::c_longsize = int_size_in_bytes (long_integer_type_node);

  /* Set-up target C++ ABI.  */
  Target::reverseCppOverloads = false;
  Target::cppExceptions = true;

  /* Initialize all compile-time properties for floating-point types.
     Should ensure that our real_t type is able to represent real_value.  */
  gcc_assert (sizeof (real_t) >= sizeof (real_value));

  define_float_constants <Target::FloatProperties> (float_type_node);
  define_float_constants <Target::DoubleProperties> (double_type_node);
  define_float_constants <Target::RealProperties> (long_double_type_node);

  /* Commonly used floating-point constants.  */
  const machine_mode mode = TYPE_MODE (long_double_type_node);
  real_convert (&CTFloat::zero.rv (), mode, &dconst0);
  real_convert (&CTFloat::one.rv (), mode, &dconst1);
  real_convert (&CTFloat::minusone.rv (), mode, &dconstm1);
  real_convert (&CTFloat::half.rv (), mode, &dconsthalf);
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

/* Return size of OS critical section.
   Can't use the sizeof () calls directly since cross compiling is supported
   and would end up using the host sizes rather than the target sizes.  */

unsigned
Target::critsecsize (void)
{
  return targetdm.d_critsec_size ();
}

/* Returns a Type for the va_list type of the target.  */

Type *
Target::va_listType (void)
{
  return Type::tvalist;
}

/* Checks whether the target supports a vector type with total size SZ
   (in bytes) and element type TYPE.  */

int
Target::isVectorTypeSupported (int sz, Type *type)
{
  /* Size must be greater than zero, and a power of two.  */
  if (sz <= 0 || sz & (sz - 1))
    return 2;

  /* __vector(void[]) is treated same as __vector(ubyte[])  */
  if (type == Type::tvoid)
    type = Type::tuns8;

  /* No support for non-trivial types.  */
  if (!type->isTypeBasic ())
    return 3;

  /* If there is no hardware support, check if we can safely emulate it.  */
  tree ctype = build_ctype (type);
  machine_mode mode = TYPE_MODE (ctype);

  if (!targetm.vector_mode_supported_p (mode)
      && !targetm.scalar_mode_supported_p (as_a <scalar_mode> (mode)))
    return 3;

  return 0;
}

/* Checks whether the target supports operation OP for vectors of type TYPE.
   For binary ops T2 is the type of the right-hand operand.
   Returns true if the operation is supported or type is not a vector.  */

bool
Target::isVectorOpSupported (Type *type, TOK op, Type *)
{
  if (type->ty != Tvector)
    return true;

  /* Don't support if type is non-scalar, such as __vector(void[]).  */
  if (!type->isscalar ())
    return false;

  /* Don't support if expression cannot be represented.  */
  switch (op)
    {
    case TOKpow:
    case TOKpowass:
      /* pow() is lowered as a function call.  */
      return false;

    case TOKmod:
    case TOKmodass:
      /* fmod() is lowered as a function call.  */
      if (type->isfloating ())
	return false;
      break;

    case TOKandand:
    case TOKoror:
      /* Logical operators must have a result type of bool.  */
      return false;

    case TOKue:
    case TOKlg:
    case TOKule:
    case TOKul:
    case TOKuge:
    case TOKug:
    case TOKle:
    case TOKlt:
    case TOKge:
    case TOKgt:
    case TOKleg:
    case TOKunord:
    case TOKequal:
    case TOKnotequal:
    case TOKidentity:
    case TOKnotidentity:
      /* Comparison operators must have a result type of bool.  */
      return false;

    default:
      break;
    }

  return true;
}

/* Return the symbol mangling of S for C++ linkage.  */

const char *
Target::toCppMangle (Dsymbol *s)
{
  return toCppMangleItanium (s);
}

/* Return the symbol mangling of CD for C++ linkage.  */

const char *
Target::cppTypeInfoMangle (ClassDeclaration *cd)
{
  return cppTypeInfoMangleItanium (cd);
}

/* For a vendor-specific type, return a string containing the C++ mangling.
   In all other cases, return NULL.  */

const char *
Target::cppTypeMangle (Type *type)
{
  if (type->isTypeBasic () || type->ty == Tvector || type->ty == Tstruct)
    {
      tree ctype = build_ctype (type);
      return targetm.mangle_type (ctype);
    }

  return NULL;
}

/* Return the type that will really be used for passing the given parameter
   ARG to an extern(C++) function.  */

Type *
Target::cppParameterType (Parameter *arg)
{
  Type *t = arg->type->merge2 ();
  if (arg->storageClass & (STCout | STCref))
    t = t->referenceTo ();
  else if (arg->storageClass & STClazy)
    {
      /* Mangle as delegate.  */
      Type *td = TypeFunction::create (NULL, t, 0, LINKd);
      td = TypeDelegate::create (td);
      t = t->merge2 ();
    }

  /* Could be a va_list, which we mangle as a pointer.  */
  if (t->ty == Tsarray && Type::tvalist->ty == Tsarray)
    {
      Type *tb = t->toBasetype ()->mutableOf ();
      if (tb == Type::tvalist)
	{
	  tb = t->nextOf ()->pointerTo ();
	  t = tb->castMod (t->mod);
	}
    }

  return t;
}

/* Checks whether TYPE is a vendor-specific fundamental type.  Stores the result
   in IS_FUNDAMENTAL and returns true if the parameter was set.  */

bool
Target::cppFundamentalType (const Type *, bool &)
{
  return false;
}

/* Return the default system linkage for the target.  */

LINK
Target::systemLinkage (void)
{
  return LINKc;
}
