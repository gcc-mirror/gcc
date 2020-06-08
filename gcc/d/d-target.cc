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

Target target;


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

  /* Signalling floating-point NaN.  */
  real_nan (&f.snan.rv (), "", 0, mode);

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

  /* Size of run-time TypeInfo object.  */
  this->classinfosize = 19 * this->ptrsize;

  /* Much of the dmd front-end uses ints for sizes and offsets, and cannot
     handle any larger data type without some pervasive rework.  */
  this->maxStaticDataSize = tree_to_shwi (TYPE_MAX_VALUE (integer_type_node));

  /* Define what type to use for size_t, ptrdiff_t.  */
  if (this->ptrsize == 8)
    {
      global.params.isLP64 = true;
      Type::tsize_t = Type::basic[Tuns64];
      Type::tptrdiff_t = Type::basic[Tint64];
    }
  else if (this->ptrsize == 4)
    {
      Type::tsize_t = Type::basic[Tuns32];
      Type::tptrdiff_t = Type::basic[Tint32];
    }
  else if (this->ptrsize == 2)
    {
      Type::tsize_t = Type::basic[Tuns16];
      Type::tptrdiff_t = Type::basic[Tint16];
    }
  else
    sorry ("D does not support pointers on this target.");

  Type::thash_t = Type::tsize_t;

  /* Set-up target C ABI.  */
  this->c.longsize = int_size_in_bytes (long_integer_type_node);
  this->c.long_doublesize = int_size_in_bytes (long_double_type_node);

  /* Set-up target C++ ABI.  */
  this->cpp.reverseOverloads = false;
  this->cpp.exceptions = true;
  this->cpp.twoDtorInVtable = true;

  /* Set-up target Objective-C ABI.  */
  this->objc.supported = false;

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

/* For a vendor-specific type, return a string containing the C++ mangling.
   In all other cases, return NULL.  */

const char *
TargetCPP::typeMangle (Type *type)
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
TargetCPP::parameterType (Parameter *arg)
{
  Type *t = arg->type->merge2 ();
  if (arg->storageClass & (STCout | STCref))
    t = t->referenceTo ();
  else if (arg->storageClass & STClazy)
    {
      /* Mangle as delegate.  */
      Type *td = TypeFunction::create (NULL, t, VARARGnone, LINKd);
      td = TypeDelegate::create (td);
      t = t->merge2 ();
    }

  /* Could be a va_list, which we mangle as a pointer.  */
  Type *tvalist = target.va_listType (Loc (), NULL);
  if (t->ty == Tsarray && tvalist->ty == Tsarray)
    {
      Type *tb = t->toBasetype ()->mutableOf ();
      if (tb == tvalist)
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
TargetCPP::fundamentalType (const Type *, bool &)
{
  return false;
}

/* Return the default system linkage for the target.  */

LINK
Target::systemLinkage (void)
{
  return LINKc;
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
