/* d-longdouble.cc -- Software floating-point emulation for the frontend.
   Copyright (C) 2006-2020 Free Software Foundation, Inc.

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

#include "dmd/mtype.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "stor-layout.h"

#include "d-tree.h"
#include "longdouble.h"


/* Constant real values 0, 1, -1 and 0.5.  */
real_t CTFloat::zero;
real_t CTFloat::one;
real_t CTFloat::minusone;
real_t CTFloat::half;

/* Truncate longdouble to the highest precision supported by target.  */

longdouble
longdouble::normalize (void)
{
  const machine_mode mode = TYPE_MODE (long_double_type_node);
  real_convert (&this->rv (), mode, &this->rv ());
  return *this;
}

/* Assign a real_value to a longdouble type.  */

void
longdouble::set (real_value &d)
{
  real_convert (&this->rv (), TYPE_MODE (long_double_type_node), &d);
}

/* Conversion routines between longdouble and integer types.  */

void
longdouble::set (int32_t d)
{
  real_from_integer (&this->rv (), TYPE_MODE (double_type_node), d, SIGNED);
}

void
longdouble::set (int64_t d)
{
  real_from_integer (&this->rv (), TYPE_MODE (long_double_type_node), d,
		     SIGNED);
}

int64_t
longdouble::to_int (void) const
{
  bool overflow;
  wide_int wi = real_to_integer (&this->rv (), &overflow, 64);
  return wi.to_shwi ();
}

/* Unsigned variants of the same conversion routines.  */

void
longdouble::set (uint32_t d)
{
  real_from_integer (&this->rv (), TYPE_MODE (double_type_node), d, UNSIGNED);
}

void
longdouble::set (uint64_t d)
{
  real_from_integer (&this->rv (), TYPE_MODE (long_double_type_node), d,
		     UNSIGNED);
}

uint64_t
longdouble::to_uint (void) const
{
  bool overflow;
  wide_int wi = real_to_integer (&this->rv (), &overflow, 64);
  return wi.to_uhwi ();
}

/* For conversion between boolean, only need to check if is zero.  */

void
longdouble::set (bool d)
{
  this->rv () = (d == false) ? dconst0 : dconst1;
}

bool
longdouble::to_bool (void) const
{
  return this->rv ().cl != rvc_zero;
}

/* Overload numeric operators for longdouble types.  */

longdouble
longdouble::add (const longdouble &r) const
{
  longdouble x;
  real_arithmetic (&x.rv (), PLUS_EXPR, &this->rv (), &r.rv ());
  return x.normalize ();
}

longdouble
longdouble::sub (const longdouble &r) const
{
  longdouble x;
  real_arithmetic (&x.rv (), MINUS_EXPR, &this->rv (), &r.rv ());
  return x.normalize ();
}

longdouble
longdouble::mul (const longdouble &r) const
{
  longdouble x;
  real_arithmetic (&x.rv (), MULT_EXPR, &this->rv (), &r.rv ());
  return x.normalize ();
}

longdouble
longdouble::div (const longdouble &r) const
{
  longdouble x;
  real_arithmetic (&x.rv (), RDIV_EXPR, &this->rv (), &r.rv ());
  return x.normalize ();
}

longdouble
longdouble::mod (const longdouble &r) const
{
  longdouble x;
  real_value q;

  if (r.rv ().cl == rvc_zero || REAL_VALUE_ISINF (this->rv ()))
    {
      real_nan (&x.rv (), "", 1, TYPE_MODE (long_double_type_node));
      return x;
    }

  if (this->rv ().cl == rvc_zero)
    return *this;

  if (REAL_VALUE_ISINF (r.rv ()))
    return *this;

  /* Need to check for NaN?  */
  real_arithmetic (&q, RDIV_EXPR, &this->rv (), &r.rv ());
  real_arithmetic (&q, FIX_TRUNC_EXPR, &q, NULL);
  real_arithmetic (&q, MULT_EXPR, &q, &r.rv ());
  real_arithmetic (&x.rv (), MINUS_EXPR, &this->rv (), &q);

  return x.normalize ();
}

longdouble
longdouble::neg (void) const
{
  longdouble x;
  real_arithmetic (&x.rv (), NEGATE_EXPR, &this->rv (), NULL);
  return x.normalize ();
}

/* Overload equality operators for longdouble types.  */

int
longdouble::cmp (const longdouble &r) const
{
  if (real_compare (LT_EXPR, &this->rv (), &r.rv ()))
    return -1;

  if (real_compare (GT_EXPR, &this->rv (), &r.rv ()))
    return 1;

  return 0;
}

int
longdouble::equals (const longdouble &r) const
{
  return real_compare (EQ_EXPR, &this->rv (), &r.rv ());
}
