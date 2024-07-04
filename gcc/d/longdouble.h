/* longdouble.h -- Definitions of floating-point access for the frontend.
   Copyright (C) 2015-2024 Free Software Foundation, Inc.

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

#ifndef GCC_D_LONGDOUBLE_H
#define GCC_D_LONGDOUBLE_H

struct real_value;
class Type;

struct longdouble
{
public:
  /* Return the hidden real_value from the longdouble type.  */
  const real_value &rv (void) const
  { return *(const real_value *) this; }

  real_value &rv (void)
  { return *(real_value *) this; }

  /* Normalize the value to be the precision supported by target.  */
  longdouble normalize (void);

  /* No constructor to be able to use this class in a union.  */
  template <typename T> longdouble &operator = (T x)
  { set (x); return *this; }

  /* Lvalue operators.  */
  void set (real_value &d);
  void set (int32_t d);
  void set (int64_t d);
  void set (uint32_t d);
  void set (uint64_t d);
  void set (bool d);

  /* Rvalue operators.  */
  bool to_bool () const;
  int64_t to_int () const;
  uint64_t to_uint () const;

  operator int32_t (void)
  { return (int32_t) this->to_int (); }

  operator int64_t (void)
  { return this->to_int (); }

  operator uint32_t (void)
  { return (uint32_t) this->to_uint (); }

  operator uint64_t (void)
  { return this->to_uint (); }

  operator bool (void)
  { return this->to_bool (); }

  /* Arithmetic operators.  */
  longdouble add (const longdouble &r) const;
  longdouble sub (const longdouble &r) const;
  longdouble mul (const longdouble &r) const;
  longdouble div (const longdouble &r) const;
  longdouble mod (const longdouble &r) const;
  longdouble neg () const;

  longdouble operator + (const longdouble &r)
  { return this->add (r); }

  longdouble operator - (const longdouble &r)
  { return this->sub (r); }

  longdouble operator * (const longdouble &r)
  { return this->mul (r); }

  longdouble operator / (const longdouble &r)
  { return this->div (r); }

  longdouble operator % (const longdouble &r)
  { return this->mod (r); }

  longdouble operator - (void)
  { return this->neg (); }

  /* Comparison operators.  */
  int cmp (const longdouble &t) const;
  int equals (const longdouble &t) const;

  bool operator < (const longdouble &r)
  { return this->cmp (r) < 0; }

  bool operator <= (const longdouble &r)
  { return this->cmp (r) <= 0; }

  bool operator > (const longdouble &r)
  { return this->cmp (r) > 0; }

  bool operator >= (const longdouble &r)
  { return this->cmp (r) >= 0; }

  bool operator == (const longdouble &r)
  { return this->equals (r); }

  bool operator != (const longdouble &r)
  { return !this->equals (r); }

private:
  /* Including gcc/real.h presents too many problems, so just
     statically allocate enough space for REAL_VALUE_TYPE.  */
  long realvalue[(2 + (16 + sizeof (long)) / sizeof (long))];
};

/* Declared, but "volatile" is not required.  */
typedef longdouble volatile_longdouble;

/* Use ldouble() to explicitly create a longdouble value.  */
template <typename T>
inline longdouble
ldouble (T x)
{
  longdouble d;
  d.set (x);
  return d;
}

#endif  /* GCC_D_LONGDOUBLE_H  */
