/* A few TImode functions needed for TFmode emulated arithmetic.
   Copyright 2002, 2003 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "tconfig.h"

#if _MIPS_SIM == 2 /* N32 */ || _MIPS_SIM == 3 /* 64 */

typedef int TItype __attribute__ ((mode (TI)));
typedef int DItype __attribute__ ((mode (DI)));
typedef int SItype __attribute__ ((mode (SI)));

typedef unsigned int UDItype __attribute__ ((mode (DI)));

typedef union
{
  struct TIstruct {
#if LIBGCC2_WORDS_BIG_ENDIAN
    DItype high, low;
#else
    DItype low, high;
#endif
  } s;
  TItype ll;
} TIunion;

TItype __negti2 (TItype);
TItype __ashlti3 (TItype, int);
#if 0
TItype __ashrti3 (TItype, int);
#endif
TItype __lshrti3 (TItype, int);

TItype
__negti2 (TItype u)
{
  TIunion w;
  TIunion uu;

  uu.ll = u;

  w.s.low = -uu.s.low;
  w.s.high = -uu.s.high - ((UDItype) w.s.low > 0);

  return w.ll;
}

TItype
__ashlti3 (TItype u, int b)
{
  TIunion w;
  int bm;
  TIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (DItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.low = 0;
      w.s.high = (UDItype) uu.s.low << -bm;
    }
  else
    {
      UDItype carries = (UDItype) uu.s.low >> bm;

      w.s.low = (UDItype) uu.s.low << b;
      w.s.high = ((UDItype) uu.s.high << b) | carries;
    }

  return w.ll;
}

#if 0
TItype
__ashrti3 (TItype u, int b)
{
  TIunion w;
  int bm;
  TIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (DItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      /* w.s.high = 1..1 or 0..0 */
      w.s.high = uu.s.high >> (sizeof (DItype) * BITS_PER_UNIT - 1);
      w.s.low = uu.s.high >> -bm;
    }
  else
    {
      UDItype carries = (UDItype) uu.s.high << bm;

      w.s.high = uu.s.high >> b;
      w.s.low = ((UDItype) uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

TItype
__lshrti3 (TItype u, int b)
{
  TIunion w;
  int bm;
  TIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (DItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.high = 0;
      w.s.low = (UDItype) uu.s.high >> -bm;
    }
  else
    {
      UDItype carries = (UDItype) uu.s.high << bm;

      w.s.high = (UDItype) uu.s.high >> b;
      w.s.low = ((UDItype) uu.s.low >> b) | carries;
    }

  return w.ll;
}

#endif /* N32 or N64 */
