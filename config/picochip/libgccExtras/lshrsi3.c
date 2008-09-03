/*

picoChip GCC support for 32-bit logical shift right.

Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.
Contributed by picoChip Designs Ltd.
Maintained by Daniel Towner (daniel.towner@picochip.com)

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. */

typedef int HItype __attribute__ ((mode (HI)));
typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));

typedef struct USIstruct {
  UHItype low, high;
} USIstruct;

typedef union USIunion {
  USItype l;
  USIstruct s;
} USIunion;

USItype __lshrsi3(USIunion value, HItype count) {
  USIunion result;
  int temp;

  /* Ignore a zero count until we get into the (count < 16)
     clause. This is slightly slower when shifting by zero, but faster
     and smaller in all other cases (due to the better scheduling
     opportunities available by putting the test near computational
     instructions. */

  if (count < 16) {
    /* Shift low and high words by the count. */
    result.s.low = value.s.low >> count;
    result.s.high = value.s.high >> count;
     
    /* There is now a hole in the upper `count' bits of the low
       word. Shift the lower `count' bits of the upper word into the
       low word. This only works when count isn't zero. */
    if (count != 0) {
      temp = value.s.high << (16 - count);
      result.s.low |= temp;
    }

  } else {
    /* Shift the upper word of the source into the lower word of the
       result, and zero the result's upper word. Note that we actually
       ned to shift by (count - 16), but as we are only using the
       bottom 4 bits, this is equivalent to shifting by count. */
    result.s.low = value.s.high >> count;
    result.s.high = 0;

  }

  return result.l;

}
