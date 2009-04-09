/*

picoChip GCC support for 32-bit arithmetic shift right.

Copyright (C) 2003, 2004, 2005, 2008, 2009  Free Software Foundation, Inc.
Contributed by picoChip Designs Ltd.
Maintained by Daniel Towner (daniel.towner@picochip.com)

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

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

USItype __ashrsi3(USIunion value, HItype count) {
  USIunion result;
  int temp;
  int wordOfSignBits;

  /* Ignore a zero count until we get into the (count < 16)
     clause. This is slightly slower when shifting by zero, but faster
     and smaller in all other cases (due to the better scheduling
     opportunities available by putting the test near computational
     instructions. */
  /* if (count == 0) return value.l; */
  
  if (count < 16) {
    /* Shift low and high words by the count. The high word must use
       an arithmetic shift. There is no arithmetic shift-right by
       variable, so synthesise it. */
    int signWord;
    int reverseCount;

    /* Shift low and high parts by the count. The upper word now has
       invalid signed bits. */
    result.s.low = value.s.low >> count;
    result.s.high = value.s.high >> count;

    if (count != 0) {

      reverseCount = 16 - count;
  
      /* Given a word of sign bits, shift back left to create the
	 destination sign bits. */
      wordOfSignBits = __builtin_asri(value.s.high, 15);
      signWord = wordOfSignBits << reverseCount;
      result.s.high |= signWord;
     
      /* There is now a hole in the upper `count' bits of the low
	 word. Shift the lower `count' bits of the upper word into the
	 low word. */
      temp = value.s.high << reverseCount;
      result.s.low |= temp;
    }

  } else {
    int signWord;

    /* Shift is greater than one word, so top word will always be set
       to sign bits, and bottom word will be shifted from top word. */
    result.s.low = value.s.high >> count;
    result.s.high = __builtin_asri(value.s.high, 15);

    if (count != 16) {

      /* Shift the upper word of the source into the lower word of the
	 result. Arithmetically shift the upper word as well, to retain
	 the sign. This shift must be synthesised, as no such shift
	 exists in the instruction set. */
      int signWord;
 

      /* Given a complete word of sign-bits, shift this back left to
	 create the destination sign bits. */
      signWord = result.s.high << (16 - count);
      //      signWord = wordOfSignBits << (16 - count);

      /* Insert the sign bits to the result's low word. */
      result.s.low |= signWord;

    }

  }

  return result.l;

}
