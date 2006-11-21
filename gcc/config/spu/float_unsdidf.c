/* Copyright (C) 2006 Free Software Foundation, Inc.
  
   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option)
   any later version.
  
   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.
  
   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License.  The exception does not
   however invalidate any other reasons why the executable file might be covered
   by the GNU General Public License. */

#include <spu_intrinsics.h>
const unsigned char __didf_scale[16] __attribute__ ((__aligned__ (16))) = {
  0x00, 0x00, 0x04, 0x3e,
  0x00, 0x00, 0x04, 0x1e,
  0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00
};
const unsigned char __didf_pat[16] __attribute__ ((__aligned__ (16))) = {
  0x02, 0x03, 0x10, 0x11,
  0x12, 0x13, 0x80, 0x80,
  0x06, 0x07, 0x14, 0x15,
  0x16, 0x17, 0x80, 0x80
};

/* double __float_unsdidf (unsigned long long int) 
   Construct two exact doubles representing the high and low parts (in
   parallel), then add them. */
qword __float_unsdidf (qword DI);
qword
__float_unsdidf (qword DI)
{
  qword t0, t1, t2, t3, t4, t5, t6, t7, t8;
  t0 = si_clz (DI);
  t1 = si_shl (DI, t0);
  t2 = si_ceqi (t0, 32);
  t3 = si_sf (t0, *(qword *) __didf_scale);
  t4 = si_a (t1, t1);
  t5 = si_andc (t3, t2);
  t6 = si_shufb (t5, t4, *(qword *) __didf_pat);
  t7 = si_shlqbii (t6, 4);
  t8 = si_shlqbyi (t7, 8);
  return si_dfa (t7, t8);
}
