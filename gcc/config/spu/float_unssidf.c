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
const unsigned char __sidf_pat[16] __attribute__ ((__aligned__ (16))) = {
  0x02, 0x03, 0x10, 0x11,
  0x12, 0x13, 0x80, 0x80,
  0x06, 0x07, 0x14, 0x15,
  0x16, 0x17, 0x80, 0x80
};

/* double __float_unssidf (unsigned int SI) */
qword __float_unssidf (qword SI);
qword
__float_unssidf (qword SI)
{
  qword t0, t1, t2, t3, t4, t5, t6, t7;
  t0 = si_clz (SI);
  t1 = si_il (1054);
  t2 = si_shl (SI, t0);
  t3 = si_ceqi (t0, 32);
  t4 = si_sf (t0, t1);
  t5 = si_a (t2, t2);
  t6 = si_andc (t4, t3);
  t7 = si_shufb (t6, t5, *(qword *)__sidf_pat);
  return si_shlqbii (t7, 4);
}
