/* Copyright (C) 2008, 2009 Free Software Foundation, Inc.
 
   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.
 
   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.
 
   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <spu_intrinsics.h>

typedef int TItype __attribute__ ((mode (TI)));

/* A straight forward vectorization and unrolling of
 *   short l[8], r[8];
 *   TItype total = 0;
 *   for (i = 0; i < 8; i++)
 *     for (j = 0; j < 8; j++)
 *       total += (TItype)((l[7-i] * r[7-j]) << (16 * (i + j)));
 */
TItype
__multi3 (TItype l, TItype r)
{
  qword u = *(qword *) & l;
  qword v = *(qword *) & r;
  qword splat0 = si_shufb (v, v, si_ilh (0x0001));
  qword splat1 = si_shufb (v, v, si_ilh (0x0203));
  qword splat2 = si_shufb (v, v, si_ilh (0x0405));
  qword splat3 = si_shufb (v, v, si_ilh (0x0607));
  qword splat4 = si_shufb (v, v, si_ilh (0x0809));
  qword splat5 = si_shufb (v, v, si_ilh (0x0a0b));
  qword splat6 = si_shufb (v, v, si_ilh (0x0c0d));
  qword splat7 = si_shufb (v, v, si_ilh (0x0e0f));

  qword part0l = si_shlqbyi (si_mpyu   (u, splat0), 14);
  qword part1h = si_shlqbyi (si_mpyhhu (u, splat1), 14);
  qword part1l = si_shlqbyi (si_mpyu   (u, splat1), 12);
  qword part2h = si_shlqbyi (si_mpyhhu (u, splat2), 12);
  qword part2l = si_shlqbyi (si_mpyu   (u, splat2), 10);
  qword part3h = si_shlqbyi (si_mpyhhu (u, splat3), 10);
  qword part3l = si_shlqbyi (si_mpyu   (u, splat3), 8);
  qword part4h = si_shlqbyi (si_mpyhhu (u, splat4), 8);
  qword part4l = si_shlqbyi (si_mpyu   (u, splat4), 6);
  qword part5h = si_shlqbyi (si_mpyhhu (u, splat5), 6);
  qword part5l = si_shlqbyi (si_mpyu   (u, splat5), 4);
  qword part6h = si_shlqbyi (si_mpyhhu (u, splat6), 4);
  qword part6l = si_shlqbyi (si_mpyu   (u, splat6), 2);
  qword part7h = si_shlqbyi (si_mpyhhu (u, splat7), 2);
  qword part7l = si_mpyu (u, splat7);

  qword carry, total0, total1, total2, total3, total4;
  qword total5, total6, total7, total8, total9, total10;
  qword total;

  total0 = si_a (si_a (si_a (part0l, part1h), si_a (part1l, part2h)), part7l);
  total1 = si_a (part2l, part3h);
  total2 = si_a (part3l, part4h);
  total3 = si_a (part4l, part5h);
  total4 = si_a (part5l, part6h);
  total5 = si_a (part6l, part7h);
  total6 = si_a (total0, total1);
  total7 = si_a (total2, total3);
  total8 = si_a (total4, total5);
  total9 = si_a (total6, total7);
  total10 = si_a (total8, total9);

  carry = si_cg (part2l, part3h);
  carry = si_a (carry, si_cg (part3l, part4h));
  carry = si_a (carry, si_cg (part4l, part5h));
  carry = si_a (carry, si_cg (part5l, part6h));
  carry = si_a (carry, si_cg (part6l, part7h));
  carry = si_a (carry, si_cg (total0, total1));
  carry = si_a (carry, si_cg (total2, total3));
  carry = si_a (carry, si_cg (total4, total5));
  carry = si_a (carry, si_cg (total6, total7));
  carry = si_a (carry, si_cg (total8, total9));
  carry = si_shlqbyi (carry, 4);

  total = si_cg (total10, carry);
  total = si_shlqbyi (total, 4);
  total = si_cgx (total10, carry, total);
  total = si_shlqbyi (total, 4);
  total = si_addx (total10, carry, total);
  return *(TItype *) & total;
}
