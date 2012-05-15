/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             A R I T 6 4 . C                              *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *         Copyright (C) 2009-2012, Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

extern void __gnat_rcheck_CE_Overflow_Check(char *file, int line)
  __attribute__ ((__noreturn__));

long long int __gnat_mulv64 (long long int x, long long int y)
{
  unsigned neg = (x >= 0) ^ (y >= 0);
  long long unsigned xa = x >= 0 ? (long long unsigned) x
                                 : -(long long unsigned) x;
  long long unsigned ya = y >= 0 ? (long long unsigned) y
                                 : -(long long unsigned) y;
  unsigned xhi = (unsigned) (xa >> 32);
  unsigned yhi = (unsigned) (ya >> 32);
  unsigned xlo = (unsigned) xa;
  unsigned ylo = (unsigned) ya;
  long long unsigned mid
    = xhi ? (long long unsigned) xhi * (long long unsigned) ylo
	 : (long long unsigned) yhi * (long long unsigned) xlo;
  long long unsigned low = (long long unsigned) xlo * (long long unsigned) ylo;

  if ((xhi && yhi) ||  mid + (low  >> 32) > 0x7fffffff + neg)
    __gnat_rcheck_CE_Overflow_Check (__FILE__, __LINE__);

  low += ((long long unsigned) (unsigned) mid) << 32;

  return (long long int) (neg ? -low : low);
}
