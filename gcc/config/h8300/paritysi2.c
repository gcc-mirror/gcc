/* More subroutines needed by GCC output code on some machines.  */
/* Compile this one with gcc.  */
/* Copyright (C) 2003  Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

int
__paritysi2 (unsigned long x)
{
#ifdef __H8300__
  unsigned char a;
  asm ("xor.b\t%z1,%w1\n\t"
       "xor.b\t%y1,%w1\n\t"
       "xor.b\t%x1,%w1\n\t"
       "bld\t#0,%w1\n\t"
       "bxor\t#1,%w1\n\t"
       "bxor\t#2,%w1\n\t"
       "bxor\t#3,%w1\n\t"
       "bxor\t#4,%w1\n\t"
       "bxor\t#5,%w1\n\t"
       "bxor\t#6,%w1\n\t"
       "bxor\t#7,%w1\n\t"
       "xor.b\t%X0,%X0\n\t"
       "bst\t#0,%s0" : "=r" (a) : "r" (x));
  return a;
#else
  unsigned short a;
  asm ("xor.w\t%e1,%f1\n\t"
       "xor.b\t%x1,%w1\n\t"
       "bld\t#0,%w1\n\t"
       "bxor\t#1,%w1\n\t"
       "bxor\t#2,%w1\n\t"
       "bxor\t#3,%w1\n\t"
       "bxor\t#4,%w1\n\t"
       "bxor\t#5,%w1\n\t"
       "bxor\t#6,%w1\n\t"
       "bxor\t#7,%w1\n\t"
       "xor.w\t%T0,%T0\n\t"
       "bst\t#0,%s0" : "=r" (a) : "r" (x));
  return a;
#endif
}
