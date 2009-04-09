/* Copyright (C) 2006, 2009 Free Software Foundation, Inc.

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

#ifndef _VEC_TYPES_H_
#define _VEC_TYPES_H_	1

#include <spu_intrinsics.h>

/* Define additional PowerPC SIMD/Vector Multi-media eXtension
 * single keyword vector data types for use in mapping VMX code
 * to the SPU.
 */
#define vec_bchar16	__vector unsigned char
#define vec_bshort8	__vector unsigned short
#define vec_pixel8	__vector unsigned short
#define vec_bint4	__vector unsigned int

#endif /* _VEC_TYPES_H_ */
