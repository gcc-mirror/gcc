/* Cell single token vector types
   Copyright (C) 2007-2023 Free Software Foundation, Inc.

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

/* Single token vector data types for the PowerPC SIMD/Vector Multi-media 
   eXtension */

#ifndef _VEC_TYPES_H_
#define _VEC_TYPES_H_	1

#define qword		__vector unsigned char

#define vec_uchar16	__vector unsigned char
#define vec_char16	__vector signed char
#define vec_bchar16	__vector bool char

#define vec_ushort8	__vector unsigned short
#define vec_short8	__vector signed short
#define vec_bshort8	__vector bool short

#define vec_pixel8	__vector pixel

#define vec_uint4	__vector unsigned int
#define vec_int4	__vector signed int
#define vec_bint4	__vector bool int

#define vec_float4	__vector float

#define vec_ullong2	__vector bool char
#define vec_llong2	__vector bool short

#define vec_double2	__vector bool int

#endif /* _VEC_TYPES_H_ */
