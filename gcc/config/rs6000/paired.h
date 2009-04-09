/* PowerPC 750CL user include file.
   Copyright (C) 2007, 2009 Free Software Foundation, Inc. 
   Contributed by Revital Eres (eres@il.ibm.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _PAIRED_H
#define _PAIRED_H

#define vector __attribute__((vector_size(8)))

#define paired_msub __builtin_paired_msub
#define paired_madd __builtin_paired_madd
#define paired_nmsub __builtin_paired_nmsub
#define paired_nmadd __builtin_paired_nmadd
#define paired_sum0 __builtin_paired_sum0
#define paired_sum1 __builtin_paired_sum1
#define paired_div __builtin_paired_divv2sf3
#define paired_add __builtin_paired_addv2sf3
#define paired_sub __builtin_paired_subv2sf3
#define paired_mul __builtin_paired_mulv2sf3
#define paired_muls0 __builtin_paired_muls0
#define paired_muls1 __builtin_paired_muls1
#define paired_madds0 __builtin_paired_madds0
#define paired_madds1 __builtin_paired_madds1
#define paired_merge00 __builtin_paired_merge00
#define paired_merge01 __builtin_paired_merge01
#define paired_merge10 __builtin_paired_merge10
#define paired_merge11 __builtin_paired_merge11
#define paired_abs __builtin_paired_absv2sf2
#define paired_nabs __builtin_paired_nabsv2sf2
#define paired_neg __builtin_paired_negv2sf2
#define paired_sqrt __builtin_paired_sqrtv2sf2
#define paired_res __builtin_paired_resv2sf2
#define paired_stx __builtin_paired_stx
#define paired_lx __builtin_paired_lx
#define paired_cmpu0 __builtin_paired_cmpu0
#define paired_cmpu1 __builtin_paired_cmpu1
#define paired_sel __builtin_paired_selv2sf4

/* Condition register codes for Paired predicates. */
#define LT            0
#define GT            1
#define EQ            2
#define UN            3

#define paired_cmpu0_un(a,b) __builtin_paired_cmpu0 (UN, (a), (b))
#define paired_cmpu0_eq(a,b) __builtin_paired_cmpu0 (EQ, (a), (b))
#define paired_cmpu0_lt(a,b) __builtin_paired_cmpu0 (LT, (a), (b))
#define paired_cmpu0_gt(a,b) __builtin_paired_cmpu0 (GT, (a), (b))
#define paired_cmpu1_un(a,b) __builtin_paired_cmpu1 (UN, (a), (b))
#define paired_cmpu1_eq(a,b) __builtin_paired_cmpu1 (EQ, (a), (b))
#define paired_cmpu1_lt(a,b) __builtin_paired_cmpu1 (LT, (a), (b))
#define paired_cmpu1_gt(a,b) __builtin_paired_cmpu1 (GT, (a), (b))

#endif /* _PAIRED_H */
