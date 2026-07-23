/* RISC-V 'V' Extension intrinsics include file.
   Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

#ifndef __RISCV_VECTOR_H
#define __RISCV_VECTOR_H

#include <stdint.h>
#include <stddef.h>

#define __riscv_intrinsic_v 1
#define __riscv_intrinsic_zve32f 1
#define __riscv_intrinsic_zve32x 1
#define __riscv_intrinsic_zve64d 1
#define __riscv_intrinsic_zve64f 1
#define __riscv_intrinsic_zve64x 1
#define __riscv_intrinsic_zvbb 1
#define __riscv_intrinsic_zvbc 1
#define __riscv_intrinsic_zvfbfmin 1
#define __riscv_intrinsic_zvfbfwma 1
#define __riscv_intrinsic_zvfh 1
#define __riscv_intrinsic_zvfhmin 1
#define __riscv_intrinsic_zvkb 1
#define __riscv_intrinsic_zvkg 1
#define __riscv_intrinsic_zvkned 1
#define __riscv_intrinsic_zvknha 1
#define __riscv_intrinsic_zvknhb 1
#define __riscv_intrinsic_zvksed 1
#define __riscv_intrinsic_zvksh 1
#define __riscv_intrinsic_zvabd 1

#if defined (__riscv_intrinsic_zvkned) \
    && defined (__riscv_intrinsic_zvknhb) \
    && defined (__riscv_intrinsic_zvkb)
#define __riscv_intrinsic_zvkn 1
#endif

#if defined (__riscv_intrinsic_zvkn) && defined (__riscv_intrinsic_zvbc)
#define __riscv_intrinsic_zvknc 1
#endif

#if defined (__riscv_intrinsic_zvkn) && defined (__riscv_intrinsic_zvkg)
#define __riscv_intrinsic_zvkng 1
#endif

#if defined (__riscv_intrinsic_zvksed) \
    && defined (__riscv_intrinsic_zvksh) \
    && defined (__riscv_intrinsic_zvkb)
#define __riscv_intrinsic_zvks 1
#endif

#if defined (__riscv_intrinsic_zvks) && defined (__riscv_intrinsic_zvbc)
#define __riscv_intrinsic_zvksc 1
#endif

#if defined (__riscv_intrinsic_zvks) && defined (__riscv_intrinsic_zvkg)
#define __riscv_intrinsic_zvksg 1
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* NOTE: This implementation of riscv_vector.h is intentionally short.  It does
   not define the RVV types and intrinsic functions directly in C and C++
   code, but instead uses the following pragma to tell GCC to insert the
   necessary type and function definitions itself.  The net effect is the
   same, and the file is a complete implementation of riscv_vector.h.  */
#pragma riscv intrinsic "vector"

#ifdef __cplusplus
}
#endif // __cplusplus
#endif // __RISCV_VECTOR_H
