/* RISC-V 'XTheadVector' Extension intrinsics include file.
   Copyright (C) 2024 Free Software Foundation, Inc.

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

#ifndef __RISCV_TH_VECTOR_H
#define __RISCV_TH_VECTOR_H

#include <stdint.h>
#include <stddef.h>

#ifndef __riscv_xtheadvector
#error "XTheadVector intrinsics require the xtheadvector extension."
#else
#ifdef __cplusplus
extern "C" {
#endif

/* NOTE: This implementation of riscv_th_vector.h is intentionally short.  It does
   not define the RVV types and intrinsic functions directly in C and C++
   code, but instead uses the following pragma to tell GCC to insert the
   necessary type and function definitions itself.  The net effect is the
   same, and the file is a complete implementation of riscv_th_vector.h.  */
#pragma riscv intrinsic "xtheadvector"

#ifdef __cplusplus
}
#endif // __cplusplus
#endif // __riscv_xtheadvector
#endif // __RISCV_TH_ECTOR_H
