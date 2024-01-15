/* RISC-V  Bitmanip Extension intrinsics include file.
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

#ifndef __RISCV_BITMANIP_H
#define __RISCV_BITMANIP_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined (__riscv_zbb)

extern __inline unsigned
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_clz_32 (uint32_t x)
{
  return __builtin_riscv_clz_32 (x);
}

extern __inline unsigned
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_ctz_32 (uint32_t x)
{
  return __builtin_riscv_ctz_32 (x);
}

extern __inline unsigned
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_cpop_32 (uint32_t x)
{
  return __builtin_riscv_popcount_32 (x);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_orc_b_32 (uint32_t x)
{
  return __builtin_riscv_orc_b_32 (x);
}

#if __riscv_xlen == 64

extern __inline unsigned
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_clz_64 (uint64_t x)
{
  return __builtin_riscv_clz_64 (x);
}

extern __inline unsigned
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_ctz_64 (uint64_t x)
{
  return __builtin_riscv_ctz_64 (x);
}

extern __inline unsigned
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_cpop_64 (uint64_t x)
{
  return __builtin_riscv_popcount_64 (x);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_orc_b_64 (uint64_t x)
{
  return __builtin_riscv_orc_b_64 (x);
}

#endif

#endif // __riscv_zbb

#if defined (__riscv_zbb) || defined (__riscv_zbkb)

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_ror_32 (uint32_t x, uint32_t shamt)
{
    return __builtin_riscv_ror_32 (x,shamt);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_rol_32 (uint32_t x, uint32_t shamt)
{
    return __builtin_riscv_rol_32 (x,shamt);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_rev8_32 (uint32_t x)
{
    return __builtin_bswap32 (x);
}

#if __riscv_xlen == 64

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_ror_64 (uint64_t x, uint32_t shamt)
{
    return __builtin_riscv_ror_64 (x,shamt);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_rol_64 (uint64_t x, uint32_t shamt)
{
    return __builtin_riscv_rol_64 (x,shamt);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_rev8_64 (uint64_t x)
{
    return __builtin_bswap64 (x);
}

#endif

#endif // __riscv_zbb || __riscv_zbkb

#if defined (__riscv_zbkb)

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_brev8_32 (uint32_t x)
{
    return __builtin_riscv_brev8_32 (x);
}

#if __riscv_xlen == 32

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_zip_32 (uint32_t x)
{
    return __builtin_riscv_zip (x);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_unzip_32 (uint32_t x)
{
    return __builtin_riscv_unzip (x);
}

#endif

#if __riscv_xlen == 64

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_brev8_64 (uint64_t x)
{
    return __builtin_riscv_brev8_64 (x);
}

#endif

#endif // __riscv_zbkb

#if defined (__riscv_zbc) || defined (__riscv_zbkc)

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_clmul_32 (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_clmul_32 (rs1,rs2);
}

#if __riscv_xlen == 32

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_clmulh_32 (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_clmulh_32 (rs1,rs2);
}

#endif

#if __riscv_xlen == 64

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_clmul_64 (uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_clmul_64 (rs1,rs2);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_clmulh_64 (uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_clmulh_64 (rs1,rs2);
}

#endif

#endif // __riscv_zbc || __riscv_zbkc

#if defined (__riscv_zbc)

#if __riscv_xlen == 32

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_clmulr_32 (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_clmulr_32 (rs1,rs2);
}

#endif

#if __riscv_xlen == 64

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_clmulr_64 (uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_clmulr_64 (rs1,rs2);
}

#endif

#endif // __riscv_zbc

#if defined (__riscv_zbkx)

#if __riscv_xlen == 32

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_xperm4_32 (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_xperm4 (rs1,rs2);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_xperm8_32 (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_xperm8 (rs1,rs2);
}

#endif

#if __riscv_xlen == 64

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_xperm4_64 (uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_xperm4 (rs1,rs2);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_xperm8_64 (uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_xperm8 (rs1,rs2);
}

#endif

#endif // __riscv_zbkx

#if defined (__cplusplus)
}
#endif // __cplusplus
#endif // __RISCV_BITMANIP_H