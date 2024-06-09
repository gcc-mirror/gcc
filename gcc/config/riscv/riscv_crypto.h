/* RISC-V 'Scalar Crypto' Extension intrinsics include file.
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

#ifndef __RISCV_SCALAR_CRYPTO_H
#define __RISCV_SCALAR_CRYPTO_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined (__riscv_zknd)

#if __riscv_xlen == 32

#ifdef __OPTIMIZE__

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes32dsi (uint32_t rs1, uint32_t rs2, const int bs)
{
  return __builtin_riscv_aes32dsi (rs1,rs2,bs);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes32dsmi (uint32_t rs1, uint32_t rs2, const int bs)
{
  return __builtin_riscv_aes32dsmi (rs1,rs2,bs);
}

#else
#define __riscv_aes32dsi(x, y, bs) __builtin_riscv_aes32dsi (x, y, bs)
#define __riscv_aes32dsmi(x, y, bs) __builtin_riscv_aes32dsmi (x, y, bs)
#endif

#endif

#if __riscv_xlen == 64

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes64ds (uint64_t rs1, uint64_t rs2)
{
  return __builtin_riscv_aes64ds (rs1,rs2);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes64dsm (uint64_t rs1, uint64_t rs2)
{
  return __builtin_riscv_aes64dsm (rs1,rs2);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes64im (uint64_t rs1)
{
  return __builtin_riscv_aes64im (rs1);
}
#endif
#endif // __riscv_zknd

#if (defined (__riscv_zknd) || defined (__riscv_zkne)) && (__riscv_xlen == 64)

#ifdef __OPTIMIZE__

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes64ks1i (uint64_t rs1, const int rnum)
{
  return __builtin_riscv_aes64ks1i (rs1,rnum);
}

#else
#define __riscv_aes64ks1i(x, rnum) __builtin_riscv_aes64ks1i (x, rnum)
#endif

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes64ks2 (uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_aes64ks2 (rs1,rs2);
}

#endif // __riscv_zknd || __riscv_zkne

#if defined (__riscv_zkne)

#if __riscv_xlen == 32

#ifdef __OPTIMIZE__

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes32esi (uint32_t rs1, uint32_t rs2, const int bs)
{
  return __builtin_riscv_aes32esi (rs1,rs2,bs);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes32esmi (uint32_t rs1, uint32_t rs2, const int bs)
{
  return __builtin_riscv_aes32esmi (rs1,rs2,bs);
}

#else
#define __riscv_aes32esi(x, y, bs) __builtin_riscv_aes32esi (x, y, bs)
#define __riscv_aes32esmi(x, y, bs) __builtin_riscv_aes32esmi (x, y, bs)
#endif

#endif

#if __riscv_xlen == 64

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes64es (uint64_t rs1,uint64_t rs2)
{
  return __builtin_riscv_aes64es (rs1,rs2);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_aes64esm (uint64_t rs1,uint64_t rs2)
{
  return __builtin_riscv_aes64esm (rs1,rs2);
}
#endif
#endif // __riscv_zkne

#if defined (__riscv_zknh)

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha256sig0 (uint32_t rs1)
{
  return __builtin_riscv_sha256sig0 (rs1);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha256sig1 (uint32_t rs1)
{
  return __builtin_riscv_sha256sig1 (rs1);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha256sum0 (uint32_t rs1)
{
  return __builtin_riscv_sha256sum0 (rs1);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha256sum1 (uint32_t rs1)
{
  return __builtin_riscv_sha256sum1 (rs1);
}

#if __riscv_xlen == 32

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sig0h (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sha512sig0h (rs1,rs2);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sig0l (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sha512sig0l (rs1,rs2);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sig1h (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sha512sig1h (rs1,rs2);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sig1l (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sha512sig1l (rs1,rs2);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sum0r (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sha512sum0r (rs1,rs2);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sum1r (uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sha512sum1r (rs1,rs2);
}

#endif

#if __riscv_xlen == 64

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sig0 (uint64_t rs1)
{
  return __builtin_riscv_sha512sig0 (rs1);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sig1 (uint64_t rs1)
{
  return __builtin_riscv_sha512sig1 (rs1);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sum0 (uint64_t rs1)
{
  return __builtin_riscv_sha512sum0 (rs1);
}

extern __inline uint64_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sha512sum1 (uint64_t rs1)
{
  return __builtin_riscv_sha512sum1 (rs1);
}
#endif
#endif // __riscv_zknh

#if defined (__riscv_zksh)

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sm3p0 (uint32_t rs1)
{
    return __builtin_riscv_sm3p0 (rs1);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sm3p1 (uint32_t rs1)
{
    return __builtin_riscv_sm3p1 (rs1);
}

#endif // __riscv_zksh

#if defined (__riscv_zksed)

#ifdef __OPTIMIZE__

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sm4ed (uint32_t rs1, uint32_t rs2, const int bs)
{
  return __builtin_riscv_sm4ed (rs1,rs2,bs);
}

extern __inline uint32_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_sm4ks (uint32_t rs1, uint32_t rs2, const int bs)
{
  return __builtin_riscv_sm4ks (rs1,rs2,bs);
}

#else
#define __riscv_sm4ed(x, y, bs) __builtin_riscv_sm4ed(x, y, bs);
#define __riscv_sm4ks(x, y, bs) __builtin_riscv_sm4ks(x, y, bs);
#endif

#endif // __riscv_zksed

#if defined (__cplusplus)
}
#endif // __cplusplus
#endif // __RISCV_SCALAR_CRYPTO_H
