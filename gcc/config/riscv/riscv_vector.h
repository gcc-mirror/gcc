/* RISC-V 'V' Extension intrinsics include file.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.

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

#ifndef __riscv_vector
#error "Vector intrinsics require the vector extension."
#else
#ifdef __cplusplus
extern "C" {
#endif

enum RVV_CSR {
  RVV_VSTART = 0,
  RVV_VXSAT,
  RVV_VXRM,
  RVV_VCSR,
};

__extension__ extern __inline unsigned long
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vread_csr(enum RVV_CSR csr)
{
  unsigned long rv = 0;
  switch (csr)
    {
    case RVV_VSTART:
      __asm__ __volatile__ ("csrr\t%0,vstart" : "=r"(rv) : : "memory");
      break;
    case RVV_VXSAT:
      __asm__ __volatile__ ("csrr\t%0,vxsat" : "=r"(rv) : : "memory");
      break;
    case RVV_VXRM:
      __asm__ __volatile__ ("csrr\t%0,vxrm" : "=r"(rv) : : "memory");
      break;
    case RVV_VCSR:
      __asm__ __volatile__ ("csrr\t%0,vcsr" : "=r"(rv) : : "memory");
      break;
    }
  return rv;
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vwrite_csr(enum RVV_CSR csr, unsigned long value)
{
  switch (csr)
    {
    case RVV_VSTART:
      __asm__ __volatile__ ("csrw\tvstart,%z0" : : "rJ"(value) : "memory");
      break;
    case RVV_VXSAT:
      __asm__ __volatile__ ("csrw\tvxsat,%z0" : : "rJ"(value) : "memory");
      break;
    case RVV_VXRM:
      __asm__ __volatile__ ("csrw\tvxrm,%z0" : : "rJ"(value) : "memory");
      break;
    case RVV_VCSR:
      __asm__ __volatile__ ("csrw\tvcsr,%z0" : : "rJ"(value) : "memory");
      break;
    }
}

/* NOTE: This implementation of riscv_vector.h is intentionally short.  It does
   not define the RVV types and intrinsic functions directly in C and C++
   code, but instead uses the following pragma to tell GCC to insert the
   necessary type and function definitions itself.  The net effect is the
   same, and the file is a complete implementation of riscv_vector.h.  */
#pragma riscv intrinsic "vector"

#ifdef __cplusplus
}
#endif // __cplusplus
#endif // __riscv_vector
#endif // __RISCV_VECTOR_H
