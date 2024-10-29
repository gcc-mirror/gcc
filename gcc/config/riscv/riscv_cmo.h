/* RISC-V CMO Extension intrinsics include file.
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

#ifndef __RISCV_CMO_H
#define __RISCV_CMO_H

#if defined (__riscv_zicbom)

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_cmo_clean (void *addr)
{
    __builtin_riscv_zicbom_cbo_clean (addr);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_cmo_flush (void *addr)
{
    __builtin_riscv_zicbom_cbo_flush (addr);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_cmo_inval (void *addr)
{
    __builtin_riscv_zicbom_cbo_inval (addr);
}

#endif // __riscv_zicbom

#if defined (__riscv_zicbop)

# define rnum 1

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_cmo_prefetch (void *addr, const int vs1, const int vs2)
{
    __builtin_prefetch (addr,vs1,vs2);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_cmo_prefetchi ()
{
    return __builtin_riscv_zicbop_cbo_prefetchi (rnum);
}

#endif // __riscv_zicbop

#if defined (__riscv_zicboz)

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__riscv_cmo_zero (void *addr)
{
    __builtin_riscv_zicboz_cbo_zero (addr);
}

#endif // __riscv_zicboz

#endif // __RISCV_CMO_H
