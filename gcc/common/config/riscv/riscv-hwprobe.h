/* Definitions for the Linux riscv_hwprobe interface.

   Copyright (C) 2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_RISCV_HWPROBE_H
#define GCC_RISCV_HWPROBE_H

#ifndef __NR_riscv_hwprobe
#define __NR_riscv_hwprobe 258
#endif

enum {
#define RISCV_HWPROBE_KEY(NAME, VALUE) RISCV_HWPROBE_KEY_##NAME = VALUE,
#include "common/config/riscv/riscv-hwprobe.def"
#define RISCV_HWPROBE_EXT(NAME, UPPERCASE_NAME, KEY, BIT, XLEN) \
  RISCV_HWPROBE_EXT_##UPPERCASE_NAME = (1ULL << BIT),
#include "common/config/riscv/riscv-hwprobe.def"
};

#define RISCV_HWPROBE_BASE_BEHAVIOR_IMA (1ULL << 0)

/* What the MVENDORID, MARCHID and MIMPID keys answer with, and the type
   riscv-cores.def records those registers in.  */

typedef unsigned long long riscv_core_id_t;

struct riscv_hwprobe {
  long long key;
  unsigned long long value;
};

static inline long
syscall_5_args (long number, long arg1, long arg2, long arg3,
		long arg4, long arg5)
{
  register long a7 __asm__ ("a7") = number;
  register long a0 __asm__ ("a0") = arg1;
  register long a1 __asm__ ("a1") = arg2;
  register long a2 __asm__ ("a2") = arg3;
  register long a3 __asm__ ("a3") = arg4;
  register long a4 __asm__ ("a4") = arg5;
  __asm__ __volatile__ ("ecall\n\t"
			: "=r"(a0)
			: "r"(a7), "r"(a0), "r"(a1), "r"(a2), "r"(a3), "r"(a4)
			: "memory");
  return a0;
}

static inline long
riscv_hwprobe (struct riscv_hwprobe *hwprobes, long npairs)
{
  return syscall_5_args (__NR_riscv_hwprobe, (long)hwprobes, npairs, 0,
			 0, 0);
}

static inline unsigned long long
riscv_hwprobe_value (const struct riscv_hwprobe *pairs, int npairs,
		     long long key)
{
  int i;

  for (i = 0; i < npairs; i++)
    if (pairs[i].key == key)
      return pairs[i].value;

  return 0;
}

#endif
