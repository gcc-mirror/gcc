/* PowerPC support for accessing the AUXV AT_PLATFORM, AT_HWCAP and AT_HWCAP2
   values from the Thread Control Block (TCB).

   Copyright (C) 2016-2020 Free Software Foundation, Inc.
   Contributed by Peter Bergner <bergner@vnet.ibm.com>.

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

#ifndef _PPC_AUXV_H
#define _PPC_AUXV_H

/* The PLATFORM value stored in the TCB is offset by _DL_FIRST_PLATFORM.  */
#define _DL_FIRST_PLATFORM             32

/* AT_PLATFORM bits.  These must match the values defined in GLIBC. */
#define PPC_PLATFORM_POWER4            0
#define PPC_PLATFORM_PPC970            1
#define PPC_PLATFORM_POWER5            2
#define PPC_PLATFORM_POWER5_PLUS       3
#define PPC_PLATFORM_POWER6            4
#define PPC_PLATFORM_CELL_BE           5
#define PPC_PLATFORM_POWER6X           6
#define PPC_PLATFORM_POWER7            7
#define PPC_PLATFORM_PPCA2             8
#define PPC_PLATFORM_PPC405            9
#define PPC_PLATFORM_PPC440            10
#define PPC_PLATFORM_PPC464            11
#define PPC_PLATFORM_PPC476            12
#define PPC_PLATFORM_POWER8            13
#define PPC_PLATFORM_POWER9            14

/* AT_HWCAP bits.  These must match the values defined in the Linux kernel.  */
#define PPC_FEATURE_32              0x80000000
#define PPC_FEATURE_64              0x40000000
#define PPC_FEATURE_601_INSTR       0x20000000
#define PPC_FEATURE_HAS_ALTIVEC     0x10000000
#define PPC_FEATURE_HAS_FPU         0x08000000
#define PPC_FEATURE_HAS_MMU         0x04000000
#define PPC_FEATURE_HAS_4xxMAC      0x02000000
#define PPC_FEATURE_UNIFIED_CACHE   0x01000000
#define PPC_FEATURE_HAS_SPE         0x00800000
#define PPC_FEATURE_HAS_EFP_SINGLE  0x00400000
#define PPC_FEATURE_HAS_EFP_DOUBLE  0x00200000
#define PPC_FEATURE_NO_TB           0x00100000
#define PPC_FEATURE_POWER4          0x00080000
#define PPC_FEATURE_POWER5          0x00040000
#define PPC_FEATURE_POWER5_PLUS     0x00020000
#define PPC_FEATURE_CELL_BE         0x00010000
#define PPC_FEATURE_BOOKE           0x00008000
#define PPC_FEATURE_SMT             0x00004000
#define PPC_FEATURE_ICACHE_SNOOP    0x00002000
#define PPC_FEATURE_ARCH_2_05       0x00001000
#define PPC_FEATURE_PA6T            0x00000800
#define PPC_FEATURE_HAS_DFP         0x00000400
#define PPC_FEATURE_POWER6_EXT      0x00000200
#define PPC_FEATURE_ARCH_2_06       0x00000100
#define PPC_FEATURE_HAS_VSX         0x00000080
#define PPC_FEATURE_PERFMON_COMPAT  0x00000040
#define PPC_FEATURE_TRUE_LE         0x00000002
#define PPC_FEATURE_PPC_LE          0x00000001

/* AT_HWCAP2 bits.  These must match the values defined in the Linux kernel.  */
#define PPC_FEATURE2_ARCH_2_07      0x80000000
#define PPC_FEATURE2_HAS_HTM        0x40000000
#define PPC_FEATURE2_HAS_DSCR       0x20000000
#define PPC_FEATURE2_HAS_EBB        0x10000000
#define PPC_FEATURE2_HAS_ISEL       0x08000000
#define PPC_FEATURE2_HAS_TAR        0x04000000
#define PPC_FEATURE2_HAS_VEC_CRYPTO 0x02000000
#define PPC_FEATURE2_HTM_NOSC       0x01000000
#define PPC_FEATURE2_ARCH_3_00      0x00800000
#define PPC_FEATURE2_HAS_IEEE128    0x00400000
#define PPC_FEATURE2_DARN           0x00200000
#define PPC_FEATURE2_SCV            0x00100000
#define PPC_FEATURE2_HTM_NO_SUSPEND 0x00080000


/* Thread Control Block (TCB) offsets of the AT_PLATFORM, AT_HWCAP and
   AT_HWCAP2 values.  These must match the values defined in GLIBC.  */
#define TCB_PLATFORM_OFFSET ((TARGET_64BIT) ? -28764 : -28724)
#define TCB_HWCAP_BASE_OFFSET ((TARGET_64BIT) ? -28776 : -28736)
#define TCB_HWCAP1_OFFSET \
  ((BYTES_BIG_ENDIAN) ? TCB_HWCAP_BASE_OFFSET : TCB_HWCAP_BASE_OFFSET+4)
#define TCB_HWCAP2_OFFSET \
  ((BYTES_BIG_ENDIAN) ? TCB_HWCAP_BASE_OFFSET+4 : TCB_HWCAP_BASE_OFFSET)
#define TCB_HWCAP_OFFSET(ID) \
  (((ID) == 0) ? TCB_HWCAP1_OFFSET : TCB_HWCAP2_OFFSET)

#endif /* _PPC_AUXV_H */
