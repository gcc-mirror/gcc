/* -*- buffer-read-only: t -*-
   Generated automatically by parsecpu.awk from arm-cpus.in.
   Do not edit.

   Copyright (C) 2011-2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3,
   or (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

static const struct processors all_cores[] =
{
  {
    "arm2",
    TARGET_CPU_arm2,
    (TF_CO_PROC | TF_NO_MODE32),
    "2", BASE_ARCH_2,
    {
      ISA_ARMv2,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm250",
    TARGET_CPU_arm250,
    (TF_CO_PROC | TF_NO_MODE32),
    "2", BASE_ARCH_2,
    {
      ISA_ARMv2,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm3",
    TARGET_CPU_arm3,
    (TF_CO_PROC | TF_NO_MODE32),
    "2", BASE_ARCH_2,
    {
      ISA_ARMv2,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm6",
    TARGET_CPU_arm6,
    (TF_CO_PROC),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm60",
    TARGET_CPU_arm60,
    (TF_CO_PROC),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm600",
    TARGET_CPU_arm600,
    (TF_CO_PROC | TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm610",
    TARGET_CPU_arm610,
    (TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm620",
    TARGET_CPU_arm620,
    (TF_CO_PROC | TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm7",
    TARGET_CPU_arm7,
    (TF_CO_PROC),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm7d",
    TARGET_CPU_arm7d,
    (TF_CO_PROC),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm7di",
    TARGET_CPU_arm7di,
    (TF_CO_PROC),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm70",
    TARGET_CPU_arm70,
    (TF_CO_PROC),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm700",
    TARGET_CPU_arm700,
    (TF_CO_PROC | TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm700i",
    TARGET_CPU_arm700i,
    (TF_CO_PROC | TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm710",
    TARGET_CPU_arm710,
    (TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm720",
    TARGET_CPU_arm720,
    (TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm710c",
    TARGET_CPU_arm710c,
    (TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm7100",
    TARGET_CPU_arm7100,
    (TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm7500",
    TARGET_CPU_arm7500,
    (TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm7500fe",
    TARGET_CPU_arm7500fe,
    (TF_CO_PROC | TF_WBUF),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    &arm_slowmul_tune
  },
  {
    "arm7m",
    TARGET_CPU_arm7m,
    (TF_CO_PROC),
    "3M", BASE_ARCH_3M,
    {
      ISA_ARMv3m,isa_bit_mode26,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm7dm",
    TARGET_CPU_arm7dm,
    (TF_CO_PROC),
    "3M", BASE_ARCH_3M,
    {
      ISA_ARMv3m,isa_bit_mode26,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm7dmi",
    TARGET_CPU_arm7dmi,
    (TF_CO_PROC),
    "3M", BASE_ARCH_3M,
    {
      ISA_ARMv3m,isa_bit_mode26,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm8",
    TARGET_CPU_arm8,
    (TF_LDSCHED),
    "4", BASE_ARCH_4,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm810",
    TARGET_CPU_arm810,
    (TF_LDSCHED),
    "4", BASE_ARCH_4,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "strongarm",
    TARGET_CPU_strongarm,
    (TF_LDSCHED | TF_STRONG),
    "4", BASE_ARCH_4,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    &arm_strongarm_tune
  },
  {
    "strongarm110",
    TARGET_CPU_strongarm110,
    (TF_LDSCHED | TF_STRONG),
    "4", BASE_ARCH_4,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    &arm_strongarm_tune
  },
  {
    "strongarm1100",
    TARGET_CPU_strongarm1100,
    (TF_LDSCHED | TF_STRONG),
    "4", BASE_ARCH_4,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    &arm_strongarm_tune
  },
  {
    "strongarm1110",
    TARGET_CPU_strongarm1110,
    (TF_LDSCHED | TF_STRONG),
    "4", BASE_ARCH_4,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    &arm_strongarm_tune
  },
  {
    "fa526",
    TARGET_CPU_fa526,
    (TF_LDSCHED),
    "4", BASE_ARCH_4,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "fa626",
    TARGET_CPU_fa626,
    (TF_LDSCHED),
    "4", BASE_ARCH_4,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm7tdmi",
    TARGET_CPU_arm7tdmi,
    (TF_CO_PROC),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm7tdmi-s",
    TARGET_CPU_arm7tdmis,
    (TF_CO_PROC),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm710t",
    TARGET_CPU_arm710t,
    (TF_WBUF),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm720t",
    TARGET_CPU_arm720t,
    (TF_WBUF),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm740t",
    TARGET_CPU_arm740t,
    (TF_WBUF),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm9",
    TARGET_CPU_arm9,
    (TF_LDSCHED),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm9tdmi",
    TARGET_CPU_arm9tdmi,
    (TF_LDSCHED),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm920",
    TARGET_CPU_arm920,
    (TF_LDSCHED),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm920t",
    TARGET_CPU_arm920t,
    (TF_LDSCHED),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm922t",
    TARGET_CPU_arm922t,
    (TF_LDSCHED),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm940t",
    TARGET_CPU_arm940t,
    (TF_LDSCHED),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "ep9312",
    TARGET_CPU_ep9312,
    (TF_LDSCHED),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm10tdmi",
    TARGET_CPU_arm10tdmi,
    (TF_LDSCHED),
    "5T", BASE_ARCH_5T,
    {
      ISA_ARMv5t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm1020t",
    TARGET_CPU_arm1020t,
    (TF_LDSCHED),
    "5T", BASE_ARCH_5T,
    {
      ISA_ARMv5t,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm9e",
    TARGET_CPU_arm9e,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm946e-s",
    TARGET_CPU_arm946es,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm966e-s",
    TARGET_CPU_arm966es,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm968e-s",
    TARGET_CPU_arm968es,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm10e",
    TARGET_CPU_arm10e,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm1020e",
    TARGET_CPU_arm1020e,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "arm1022e",
    TARGET_CPU_arm1022e,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_fastmul_tune
  },
  {
    "xscale",
    TARGET_CPU_xscale,
    (TF_LDSCHED | TF_XSCALE),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_bit_xscale,
      isa_nobit
    },
    &arm_xscale_tune
  },
  {
    "iwmmxt",
    TARGET_CPU_iwmmxt,
    (TF_LDSCHED | TF_XSCALE),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,isa_bit_xscale,isa_bit_iwmmxt,
      isa_nobit
    },
    &arm_xscale_tune
  },
  {
    "iwmmxt2",
    TARGET_CPU_iwmmxt2,
    (TF_LDSCHED | TF_XSCALE),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,isa_bit_xscale,isa_bit_iwmmxt,isa_bit_iwmmxt2,
      isa_nobit
    },
    &arm_xscale_tune
  },
  {
    "fa606te",
    TARGET_CPU_fa606te,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "fa626te",
    TARGET_CPU_fa626te,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "fmp626",
    TARGET_CPU_fmp626,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "fa726te",
    TARGET_CPU_fa726te,
    (TF_LDSCHED),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    &arm_fa726te_tune
  },
  {
    "arm926ej-s",
    TARGET_CPU_arm926ejs,
    (TF_LDSCHED),
    "5TEJ", BASE_ARCH_5TEJ,
    {
      ISA_ARMv5tej,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm1026ej-s",
    TARGET_CPU_arm1026ejs,
    (TF_LDSCHED),
    "5TEJ", BASE_ARCH_5TEJ,
    {
      ISA_ARMv5tej,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm1136j-s",
    TARGET_CPU_arm1136js,
    (TF_LDSCHED),
    "6J", BASE_ARCH_6J,
    {
      ISA_ARMv6j,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm1136jf-s",
    TARGET_CPU_arm1136jfs,
    (TF_LDSCHED),
    "6J", BASE_ARCH_6J,
    {
      ISA_ARMv6j,
      ISA_VFPv2,ISA_FP_DBL,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm1176jz-s",
    TARGET_CPU_arm1176jzs,
    (TF_LDSCHED),
    "6KZ", BASE_ARCH_6KZ,
    {
      ISA_ARMv6kz,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm1176jzf-s",
    TARGET_CPU_arm1176jzfs,
    (TF_LDSCHED),
    "6KZ", BASE_ARCH_6KZ,
    {
      ISA_ARMv6kz,
      ISA_VFPv2,ISA_FP_DBL,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "mpcorenovfp",
    TARGET_CPU_mpcorenovfp,
    (TF_LDSCHED),
    "6K", BASE_ARCH_6K,
    {
      ISA_ARMv6k,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "mpcore",
    TARGET_CPU_mpcore,
    (TF_LDSCHED),
    "6K", BASE_ARCH_6K,
    {
      ISA_ARMv6k,
      ISA_VFPv2,ISA_FP_DBL,
      isa_nobit
    },
    &arm_9e_tune
  },
  {
    "arm1156t2-s",
    TARGET_CPU_arm1156t2s,
    (TF_LDSCHED),
    "6T2", BASE_ARCH_6T2,
    {
      ISA_ARMv6t2,
      isa_nobit
    },
    &arm_v6t2_tune
  },
  {
    "arm1156t2f-s",
    TARGET_CPU_arm1156t2fs,
    (TF_LDSCHED),
    "6T2", BASE_ARCH_6T2,
    {
      ISA_ARMv6t2,
      ISA_VFPv2,ISA_FP_DBL,
      isa_nobit
    },
    &arm_v6t2_tune
  },
  {
    "cortex-m1",
    TARGET_CPU_cortexm1,
    (TF_LDSCHED),
    "6M", BASE_ARCH_6M,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    &arm_v6m_tune
  },
  {
    "cortex-m0",
    TARGET_CPU_cortexm0,
    (TF_LDSCHED),
    "6M", BASE_ARCH_6M,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    &arm_v6m_tune
  },
  {
    "cortex-m0plus",
    TARGET_CPU_cortexm0plus,
    (TF_LDSCHED),
    "6M", BASE_ARCH_6M,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    &arm_v6m_tune
  },
  {
    "cortex-m1.small-multiply",
    TARGET_CPU_cortexm1,
    (TF_LDSCHED | TF_SMALLMUL),
    "6M", BASE_ARCH_6M,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    &arm_v6m_tune
  },
  {
    "cortex-m0.small-multiply",
    TARGET_CPU_cortexm0,
    (TF_LDSCHED | TF_SMALLMUL),
    "6M", BASE_ARCH_6M,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    &arm_v6m_tune
  },
  {
    "cortex-m0plus.small-multiply",
    TARGET_CPU_cortexm0plus,
    (TF_LDSCHED | TF_SMALLMUL),
    "6M", BASE_ARCH_6M,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    &arm_v6m_tune
  },
  {
    "generic-armv7-a",
    TARGET_CPU_genericv7a,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7a,
      isa_nobit
    },
    &arm_cortex_tune
  },
  {
    "cortex-a5",
    TARGET_CPU_cortexa5,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7a,
      isa_nobit
    },
    &arm_cortex_a5_tune
  },
  {
    "cortex-a7",
    TARGET_CPU_cortexa7,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7ve,
      isa_nobit
    },
    &arm_cortex_a7_tune
  },
  {
    "cortex-a8",
    TARGET_CPU_cortexa8,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7a,
      isa_nobit
    },
    &arm_cortex_a8_tune
  },
  {
    "cortex-a9",
    TARGET_CPU_cortexa9,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7a,
      isa_nobit
    },
    &arm_cortex_a9_tune
  },
  {
    "cortex-a12",
    TARGET_CPU_cortexa17,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7ve,
      isa_nobit
    },
    &arm_cortex_a12_tune
  },
  {
    "cortex-a15",
    TARGET_CPU_cortexa15,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7ve,
      isa_nobit
    },
    &arm_cortex_a15_tune
  },
  {
    "cortex-a17",
    TARGET_CPU_cortexa17,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7ve,
      isa_nobit
    },
    &arm_cortex_a12_tune
  },
  {
    "cortex-r4",
    TARGET_CPU_cortexr4,
    (TF_LDSCHED),
    "7R", BASE_ARCH_7R,
    {
      ISA_ARMv7r,
      isa_nobit
    },
    &arm_cortex_tune
  },
  {
    "cortex-r4f",
    TARGET_CPU_cortexr4f,
    (TF_LDSCHED),
    "7R", BASE_ARCH_7R,
    {
      ISA_ARMv7r,
      isa_nobit
    },
    &arm_cortex_tune
  },
  {
    "cortex-r5",
    TARGET_CPU_cortexr5,
    (TF_LDSCHED),
    "7R", BASE_ARCH_7R,
    {
      ISA_ARMv7r,
      isa_bit_adiv,
      isa_nobit
    },
    &arm_cortex_tune
  },
  {
    "cortex-r7",
    TARGET_CPU_cortexr7,
    (TF_LDSCHED),
    "7R", BASE_ARCH_7R,
    {
      ISA_ARMv7r,
      isa_bit_adiv,
      isa_nobit
    },
    &arm_cortex_tune
  },
  {
    "cortex-r8",
    TARGET_CPU_cortexr7,
    (TF_LDSCHED),
    "7R", BASE_ARCH_7R,
    {
      ISA_ARMv7r,
      isa_bit_adiv,
      isa_nobit
    },
    &arm_cortex_tune
  },
  {
    "cortex-m7",
    TARGET_CPU_cortexm7,
    (TF_LDSCHED),
    "7EM", BASE_ARCH_7EM,
    {
      ISA_ARMv7em,
      isa_quirk_no_volatile_ce,
      isa_nobit
    },
    &arm_cortex_m7_tune
  },
  {
    "cortex-m4",
    TARGET_CPU_cortexm4,
    (TF_LDSCHED),
    "7EM", BASE_ARCH_7EM,
    {
      ISA_ARMv7em,
      isa_nobit
    },
    &arm_v7m_tune
  },
  {
    "cortex-m3",
    TARGET_CPU_cortexm3,
    (TF_LDSCHED),
    "7M", BASE_ARCH_7M,
    {
      ISA_ARMv7m,
      isa_quirk_cm3_ldrd,
      isa_nobit
    },
    &arm_v7m_tune
  },
  {
    "marvell-pj4",
    TARGET_CPU_marvell_pj4,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7a,
      isa_nobit
    },
    &arm_marvell_pj4_tune
  },
  {
    "cortex-a15.cortex-a7",
    TARGET_CPU_cortexa7,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7ve,
      isa_nobit
    },
    &arm_cortex_a15_tune
  },
  {
    "cortex-a17.cortex-a7",
    TARGET_CPU_cortexa7,
    (TF_LDSCHED),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7ve,
      isa_nobit
    },
    &arm_cortex_a12_tune
  },
  {
    "cortex-a32",
    TARGET_CPU_cortexa53,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a35_tune
  },
  {
    "cortex-a35",
    TARGET_CPU_cortexa53,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a35_tune
  },
  {
    "cortex-a53",
    TARGET_CPU_cortexa53,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a53_tune
  },
  {
    "cortex-a57",
    TARGET_CPU_cortexa57,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a57_tune
  },
  {
    "cortex-a72",
    TARGET_CPU_cortexa57,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a57_tune
  },
  {
    "cortex-a73",
    TARGET_CPU_cortexa57,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a73_tune
  },
  {
    "exynos-m1",
    TARGET_CPU_exynosm1,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_exynosm1_tune
  },
  {
    "falkor",
    TARGET_CPU_cortexa57,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_qdf24xx_tune
  },
  {
    "qdf24xx",
    TARGET_CPU_cortexa57,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_qdf24xx_tune
  },
  {
    "xgene1",
    TARGET_CPU_xgene1,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,
      isa_nobit
    },
    &arm_xgene1_tune
  },
  {
    "cortex-a57.cortex-a53",
    TARGET_CPU_cortexa53,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a57_tune
  },
  {
    "cortex-a72.cortex-a53",
    TARGET_CPU_cortexa53,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a57_tune
  },
  {
    "cortex-a73.cortex-a35",
    TARGET_CPU_cortexa53,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a73_tune
  },
  {
    "cortex-a73.cortex-a53",
    TARGET_CPU_cortexa53,
    (TF_LDSCHED),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    &arm_cortex_a73_tune
  },
  {
    "cortex-m23",
    TARGET_CPU_cortexm23,
    (TF_LDSCHED),
    "8M_BASE", BASE_ARCH_8M_BASE,
    {
      ISA_ARMv8m_base,
      isa_nobit
    },
    &arm_v6m_tune
  },
  {
    "cortex-m33",
    TARGET_CPU_cortexm33,
    (TF_LDSCHED),
    "8M_MAIN", BASE_ARCH_8M_MAIN,
    {
      ISA_ARMv8m_main,isa_bit_ARMv7em,
      isa_nobit
    },
    &arm_v7m_tune
  },
  {NULL, TARGET_CPU_arm_none, 0, NULL, BASE_ARCH_0, {isa_nobit}, NULL}
};

static const struct processors all_architectures[] =
{
  {
    "armv2", TARGET_CPU_arm2,
    (TF_CO_PROC | TF_NO_MODE32),
    "2", BASE_ARCH_2,
    {
      ISA_ARMv2,isa_bit_mode26,
      isa_nobit
    },
    NULL
  },
  {
    "armv2a", TARGET_CPU_arm2,
    (TF_CO_PROC | TF_NO_MODE32),
    "2", BASE_ARCH_2,
    {
      ISA_ARMv2,isa_bit_mode26,
      isa_nobit
    },
    NULL
  },
  {
    "armv3", TARGET_CPU_arm6,
    (TF_CO_PROC),
    "3", BASE_ARCH_3,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    NULL
  },
  {
    "armv3m", TARGET_CPU_arm7m,
    (TF_CO_PROC),
    "3M", BASE_ARCH_3M,
    {
      ISA_ARMv3m,isa_bit_mode26,
      isa_nobit
    },
    NULL
  },
  {
    "armv4", TARGET_CPU_arm7tdmi,
    (TF_CO_PROC),
    "4", BASE_ARCH_4,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    NULL
  },
  {
    "armv4t", TARGET_CPU_arm7tdmi,
    (TF_CO_PROC),
    "4T", BASE_ARCH_4T,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    NULL
  },
  {
    "armv5", TARGET_CPU_arm10tdmi,
    (TF_CO_PROC),
    "5", BASE_ARCH_5,
    {
      ISA_ARMv5,
      isa_nobit
    },
    NULL
  },
  {
    "armv5t", TARGET_CPU_arm10tdmi,
    (TF_CO_PROC),
    "5T", BASE_ARCH_5T,
    {
      ISA_ARMv5t,
      isa_nobit
    },
    NULL
  },
  {
    "armv5e", TARGET_CPU_arm1026ejs,
    (TF_CO_PROC),
    "5E", BASE_ARCH_5E,
    {
      ISA_ARMv5e,
      isa_nobit
    },
    NULL
  },
  {
    "armv5te", TARGET_CPU_arm1026ejs,
    (TF_CO_PROC),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    NULL
  },
  {
    "armv5tej", TARGET_CPU_arm1026ejs,
    (TF_CO_PROC),
    "5TEJ", BASE_ARCH_5TEJ,
    {
      ISA_ARMv5tej,
      isa_nobit
    },
    NULL
  },
  {
    "armv6", TARGET_CPU_arm1136js,
    (TF_CO_PROC),
    "6", BASE_ARCH_6,
    {
      ISA_ARMv6,
      isa_nobit
    },
    NULL
  },
  {
    "armv6j", TARGET_CPU_arm1136js,
    (TF_CO_PROC),
    "6J", BASE_ARCH_6J,
    {
      ISA_ARMv6j,
      isa_nobit
    },
    NULL
  },
  {
    "armv6k", TARGET_CPU_mpcore,
    (TF_CO_PROC),
    "6K", BASE_ARCH_6K,
    {
      ISA_ARMv6k,
      isa_nobit
    },
    NULL
  },
  {
    "armv6z", TARGET_CPU_arm1176jzs,
    (TF_CO_PROC),
    "6Z", BASE_ARCH_6Z,
    {
      ISA_ARMv6z,
      isa_nobit
    },
    NULL
  },
  {
    "armv6kz", TARGET_CPU_arm1176jzs,
    (TF_CO_PROC),
    "6KZ", BASE_ARCH_6KZ,
    {
      ISA_ARMv6kz,
      isa_nobit
    },
    NULL
  },
  {
    "armv6zk", TARGET_CPU_arm1176jzs,
    (TF_CO_PROC),
    "6KZ", BASE_ARCH_6KZ,
    {
      ISA_ARMv6kz,
      isa_nobit
    },
    NULL
  },
  {
    "armv6t2", TARGET_CPU_arm1156t2s,
    (TF_CO_PROC),
    "6T2", BASE_ARCH_6T2,
    {
      ISA_ARMv6t2,
      isa_nobit
    },
    NULL
  },
  {
    "armv6-m", TARGET_CPU_cortexm1,
    0,
    "6M", BASE_ARCH_6M,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    NULL
  },
  {
    "armv6s-m", TARGET_CPU_cortexm1,
    0,
    "6M", BASE_ARCH_6M,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    NULL
  },
  {
    "armv7", TARGET_CPU_cortexa8,
    (TF_CO_PROC),
    "7", BASE_ARCH_7,
    {
      ISA_ARMv7,
      isa_nobit
    },
    NULL
  },
  {
    "armv7-a", TARGET_CPU_cortexa8,
    (TF_CO_PROC),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7a,
      isa_nobit
    },
    NULL
  },
  {
    "armv7ve", TARGET_CPU_cortexa8,
    (TF_CO_PROC),
    "7A", BASE_ARCH_7A,
    {
      ISA_ARMv7ve,
      isa_nobit
    },
    NULL
  },
  {
    "armv7-r", TARGET_CPU_cortexr4,
    (TF_CO_PROC),
    "7R", BASE_ARCH_7R,
    {
      ISA_ARMv7r,
      isa_nobit
    },
    NULL
  },
  {
    "armv7-m", TARGET_CPU_cortexm3,
    (TF_CO_PROC),
    "7M", BASE_ARCH_7M,
    {
      ISA_ARMv7m,
      isa_nobit
    },
    NULL
  },
  {
    "armv7e-m", TARGET_CPU_cortexm4,
    (TF_CO_PROC),
    "7EM", BASE_ARCH_7EM,
    {
      ISA_ARMv7em,
      isa_nobit
    },
    NULL
  },
  {
    "armv8-a", TARGET_CPU_cortexa53,
    (TF_CO_PROC),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,
      isa_nobit
    },
    NULL
  },
  {
    "armv8-a+crc", TARGET_CPU_cortexa53,
    (TF_CO_PROC),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8a,isa_bit_crc32,
      isa_nobit
    },
    NULL
  },
  {
    "armv8.1-a", TARGET_CPU_cortexa53,
    (TF_CO_PROC),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8_1a,
      isa_nobit
    },
    NULL
  },
  {
    "armv8.2-a", TARGET_CPU_cortexa53,
    (TF_CO_PROC),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8_2a,
      isa_nobit
    },
    NULL
  },
  {
    "armv8.2-a+fp16", TARGET_CPU_cortexa53,
    (TF_CO_PROC),
    "8A", BASE_ARCH_8A,
    {
      ISA_ARMv8_2a,isa_bit_fp16,
      isa_nobit
    },
    NULL
  },
  {
    "armv8-m.base", TARGET_CPU_cortexm23,
    0,
    "8M_BASE", BASE_ARCH_8M_BASE,
    {
      ISA_ARMv8m_base,
      isa_nobit
    },
    NULL
  },
  {
    "armv8-m.main", TARGET_CPU_cortexm7,
    (TF_CO_PROC),
    "8M_MAIN", BASE_ARCH_8M_MAIN,
    {
      ISA_ARMv8m_main,
      isa_nobit
    },
    NULL
  },
  {
    "armv8-m.main+dsp", TARGET_CPU_cortexm33,
    (TF_CO_PROC),
    "8M_MAIN", BASE_ARCH_8M_MAIN,
    {
      ISA_ARMv8m_main,isa_bit_ARMv7em,
      isa_nobit
    },
    NULL
  },
  {
    "iwmmxt", TARGET_CPU_iwmmxt,
    (TF_LDSCHED | TF_STRONG | TF_XSCALE),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,isa_bit_xscale,isa_bit_iwmmxt,
      isa_nobit
    },
    NULL
  },
  {
    "iwmmxt2", TARGET_CPU_iwmmxt2,
    (TF_LDSCHED | TF_STRONG | TF_XSCALE),
    "5TE", BASE_ARCH_5TE,
    {
      ISA_ARMv5te,isa_bit_xscale,isa_bit_iwmmxt,isa_bit_iwmmxt2,
      isa_nobit
    },
    NULL
  },
  {NULL, TARGET_CPU_arm_none, 0, NULL, BASE_ARCH_0, {isa_nobit}, NULL}
};

const struct arm_fpu_desc all_fpus[] =
{
  {
    "vfp",
    {
      ISA_VFPv2,ISA_FP_DBL,
      isa_nobit
    }
  },
  {
    "vfpv2",
    {
      ISA_VFPv2,ISA_FP_DBL,
      isa_nobit
    }
  },
  {
    "vfpv3",
    {
      ISA_VFPv3,ISA_FP_D32,
      isa_nobit
    }
  },
  {
    "vfpv3-fp16",
    {
      ISA_VFPv3,ISA_FP_D32,isa_bit_fp16conv,
      isa_nobit
    }
  },
  {
    "vfpv3-d16",
    {
      ISA_VFPv3,ISA_FP_DBL,
      isa_nobit
    }
  },
  {
    "vfpv3-d16-fp16",
    {
      ISA_VFPv3,ISA_FP_DBL,isa_bit_fp16conv,
      isa_nobit
    }
  },
  {
    "vfpv3xd",
    {
      ISA_VFPv3,
      isa_nobit
    }
  },
  {
    "vfpv3xd-fp16",
    {
      ISA_VFPv3,isa_bit_fp16conv,
      isa_nobit
    }
  },
  {
    "neon",
    {
      ISA_VFPv3,ISA_NEON,
      isa_nobit
    }
  },
  {
    "neon-vfpv3",
    {
      ISA_VFPv3,ISA_NEON,
      isa_nobit
    }
  },
  {
    "neon-fp16",
    {
      ISA_VFPv3,ISA_NEON,isa_bit_fp16conv,
      isa_nobit
    }
  },
  {
    "vfpv4",
    {
      ISA_VFPv4,ISA_FP_D32,
      isa_nobit
    }
  },
  {
    "neon-vfpv4",
    {
      ISA_VFPv4,ISA_NEON,
      isa_nobit
    }
  },
  {
    "vfpv4-d16",
    {
      ISA_VFPv4,ISA_FP_DBL,
      isa_nobit
    }
  },
  {
    "fpv4-sp-d16",
    {
      ISA_VFPv4,
      isa_nobit
    }
  },
  {
    "fpv5-sp-d16",
    {
      ISA_FPv5,
      isa_nobit
    }
  },
  {
    "fpv5-d16",
    {
      ISA_FPv5,ISA_FP_DBL,
      isa_nobit
    }
  },
  {
    "fp-armv8",
    {
      ISA_FP_ARMv8,ISA_FP_D32,
      isa_nobit
    }
  },
  {
    "neon-fp-armv8",
    {
      ISA_FP_ARMv8,ISA_NEON,
      isa_nobit
    }
  },
  {
    "crypto-neon-fp-armv8",
    {
      ISA_FP_ARMv8,ISA_CRYPTO,
      isa_nobit
    }
  },
  {
    "vfp3",
    {
      ISA_VFPv3,ISA_FP_D32,
      isa_nobit
    }
  },
};
