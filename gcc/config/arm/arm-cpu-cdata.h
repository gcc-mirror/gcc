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

static const cpu_arch_extension cpu_opttab_arm9e[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_arm946es[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_arm966es[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_arm968es[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_arm10e[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_arm1020e[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_arm1022e[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_arm926ejs[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_arm1026ejs[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_genericv7a[] = {
  {
    "vfpv3-d16", false, false,
    { ISA_VFPv3,ISA_FP_DBL, isa_nobit }
  },
  {
    "vfpv3", false, false,
    { ISA_VFPv3,ISA_FP_D32, isa_nobit }
  },
  {
    "vfpv3-d16-fp16", false, false,
    { ISA_VFPv3,ISA_FP_DBL,isa_bit_fp16conv, isa_nobit }
  },
  {
    "vfpv3-fp16", false, false,
    { ISA_VFPv3,ISA_FP_D32,isa_bit_fp16conv, isa_nobit }
  },
  {
    "vfpv4-d16", false, false,
    { ISA_VFPv4,ISA_FP_DBL, isa_nobit }
  },
  {
    "vfpv4", false, false,
    { ISA_VFPv4,ISA_FP_D32, isa_nobit }
  },
  {
    "simd", false, false,
    { ISA_VFPv3,ISA_NEON, isa_nobit }
  },
  {
    "neon-fp16", false, false,
    { ISA_VFPv3,ISA_NEON,isa_bit_fp16conv, isa_nobit }
  },
  {
    "neon-vfpv4", false, false,
    { ISA_VFPv4,ISA_NEON, isa_nobit }
  },
  {
    "nosimd", true, false,
    { ISA_ALL_SIMD, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "neon", false, true, 
    { ISA_VFPv3,ISA_NEON, isa_nobit }
  },
  {
    "neon-vfpv3", false, true, 
    { ISA_VFPv3,ISA_NEON, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa5[] = {
  {
    "nosimd", true, false,
    { ISA_ALL_SIMD, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa7[] = {
  {
    "nosimd", true, false,
    { ISA_ALL_SIMD, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa8[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa9[] = {
  {
    "nosimd", true, false,
    { ISA_ALL_SIMD, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa12[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa15[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa17[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexr5[] = {
  {
    "nofp.dp", true, false,
    { ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexr7[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexr8[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexm7[] = {
  {
    "nofp.dp", true, false,
    { ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexm4[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa15cortexa7[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa17cortexa7[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa32[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa35[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa53[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa57[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa72[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa73[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_exynosm1[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_xgene1[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa57cortexa53[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa72cortexa53[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa73cortexa35[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexa73cortexa53[] = {
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const cpu_arch_extension cpu_opttab_cortexm33[] = {
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

const cpu_option all_cores[] =
{
  {
    {
      "arm2",
      NULL,
      {
        ISA_ARMv2,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv2
  },
  {
    {
      "arm250",
      NULL,
      {
        ISA_ARMv2,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv2
  },
  {
    {
      "arm3",
      NULL,
      {
        ISA_ARMv2,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv2
  },
  {
    {
      "arm6",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm60",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm600",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm610",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm620",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm7",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm7d",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm7di",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm70",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm700",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm700i",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm710",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm720",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm710c",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm7100",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm7500",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm7500fe",
      NULL,
      {
        ISA_ARMv3,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3
  },
  {
    {
      "arm7m",
      NULL,
      {
        ISA_ARMv3m,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3m
  },
  {
    {
      "arm7dm",
      NULL,
      {
        ISA_ARMv3m,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3m
  },
  {
    {
      "arm7dmi",
      NULL,
      {
        ISA_ARMv3m,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv3m
  },
  {
    {
      "arm8",
      NULL,
      {
        ISA_ARMv4,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4
  },
  {
    {
      "arm810",
      NULL,
      {
        ISA_ARMv4,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4
  },
  {
    {
      "strongarm",
      NULL,
      {
        ISA_ARMv4,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4
  },
  {
    {
      "strongarm110",
      NULL,
      {
        ISA_ARMv4,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4
  },
  {
    {
      "strongarm1100",
      NULL,
      {
        ISA_ARMv4,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4
  },
  {
    {
      "strongarm1110",
      NULL,
      {
        ISA_ARMv4,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4
  },
  {
    {
      "fa526",
      NULL,
      {
        ISA_ARMv4,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4
  },
  {
    {
      "fa626",
      NULL,
      {
        ISA_ARMv4,isa_bit_mode26,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4
  },
  {
    {
      "arm7tdmi",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm7tdmi-s",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm710t",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm720t",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm740t",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm9",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm9tdmi",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm920",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm920t",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm922t",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm940t",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "ep9312",
      NULL,
      {
        ISA_ARMv4t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv4t
  },
  {
    {
      "arm10tdmi",
      NULL,
      {
        ISA_ARMv5t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5t
  },
  {
    {
      "arm1020t",
      NULL,
      {
        ISA_ARMv5t,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5t
  },
  {
    {
      "arm9e",
      cpu_opttab_arm9e,
      {
        ISA_ARMv5te,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "arm946e-s",
      cpu_opttab_arm946es,
      {
        ISA_ARMv5te,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "arm966e-s",
      cpu_opttab_arm966es,
      {
        ISA_ARMv5te,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "arm968e-s",
      cpu_opttab_arm968es,
      {
        ISA_ARMv5te,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "arm10e",
      cpu_opttab_arm10e,
      {
        ISA_ARMv5te,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "arm1020e",
      cpu_opttab_arm1020e,
      {
        ISA_ARMv5te,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "arm1022e",
      cpu_opttab_arm1022e,
      {
        ISA_ARMv5te,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "xscale",
      NULL,
      {
        ISA_ARMv5te,
        isa_bit_xscale,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "iwmmxt",
      NULL,
      {
        ISA_ARMv5te,isa_bit_xscale,isa_bit_iwmmxt,
        isa_nobit
      }
    },
    TARGET_ARCH_iwmmxt
  },
  {
    {
      "iwmmxt2",
      NULL,
      {
        ISA_ARMv5te,isa_bit_xscale,isa_bit_iwmmxt,isa_bit_iwmmxt2,
        isa_nobit
      }
    },
    TARGET_ARCH_iwmmxt2
  },
  {
    {
      "fa606te",
      NULL,
      {
        ISA_ARMv5te,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "fa626te",
      NULL,
      {
        ISA_ARMv5te,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "fmp626",
      NULL,
      {
        ISA_ARMv5te,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "fa726te",
      NULL,
      {
        ISA_ARMv5te,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5te
  },
  {
    {
      "arm926ej-s",
      cpu_opttab_arm926ejs,
      {
        ISA_ARMv5tej,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5tej
  },
  {
    {
      "arm1026ej-s",
      cpu_opttab_arm1026ejs,
      {
        ISA_ARMv5tej,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv5tej
  },
  {
    {
      "arm1136j-s",
      NULL,
      {
        ISA_ARMv6j,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6j
  },
  {
    {
      "arm1136jf-s",
      NULL,
      {
        ISA_ARMv6j,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6j
  },
  {
    {
      "arm1176jz-s",
      NULL,
      {
        ISA_ARMv6kz,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6kz
  },
  {
    {
      "arm1176jzf-s",
      NULL,
      {
        ISA_ARMv6kz,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6kz
  },
  {
    {
      "mpcorenovfp",
      NULL,
      {
        ISA_ARMv6k,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6k
  },
  {
    {
      "mpcore",
      NULL,
      {
        ISA_ARMv6k,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6k
  },
  {
    {
      "arm1156t2-s",
      NULL,
      {
        ISA_ARMv6t2,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6t2
  },
  {
    {
      "arm1156t2f-s",
      NULL,
      {
        ISA_ARMv6t2,
        ISA_VFPv2,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6t2
  },
  {
    {
      "cortex-m1",
      NULL,
      {
        ISA_ARMv6m,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6_m
  },
  {
    {
      "cortex-m0",
      NULL,
      {
        ISA_ARMv6m,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6_m
  },
  {
    {
      "cortex-m0plus",
      NULL,
      {
        ISA_ARMv6m,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6_m
  },
  {
    {
      "cortex-m1.small-multiply",
      NULL,
      {
        ISA_ARMv6m,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6_m
  },
  {
    {
      "cortex-m0.small-multiply",
      NULL,
      {
        ISA_ARMv6m,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6_m
  },
  {
    {
      "cortex-m0plus.small-multiply",
      NULL,
      {
        ISA_ARMv6m,
        isa_nobit
      }
    },
    TARGET_ARCH_armv6_m
  },
  {
    {
      "generic-armv7-a",
      cpu_opttab_genericv7a,
      {
        ISA_ARMv7a,
        ISA_VFPv3,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_a
  },
  {
    {
      "cortex-a5",
      cpu_opttab_cortexa5,
      {
        ISA_ARMv7a,
        ISA_VFPv3,ISA_NEON,isa_bit_fp16conv,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_a
  },
  {
    {
      "cortex-a7",
      cpu_opttab_cortexa7,
      {
        ISA_ARMv7ve,
        ISA_VFPv4,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7ve
  },
  {
    {
      "cortex-a8",
      cpu_opttab_cortexa8,
      {
        ISA_ARMv7a,
        ISA_VFPv3,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_a
  },
  {
    {
      "cortex-a9",
      cpu_opttab_cortexa9,
      {
        ISA_ARMv7a,
        ISA_VFPv3,ISA_NEON,isa_bit_fp16conv,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_a
  },
  {
    {
      "cortex-a12",
      cpu_opttab_cortexa12,
      {
        ISA_ARMv7ve,
        ISA_VFPv4,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7ve
  },
  {
    {
      "cortex-a15",
      cpu_opttab_cortexa15,
      {
        ISA_ARMv7ve,
        ISA_VFPv4,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7ve
  },
  {
    {
      "cortex-a17",
      cpu_opttab_cortexa17,
      {
        ISA_ARMv7ve,
        ISA_VFPv4,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7ve
  },
  {
    {
      "cortex-r4",
      NULL,
      {
        ISA_ARMv7r,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_r
  },
  {
    {
      "cortex-r4f",
      NULL,
      {
        ISA_ARMv7r,
        ISA_VFPv3,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_r
  },
  {
    {
      "cortex-r5",
      cpu_opttab_cortexr5,
      {
        ISA_ARMv7r,
        isa_bit_adiv,
        ISA_VFPv3,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_r
  },
  {
    {
      "cortex-r7",
      cpu_opttab_cortexr7,
      {
        ISA_ARMv7r,
        isa_bit_adiv,
        ISA_VFPv3,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_r
  },
  {
    {
      "cortex-r8",
      cpu_opttab_cortexr8,
      {
        ISA_ARMv7r,
        isa_bit_adiv,
        ISA_VFPv3,ISA_FP_DBL,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_r
  },
  {
    {
      "cortex-m7",
      cpu_opttab_cortexm7,
      {
        ISA_ARMv7em,
        ISA_FPv5,ISA_FP_DBL,
        isa_quirk_no_volatile_ce,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7e_m
  },
  {
    {
      "cortex-m4",
      cpu_opttab_cortexm4,
      {
        ISA_ARMv7em,
        ISA_VFPv4,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7e_m
  },
  {
    {
      "cortex-m3",
      NULL,
      {
        ISA_ARMv7m,
        isa_quirk_cm3_ldrd,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_m
  },
  {
    {
      "marvell-pj4",
      NULL,
      {
        ISA_ARMv7a,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7_a
  },
  {
    {
      "cortex-a15.cortex-a7",
      cpu_opttab_cortexa15cortexa7,
      {
        ISA_ARMv7ve,
        ISA_VFPv4,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7ve
  },
  {
    {
      "cortex-a17.cortex-a7",
      cpu_opttab_cortexa17cortexa7,
      {
        ISA_ARMv7ve,
        ISA_VFPv4,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv7ve
  },
  {
    {
      "cortex-a32",
      cpu_opttab_cortexa32,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-a35",
      cpu_opttab_cortexa35,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-a53",
      cpu_opttab_cortexa53,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-a57",
      cpu_opttab_cortexa57,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-a72",
      cpu_opttab_cortexa72,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-a73",
      cpu_opttab_cortexa73,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "exynos-m1",
      cpu_opttab_exynosm1,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "xgene1",
      cpu_opttab_xgene1,
      {
        ISA_ARMv8a,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-a57.cortex-a53",
      cpu_opttab_cortexa57cortexa53,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-a72.cortex-a53",
      cpu_opttab_cortexa72cortexa53,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-a73.cortex-a35",
      cpu_opttab_cortexa73cortexa35,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-a73.cortex-a53",
      cpu_opttab_cortexa73cortexa53,
      {
        ISA_ARMv8a,
        isa_bit_crc32,
        ISA_FP_ARMv8,ISA_NEON,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_a
  },
  {
    {
      "cortex-m23",
      NULL,
      {
        ISA_ARMv8m_base,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_m_base
  },
  {
    {
      "cortex-m33",
      cpu_opttab_cortexm33,
      {
        ISA_ARMv8m_main,
        isa_bit_ARMv7em,
        ISA_FPv5,
        isa_nobit
      }
    },
    TARGET_ARCH_armv8_m_main
  },
  {{NULL, NULL, {isa_nobit}}, TARGET_ARCH_arm_none}
};
static const struct cpu_arch_extension arch_opttab_armv5e[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv5te[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv5tej[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv6[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv6j[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv6k[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv6z[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv6kz[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv6zk[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv6t2[] = {
  {
    "fp", false, false,
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv2", false, true, 
    { ISA_VFPv2,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv7[] = {
  {
    "fp", false, false,
    { ISA_VFPv3,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv3-d16", false, true, 
    { ISA_VFPv3,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv7_a[] = {
  {
    "fp", false, false,
    { ISA_VFPv3,ISA_FP_DBL, isa_nobit }
  },
  {
    "vfpv3", false, false,
    { ISA_VFPv3,ISA_FP_D32, isa_nobit }
  },
  {
    "vfpv3-d16-fp16", false, false,
    { ISA_VFPv3,ISA_FP_DBL,isa_bit_fp16conv, isa_nobit }
  },
  {
    "vfpv3-fp16", false, false,
    { ISA_VFPv3,ISA_FP_DBL,ISA_FP_D32,isa_bit_fp16conv, isa_nobit }
  },
  {
    "vfpv4-d16", false, false,
    { ISA_VFPv4,ISA_FP_DBL, isa_nobit }
  },
  {
    "vfpv4", false, false,
    { ISA_VFPv4,ISA_FP_D32, isa_nobit }
  },
  {
    "simd", false, false,
    { ISA_VFPv3,ISA_NEON, isa_nobit }
  },
  {
    "neon-fp16", false, false,
    { ISA_VFPv3,ISA_NEON,isa_bit_fp16conv, isa_nobit }
  },
  {
    "neon-vfpv4", false, false,
    { ISA_VFPv4,ISA_NEON, isa_nobit }
  },
  {
    "nosimd", true, false,
    { ISA_ALL_SIMD, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv3-d16", false, true, 
    { ISA_VFPv3,ISA_FP_DBL, isa_nobit }
  },
  {
    "neon", false, true, 
    { ISA_VFPv3,ISA_NEON, isa_nobit }
  },
  {
    "neon-vfpv3", false, true, 
    { ISA_VFPv3,ISA_NEON, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv7ve[] = {
  {
    "vfpv3-d16", false, false,
    { ISA_VFPv3,ISA_FP_DBL, isa_nobit }
  },
  {
    "vfpv3", false, false,
    { ISA_VFPv3,ISA_FP_D32, isa_nobit }
  },
  {
    "vfpv3-d16-fp16", false, false,
    { ISA_VFPv3,ISA_FP_DBL,isa_bit_fp16conv, isa_nobit }
  },
  {
    "vfpv3-fp16", false, false,
    { ISA_VFPv3,ISA_FP_DBL,ISA_FP_D32,isa_bit_fp16conv, isa_nobit }
  },
  {
    "fp", false, false,
    { ISA_VFPv4,ISA_FP_DBL, isa_nobit }
  },
  {
    "vfpv4", false, false,
    { ISA_VFPv4,ISA_FP_D32, isa_nobit }
  },
  {
    "neon", false, false,
    { ISA_VFPv3,ISA_NEON, isa_nobit }
  },
  {
    "neon-fp16", false, false,
    { ISA_VFPv3,ISA_NEON,isa_bit_fp16conv, isa_nobit }
  },
  {
    "simd", false, false,
    { ISA_VFPv4,ISA_NEON, isa_nobit }
  },
  {
    "nosimd", true, false,
    { ISA_ALL_SIMD, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv4-d16", false, true, 
    { ISA_VFPv4,ISA_FP_DBL, isa_nobit }
  },
  {
    "neon-vfpv3", false, true, 
    { ISA_VFPv3,ISA_NEON, isa_nobit }
  },
  {
    "neon-vfpv4", false, true, 
    { ISA_VFPv4,ISA_NEON, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv7_r[] = {
  {
    "fp.sp", false, false,
    { ISA_VFPv3, isa_nobit }
  },
  {
    "fp", false, false,
    { ISA_VFPv3,ISA_FP_DBL, isa_nobit }
  },
  {
    "idiv", false, false,
    { isa_bit_adiv, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "noidiv", true, false,
    { isa_bit_adiv, isa_nobit }
  },
  {
    "vfpv3xd", false, true, 
    { ISA_VFPv3, isa_nobit }
  },
  {
    "vfpv3-d16", false, true, 
    { ISA_VFPv3,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv7e_m[] = {
  {
    "fp", false, false,
    { ISA_VFPv4, isa_nobit }
  },
  {
    "fpv5", false, false,
    { ISA_FPv5, isa_nobit }
  },
  {
    "fp.dp", false, false,
    { ISA_FPv5,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "vfpv4-sp-d16", false, true, 
    { ISA_VFPv4, isa_nobit }
  },
  {
    "fpv5-d16", false, true, 
    { ISA_FPv5,ISA_FP_DBL, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv8_a[] = {
  {
    "crc", false, false,
    { isa_bit_crc32, isa_nobit }
  },
  {
    "simd", false, false,
    { ISA_FP_ARMv8,ISA_NEON, isa_nobit }
  },
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  {
    "nocrypto", true, false,
    { ISA_ALL_CRYPTO, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv8_1_a[] = {
  {
    "simd", false, false,
    { ISA_FP_ARMv8,ISA_NEON, isa_nobit }
  },
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  {
    "nocrypto", true, false,
    { ISA_ALL_CRYPTO, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv8_2_a[] = {
  {
    "simd", false, false,
    { ISA_FP_ARMv8,ISA_NEON, isa_nobit }
  },
  {
    "fp16", false, false,
    { isa_bit_fp16,ISA_FP_ARMv8,ISA_NEON, isa_nobit }
  },
  {
    "crypto", false, false,
    { ISA_FP_ARMv8,ISA_CRYPTO, isa_nobit }
  },
  {
    "nocrypto", true, false,
    { ISA_ALL_CRYPTO, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

static const struct cpu_arch_extension arch_opttab_armv8_m_main[] = {
  {
    "dsp", false, false,
    { isa_bit_ARMv7em, isa_nobit }
  },
  {
    "fp", false, false,
    { ISA_FPv5, isa_nobit }
  },
  {
    "fp.dp", false, false,
    { ISA_FPv5,ISA_FP_DBL, isa_nobit }
  },
  {
    "nofp", true, false,
    { ISA_ALL_FP, isa_nobit }
  },
  {
    "nodsp", true, false,
    { isa_bit_ARMv7em, isa_nobit }
  },
  { NULL, false, false, {isa_nobit}}
};

const arch_option all_architectures[] =
{
  {
    "armv2",
    NULL,
    {
      ISA_ARMv2,isa_bit_mode26,
      isa_nobit
    },
    "2", BASE_ARCH_2,
    0,
    TARGET_CPU_arm2,
  },
  {
    "armv2a",
    NULL,
    {
      ISA_ARMv2,isa_bit_mode26,
      isa_nobit
    },
    "2", BASE_ARCH_2,
    0,
    TARGET_CPU_arm2,
  },
  {
    "armv3",
    NULL,
    {
      ISA_ARMv3,isa_bit_mode26,
      isa_nobit
    },
    "3", BASE_ARCH_3,
    0,
    TARGET_CPU_arm6,
  },
  {
    "armv3m",
    NULL,
    {
      ISA_ARMv3m,isa_bit_mode26,
      isa_nobit
    },
    "3M", BASE_ARCH_3M,
    0,
    TARGET_CPU_arm7m,
  },
  {
    "armv4",
    NULL,
    {
      ISA_ARMv4,isa_bit_mode26,
      isa_nobit
    },
    "4", BASE_ARCH_4,
    0,
    TARGET_CPU_arm7tdmi,
  },
  {
    "armv4t",
    NULL,
    {
      ISA_ARMv4t,
      isa_nobit
    },
    "4T", BASE_ARCH_4T,
    0,
    TARGET_CPU_arm7tdmi,
  },
  {
    "armv5",
    NULL,
    {
      ISA_ARMv5,
      isa_nobit
    },
    "5", BASE_ARCH_5,
    0,
    TARGET_CPU_arm10tdmi,
  },
  {
    "armv5t",
    NULL,
    {
      ISA_ARMv5t,
      isa_nobit
    },
    "5T", BASE_ARCH_5T,
    0,
    TARGET_CPU_arm10tdmi,
  },
  {
    "armv5e",
    arch_opttab_armv5e,
    {
      ISA_ARMv5e,
      isa_nobit
    },
    "5E", BASE_ARCH_5E,
    0,
    TARGET_CPU_arm1026ejs,
  },
  {
    "armv5te",
    arch_opttab_armv5te,
    {
      ISA_ARMv5te,
      isa_nobit
    },
    "5TE", BASE_ARCH_5TE,
    0,
    TARGET_CPU_arm1026ejs,
  },
  {
    "armv5tej",
    arch_opttab_armv5tej,
    {
      ISA_ARMv5tej,
      isa_nobit
    },
    "5TEJ", BASE_ARCH_5TEJ,
    0,
    TARGET_CPU_arm1026ejs,
  },
  {
    "armv6",
    arch_opttab_armv6,
    {
      ISA_ARMv6,
      isa_nobit
    },
    "6", BASE_ARCH_6,
    0,
    TARGET_CPU_arm1136js,
  },
  {
    "armv6j",
    arch_opttab_armv6j,
    {
      ISA_ARMv6j,
      isa_nobit
    },
    "6J", BASE_ARCH_6J,
    0,
    TARGET_CPU_arm1136js,
  },
  {
    "armv6k",
    arch_opttab_armv6k,
    {
      ISA_ARMv6k,
      isa_nobit
    },
    "6K", BASE_ARCH_6K,
    0,
    TARGET_CPU_mpcore,
  },
  {
    "armv6z",
    arch_opttab_armv6z,
    {
      ISA_ARMv6z,
      isa_nobit
    },
    "6Z", BASE_ARCH_6Z,
    0,
    TARGET_CPU_arm1176jzs,
  },
  {
    "armv6kz",
    arch_opttab_armv6kz,
    {
      ISA_ARMv6kz,
      isa_nobit
    },
    "6KZ", BASE_ARCH_6KZ,
    0,
    TARGET_CPU_arm1176jzs,
  },
  {
    "armv6zk",
    arch_opttab_armv6zk,
    {
      ISA_ARMv6kz,
      isa_nobit
    },
    "6KZ", BASE_ARCH_6KZ,
    0,
    TARGET_CPU_arm1176jzs,
  },
  {
    "armv6t2",
    arch_opttab_armv6t2,
    {
      ISA_ARMv6t2,
      isa_nobit
    },
    "6T2", BASE_ARCH_6T2,
    0,
    TARGET_CPU_arm1156t2s,
  },
  {
    "armv6-m",
    NULL,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    "6M", BASE_ARCH_6M,
    'M',
    TARGET_CPU_cortexm1,
  },
  {
    "armv6s-m",
    NULL,
    {
      ISA_ARMv6m,
      isa_nobit
    },
    "6M", BASE_ARCH_6M,
    'M',
    TARGET_CPU_cortexm1,
  },
  {
    "armv7",
    arch_opttab_armv7,
    {
      ISA_ARMv7,
      isa_nobit
    },
    "7", BASE_ARCH_7,
    0,
    TARGET_CPU_cortexa8,
  },
  {
    "armv7-a",
    arch_opttab_armv7_a,
    {
      ISA_ARMv7a,
      isa_nobit
    },
    "7A", BASE_ARCH_7A,
    'A',
    TARGET_CPU_cortexa8,
  },
  {
    "armv7ve",
    arch_opttab_armv7ve,
    {
      ISA_ARMv7ve,
      isa_nobit
    },
    "7A", BASE_ARCH_7A,
    'A',
    TARGET_CPU_cortexa8,
  },
  {
    "armv7-r",
    arch_opttab_armv7_r,
    {
      ISA_ARMv7r,
      isa_nobit
    },
    "7R", BASE_ARCH_7R,
    'R',
    TARGET_CPU_cortexr4,
  },
  {
    "armv7-m",
    NULL,
    {
      ISA_ARMv7m,
      isa_nobit
    },
    "7M", BASE_ARCH_7M,
    'M',
    TARGET_CPU_cortexm3,
  },
  {
    "armv7e-m",
    arch_opttab_armv7e_m,
    {
      ISA_ARMv7em,
      isa_nobit
    },
    "7EM", BASE_ARCH_7EM,
    'M',
    TARGET_CPU_cortexm4,
  },
  {
    "armv8-a",
    arch_opttab_armv8_a,
    {
      ISA_ARMv8a,
      isa_nobit
    },
    "8A", BASE_ARCH_8A,
    'A',
    TARGET_CPU_cortexa53,
  },
  {
    "armv8.1-a",
    arch_opttab_armv8_1_a,
    {
      ISA_ARMv8_1a,
      isa_nobit
    },
    "8A", BASE_ARCH_8A,
    'A',
    TARGET_CPU_cortexa53,
  },
  {
    "armv8.2-a",
    arch_opttab_armv8_2_a,
    {
      ISA_ARMv8_2a,
      isa_nobit
    },
    "8A", BASE_ARCH_8A,
    'A',
    TARGET_CPU_cortexa53,
  },
  {
    "armv8-m.base",
    NULL,
    {
      ISA_ARMv8m_base,
      isa_nobit
    },
    "8M_BASE", BASE_ARCH_8M_BASE,
    'M',
    TARGET_CPU_cortexm23,
  },
  {
    "armv8-m.main",
    arch_opttab_armv8_m_main,
    {
      ISA_ARMv8m_main,
      isa_nobit
    },
    "8M_MAIN", BASE_ARCH_8M_MAIN,
    'M',
    TARGET_CPU_cortexm7,
  },
  {
    "iwmmxt",
    NULL,
    {
      ISA_ARMv5te,isa_bit_xscale,isa_bit_iwmmxt,
      isa_nobit
    },
    "5TE", BASE_ARCH_5TE,
    0,
    TARGET_CPU_iwmmxt,
  },
  {
    "iwmmxt2",
    NULL,
    {
      ISA_ARMv5te,isa_bit_xscale,isa_bit_iwmmxt,isa_bit_iwmmxt2,
      isa_nobit
    },
    "5TE", BASE_ARCH_5TE,
    0,
    TARGET_CPU_iwmmxt2,
  },
  {{NULL, NULL, {isa_nobit}},
   NULL, BASE_ARCH_0, 0, TARGET_CPU_arm_none}
};

const arm_fpu_desc all_fpus[] =
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
