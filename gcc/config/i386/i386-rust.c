/* Subroutines for the Rust front end on the x86 architecture.
   Copyright (C) 2020 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rust/rust-target.h"
#include "rust/rust-target-def.h"

/* Implement TARGET_RUST_CPU_INFO for x86 targets.  */

void
ix86_rust_target_cpu_info (void)
{
    if (TARGET_64BIT) {
        rust_add_target_info("target_arch", "x86_64");

        if (TARGET_X32) {
            // this means it uses 32-bit pointers with 64-bit, basically (ILP32)
            //rust_add_target_info("target_pointer_width", "32");
            // TODO: may also change x86_64-...-linux-gnu to x86_64-...-linux-gnux32

            // is this better than just putting in pointer width outside of if statement?

            /* TODO: compared to base linux, may also need to change max_atomic_width to 64, add "-mx32"
             * to pre-link args, make stack_probes true, make has_elf_tls false, make needs_plt true.
             * Also, still target_endian is "little", target_c_int_width is "32", maybe steal data layout
             * later from rustc spec, target_os is "linux", target_env is "gnu", target_vendor is "unknown"
             * There is no rustc support for non-gnu/linux targets with ILP32. */
        }
    } else {
        rust_add_target_info("target_arch", "x86");
    }

    // maybe more stuff I don't understand if evidenced by ix86_target_macros in i386-c.c

    // note: options that don't seem to have a target feature in rust are commented out

    // TODO: properly change at some point instead of macro def
#ifndef isa_flag
# define isa_flag ix86_isa_flags
# define isa_flag2 ix86_isa_flags2
# define fpmath ix86_fpmath
#else
# error "isa_flag and isa_flag2 already defined in i386-rust.c - weird things might happen"
#endif

    // options should be feature complete for rustc atm
  if (isa_flag2 & OPTION_MASK_ISA2_WBNOINVD)
    ; //def_or_undef (parse_in, "__WBNOINVD__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVX512VP2INTERSECT)
    ; //def_or_undef (parse_in, "__AVX512VP2INTERSECT__");
  if (isa_flag & OPTION_MASK_ISA_MMX)
    rust_add_target_info("target_feature", "mmx");
  if (isa_flag & OPTION_MASK_ISA_3DNOW)
    ; //def_or_undef (parse_in, "__3dNOW__");
  if (isa_flag & OPTION_MASK_ISA_3DNOW_A)
    ; //def_or_undef (parse_in, "__3dNOW_A__");
  if (isa_flag & OPTION_MASK_ISA_SSE)
    rust_add_target_info("target_feature", "sse");
  if (isa_flag & OPTION_MASK_ISA_SSE2)
    rust_add_target_info("target_feature", "sse2");
  if (isa_flag & OPTION_MASK_ISA_SSE3)
    rust_add_target_info("target_feature", "sse3");
  if (isa_flag & OPTION_MASK_ISA_SSSE3)
    rust_add_target_info("target_feature", "ssse3");
  if (isa_flag & OPTION_MASK_ISA_SSE4_1)
    rust_add_target_info("target_feature", "sse4.1");
  if (isa_flag & OPTION_MASK_ISA_SSE4_2)
    rust_add_target_info("target_feature", "sse4.2");
  if (isa_flag & OPTION_MASK_ISA_AES)
    rust_add_target_info("target_feature", "aes");
  if (isa_flag & OPTION_MASK_ISA_SHA)
    rust_add_target_info("target_feature", "sha");
  if (isa_flag & OPTION_MASK_ISA_PCLMUL)
    ; //def_or_undef (parse_in, "__PCLMUL__");
  if (isa_flag & OPTION_MASK_ISA_AVX)
    rust_add_target_info("target_feature", "avx");
  if (isa_flag & OPTION_MASK_ISA_AVX2)
    rust_add_target_info("target_feature", "avx2");
  if (isa_flag & OPTION_MASK_ISA_AVX512F)
    rust_add_target_info("target_feature", "avx512f");
  if (isa_flag & OPTION_MASK_ISA_AVX512ER)
    rust_add_target_info("target_feature", "avx512er");
  if (isa_flag & OPTION_MASK_ISA_AVX512CD)
    rust_add_target_info("target_feature", "avx512cd");
  if (isa_flag & OPTION_MASK_ISA_AVX512PF)
    rust_add_target_info("target_feature", "avx512pf");
  if (isa_flag & OPTION_MASK_ISA_AVX512DQ)
    rust_add_target_info("target_feature", "avx512dq");
  if (isa_flag & OPTION_MASK_ISA_AVX512BW)
    rust_add_target_info("target_feature", "avx512bw");
  if (isa_flag & OPTION_MASK_ISA_AVX512VL)
    rust_add_target_info("target_feature", "avx512vl");
  if (isa_flag & OPTION_MASK_ISA_AVX512VBMI)
    rust_add_target_info("target_feature", "avx512vbmi");
  if (isa_flag & OPTION_MASK_ISA_AVX512IFMA)
    rust_add_target_info("target_feature", "avx512ifma");
  if (isa_flag2 & OPTION_MASK_ISA2_AVX5124VNNIW)
    ; //def_or_undef (parse_in, "__AVX5124VNNIW__");
  if (isa_flag & OPTION_MASK_ISA_AVX512VBMI2)
    ; //def_or_undef (parse_in, "__AVX512VBMI2__");
  if (isa_flag & OPTION_MASK_ISA_AVX512VNNI)
    ; //def_or_undef (parse_in, "__AVX512VNNI__");
  if (isa_flag2 & OPTION_MASK_ISA2_PCONFIG)
    ; //def_or_undef (parse_in, "__PCONFIG__");
  if (isa_flag2 & OPTION_MASK_ISA2_SGX)
    ; //def_or_undef (parse_in, "__SGX__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVX5124FMAPS)
    ; //def_or_undef (parse_in, "__AVX5124FMAPS__");
  if (isa_flag & OPTION_MASK_ISA_AVX512BITALG)
    ; //def_or_undef (parse_in, "__AVX512BITALG__");
  if (isa_flag & OPTION_MASK_ISA_AVX512VPOPCNTDQ)
    rust_add_target_info("target_feature", "avx512vpopcntdq");
  if (isa_flag & OPTION_MASK_ISA_FMA)
    rust_add_target_info("target_feature", "fma");
  if (isa_flag & OPTION_MASK_ISA_RTM)
    rust_add_target_info("target_feature", "rtm");
  if (isa_flag & OPTION_MASK_ISA_SSE4A)
    rust_add_target_info("target_feature", "sse4a");
  if (isa_flag & OPTION_MASK_ISA_FMA4)
    ; //def_or_undef (parse_in, "__FMA4__");
  if (isa_flag & OPTION_MASK_ISA_XOP)
    ; //def_or_undef (parse_in, "__XOP__");
  if (isa_flag & OPTION_MASK_ISA_LWP)
    ; //def_or_undef (parse_in, "__LWP__");
  if (isa_flag & OPTION_MASK_ISA_ABM)
    ; //def_or_undef (parse_in, "__ABM__");
  if (isa_flag & OPTION_MASK_ISA_BMI)
    rust_add_target_info("target_feature", "bmi1");
  if (isa_flag & OPTION_MASK_ISA_BMI2)
    rust_add_target_info("target_feature", "bmi2");
  if (isa_flag & OPTION_MASK_ISA_LZCNT)
    rust_add_target_info("target_feature", "lzcnt");
  if (isa_flag & OPTION_MASK_ISA_TBM)
    rust_add_target_info("target_feature", "tbm");
  if (isa_flag & OPTION_MASK_ISA_POPCNT)
    rust_add_target_info("target_feature", "popcnt");
  if (isa_flag & OPTION_MASK_ISA_FSGSBASE)
    ; //def_or_undef (parse_in, "__FSGSBASE__");
  if (isa_flag & OPTION_MASK_ISA_RDRND)
    rust_add_target_info("target_feature", "rdrand");
  if (isa_flag & OPTION_MASK_ISA_F16C)
    rust_add_target_info("target_feature", "f16c");
  if (isa_flag & OPTION_MASK_ISA_RDSEED)
    rust_add_target_info("target_feature", "rdseed");
  if (isa_flag & OPTION_MASK_ISA_PRFCHW)
    ; //def_or_undef (parse_in, "__PRFCHW__");
  if (isa_flag & OPTION_MASK_ISA_ADX)
    rust_add_target_info("target_feature", "adx");
  if (isa_flag & OPTION_MASK_ISA_FXSR)
    rust_add_target_info("target_feature", "fxsr");
  if (isa_flag & OPTION_MASK_ISA_XSAVE)
    rust_add_target_info("target_feature", "xsave");
  if (isa_flag & OPTION_MASK_ISA_XSAVEOPT)
    rust_add_target_info("target_feature", "xsaveopt");
  if (isa_flag & OPTION_MASK_ISA_PREFETCHWT1)
    ; //def_or_undef (parse_in, "__PREFETCHWT1__");
  if ((fpmath & FPMATH_SSE) && (isa_flag & OPTION_MASK_ISA_SSE))
    ; //def_or_undef (parse_in, "__SSE_MATH__");
  if ((fpmath & FPMATH_SSE) && (isa_flag & OPTION_MASK_ISA_SSE2))
    ; //def_or_undef (parse_in, "__SSE2_MATH__");
  if (isa_flag & OPTION_MASK_ISA_CLFLUSHOPT)
    ; //def_or_undef (parse_in, "__CLFLUSHOPT__");
  if (isa_flag2 & OPTION_MASK_ISA2_CLZERO)
    ; //def_or_undef (parse_in, "__CLZERO__");
  if (isa_flag & OPTION_MASK_ISA_XSAVEC)
    rust_add_target_info("target_feature", "xsavec");
  if (isa_flag & OPTION_MASK_ISA_XSAVES)
    rust_add_target_info("target_feature", "xsaves");
  if (isa_flag & OPTION_MASK_ISA_CLWB)
    ; //def_or_undef (parse_in, "__CLWB__");
  if (isa_flag2 & OPTION_MASK_ISA2_MWAITX)
    ; //def_or_undef (parse_in, "__MWAITX__");
  if (isa_flag & OPTION_MASK_ISA_PKU)
    ; //def_or_undef (parse_in, "__PKU__");
  if (isa_flag2 & OPTION_MASK_ISA2_RDPID)
    ; //def_or_undef (parse_in, "__RDPID__");
  if (isa_flag & OPTION_MASK_ISA_GFNI)
    ; //def_or_undef (parse_in, "__GFNI__");
  if ((isa_flag & OPTION_MASK_ISA_SHSTK))
    ; //def_or_undef (parse_in, "__SHSTK__");
  if (isa_flag2 & OPTION_MASK_ISA2_VAES)
    ; //def_or_undef (parse_in, "__VAES__");
  if (isa_flag & OPTION_MASK_ISA_VPCLMULQDQ)
    rust_add_target_info("target_feature", "pclmulqdq");
  if (isa_flag & OPTION_MASK_ISA_MOVDIRI)
    ; //def_or_undef (parse_in, "__MOVDIRI__");
  if (isa_flag2 & OPTION_MASK_ISA2_MOVDIR64B)
    ; //def_or_undef (parse_in, "__MOVDIR64B__");
  if (isa_flag2 & OPTION_MASK_ISA2_WAITPKG)
    ; //def_or_undef (parse_in, "__WAITPKG__");
  if (isa_flag2 & OPTION_MASK_ISA2_CLDEMOTE)
    ; //def_or_undef (parse_in, "__CLDEMOTE__");
  if (isa_flag2 & OPTION_MASK_ISA2_PTWRITE)
    ; //def_or_undef (parse_in, "__PTWRITE__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVX512BF16)
    ; //def_or_undef (parse_in, "__AVX512BF16__");
  if (TARGET_MMX_WITH_SSE)
    ; //def_or_undef (parse_in, "__MMX_WITH_SSE__");
  if (isa_flag2 & OPTION_MASK_ISA2_ENQCMD)
    ; //def_or_undef (parse_in, "__ENQCMD__");
  if (TARGET_IAMCU)
    {
      //def_or_undef (parse_in, "__iamcu");
      //def_or_undef (parse_in, "__iamcu__");
    }
  if (TARGET_CMPXCHG16B)
    rust_add_target_info("target_feature", "cmpxchg16b");
  if (TARGET_MOVBE)
    rust_add_target_info("target_feature", "movbe");

#undef isa_flag
#undef isa_flag2
#undef fpmath
}

