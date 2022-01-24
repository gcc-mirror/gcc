/* Subroutines for the Rust front end on the x86 architecture.
   Copyright (C) 2020-2022 Free Software Foundation, Inc.

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
#include "tm_p.h"
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

  // features officially "stabilised" in rustc
  if (TARGET_MMX)
    rust_add_target_info("target_feature", "mmx");
  if (TARGET_SSE)
    rust_add_target_info("target_feature", "sse");
  if (TARGET_SSE2)
    rust_add_target_info("target_feature", "sse2");
  if (TARGET_SSE3)
    rust_add_target_info("target_feature", "sse3");
  if (TARGET_SSSE3)
    rust_add_target_info("target_feature", "ssse3");
  if (TARGET_SSE4_1)
    rust_add_target_info("target_feature", "sse4.1");
  if (TARGET_SSE4_2)
    rust_add_target_info("target_feature", "sse4.2");
  if (TARGET_AES)
    rust_add_target_info("target_feature", "aes");
  if (TARGET_SHA)
    rust_add_target_info("target_feature", "sha");
  if (TARGET_AVX)
    rust_add_target_info("target_feature", "avx");
  if (TARGET_AVX2)
    rust_add_target_info("target_feature", "avx2");
  if (TARGET_AVX512F)
    rust_add_target_info("target_feature", "avx512f");
  if (TARGET_AVX512ER)
    rust_add_target_info("target_feature", "avx512er");
  if (TARGET_AVX512CD)
    rust_add_target_info("target_feature", "avx512cd");
  if (TARGET_AVX512PF)
    rust_add_target_info("target_feature", "avx512pf");
  if (TARGET_AVX512DQ)
    rust_add_target_info("target_feature", "avx512dq");
  if (TARGET_AVX512BW)
    rust_add_target_info("target_feature", "avx512bw");
  if (TARGET_AVX512VL)
    rust_add_target_info("target_feature", "avx512vl");
  if (TARGET_AVX512VBMI)
    rust_add_target_info("target_feature", "avx512vbmi");
  if (TARGET_AVX512IFMA)
    rust_add_target_info("target_feature", "avx512ifma");
  if (TARGET_AVX512VPOPCNTDQ)
    rust_add_target_info("target_feature", "avx512vpopcntdq");
  if (TARGET_FMA)
    rust_add_target_info("target_feature", "fma");
  if (TARGET_RTM)
    rust_add_target_info("target_feature", "rtm");
  if (TARGET_SSE4A)
    rust_add_target_info("target_feature", "sse4a");
  if (TARGET_BMI) {
    rust_add_target_info("target_feature", "bmi1");
    rust_add_target_info("target_feature", "bmi");
  }
  if (TARGET_BMI2)
    rust_add_target_info("target_feature", "bmi2");
  if (TARGET_LZCNT)
    rust_add_target_info("target_feature", "lzcnt");
  if (TARGET_TBM)
    rust_add_target_info("target_feature", "tbm");
  if (TARGET_POPCNT)
    rust_add_target_info("target_feature", "popcnt");
  if (TARGET_RDRND) {
    rust_add_target_info("target_feature", "rdrand");
    rust_add_target_info("target_feature", "rdrnd");
  }
  if (TARGET_F16C)
    rust_add_target_info("target_feature", "f16c");
  if (TARGET_RDSEED)
    rust_add_target_info("target_feature", "rdseed");
  if (TARGET_ADX)
    rust_add_target_info("target_feature", "adx");
  if (TARGET_FXSR)
    rust_add_target_info("target_feature", "fxsr");
  if (TARGET_XSAVE)
    rust_add_target_info("target_feature", "xsave");
  if (TARGET_XSAVEOPT)
    rust_add_target_info("target_feature", "xsaveopt");
  if (TARGET_XSAVEC)
    rust_add_target_info("target_feature", "xsavec");
  if (TARGET_XSAVES)
    rust_add_target_info("target_feature", "xsaves");
  if (TARGET_VPCLMULQDQ) {
    rust_add_target_info("target_feature", "pclmulqdq");
    rust_add_target_info("target_feature", "vpclmulqdq");
  }
  if (TARGET_CMPXCHG16B)
    rust_add_target_info("target_feature", "cmpxchg16b");
  if (TARGET_MOVBE)
    rust_add_target_info("target_feature", "movbe");

  // features derived from llvm not yet in rustc:
  if (TARGET_64BIT)
    rust_add_target_info("target_feature", "64bit-mode");
  else if (TARGET_CODE16)
    rust_add_target_info("target_feature", "16bit-mode");
  else
    rust_add_target_info("target_feature", "32bit-mode");
  
  // TODO: assuming that the TARGET_80387 (which seems to mean "hard float") is also required for x87
  if (TARGET_80387 && (ix86_fpmath & FPMATH_387) != 0)
    rust_add_target_info("target_feature", "x87");

  // nopl: hard-coded (as gcc doesn't technically have feature) to return true for cpu arches with it
  // maybe refactor into switch if multiple options
  bool hasNOPL = ix86_arch == PROCESSOR_PENTIUMPRO || ix86_arch == PROCESSOR_PENTIUM4 
    || ix86_arch == PROCESSOR_NOCONA || ix86_arch == PROCESSOR_CORE2 || ix86_arch == PROCESSOR_NEHALEM 
    || ix86_arch == PROCESSOR_BONNELL || ix86_arch == PROCESSOR_SILVERMONT 
    || ix86_arch == PROCESSOR_GOLDMONT || ix86_arch == PROCESSOR_GOLDMONT_PLUS 
    || ix86_arch == PROCESSOR_TREMONT || ix86_arch == PROCESSOR_SANDYBRIDGE 
    || ix86_arch == PROCESSOR_HASWELL || ix86_arch == PROCESSOR_SKYLAKE 
    || ix86_arch == PROCESSOR_SKYLAKE_AVX512 || ix86_arch == PROCESSOR_CANNONLAKE 
    || ix86_arch == PROCESSOR_CASCADELAKE  || ix86_arch == PROCESSOR_COOPERLAKE 
    || ix86_arch == PROCESSOR_ICELAKE_CLIENT || ix86_arch == PROCESSOR_ICELAKE_SERVER 
    || ix86_arch == PROCESSOR_TIGERLAKE || ix86_arch == PROCESSOR_KNL || ix86_arch == PROCESSOR_KNM 
    || ix86_arch == PROCESSOR_AMDFAM10 || ix86_arch == PROCESSOR_BTVER1 || ix86_arch == PROCESSOR_BTVER2 
    || ix86_arch == PROCESSOR_BDVER1 || ix86_arch == PROCESSOR_BDVER2 || ix86_arch == PROCESSOR_BDVER3 
    || ix86_arch == PROCESSOR_BDVER4 || ix86_arch == PROCESSOR_ZNVER1 || ix86_arch == PROCESSOR_ZNVER2 
    || ix86_arch == PROCESSOR_ATHLON || ix86_arch == PROCESSOR_K8; 
  // this list should be exhaustive
  if (hasNOPL)
    rust_add_target_info("target_feature", "nopl");
  if (TARGET_CMOVE)
    rust_add_target_info("target_feature", "cmov");
  if (TARGET_CMPXCHG8B)
    rust_add_target_info("target_feature", "cx8");
  if (TARGET_3DNOW)
    rust_add_target_info("target_feature", "3dnow");
  if (TARGET_3DNOW_A)
    rust_add_target_info("target_feature", "3dnowa");
  if (TARGET_64BIT)
    rust_add_target_info("target_feature", "64bit");
  if (TARGET_CMPXCHG16B)
    rust_add_target_info("target_feature", "cx16");

  bool hasSlowSHLD = ix86_arch == PROCESSOR_AMDFAM10 || ix86_arch == PROCESSOR_BTVER1 
    || ix86_arch == PROCESSOR_BTVER2 || ix86_arch == PROCESSOR_BDVER1 || ix86_arch == PROCESSOR_BDVER2 
    || ix86_arch == PROCESSOR_BDVER3 || ix86_arch == PROCESSOR_BDVER4 || ix86_arch == PROCESSOR_ZNVER1 
    || ix86_arch == PROCESSOR_ZNVER2 || ix86_arch == PROCESSOR_ATHLON || ix86_arch == PROCESSOR_K8; 
  // TODO: this is not ideal as it marks the baseline x86-64 CPU as having it - only AMD ones do
  if (hasSlowSHLD)
    rust_add_target_info("target_feature", "slow-shld");
  if (ix86_arch == PROCESSOR_SILVERMONT)
    rust_add_target_info("target_feature", "slow-pmulld");
  if (ix86_arch == PROCESSOR_KNL || ix86_arch == PROCESSOR_KNM)
    rust_add_target_info("target_feature", "slow-pmaddwd");

  bool hasSlowUnaligned16 = ix86_arch == PROCESSOR_BONNELL || ix86_arch == PROCESSOR_GENERIC 
    || ix86_arch == PROCESSOR_I386 || ix86_arch == PROCESSOR_I486 || ix86_arch == PROCESSOR_PENTIUM 
    || ix86_arch == PROCESSOR_PENTIUMPRO || ix86_arch == PROCESSOR_PENTIUM4 
    || ix86_arch == PROCESSOR_NOCONA || ix86_arch == PROCESSOR_CORE2 || ix86_arch == PROCESSOR_K6 
    || ix86_arch == PROCESSOR_ATHLON || ix86_arch == PROCESSOR_K8 || ix86_arch == PROCESSOR_GEODE;
  if (hasSlowUnaligned16)
    rust_add_target_info("target_feature", "slow-unaligned-mem-16");
  if (ix86_arch == PROCESSOR_SANDYBRIDGE)
    rust_add_target_info("target_feature", "slow-unaligned-mem-32");
  if (TARGET_PREFETCHWT1)
    rust_add_target_info("target_feature", "prefetchwt1");
  if (TARGET_AVX512VBMI2)
    rust_add_target_info("target_feature", "avx512vbmi2");
  if (TARGET_PKU)
    rust_add_target_info("target_feature", "pku");
  if (TARGET_AVX512VNNI)
    rust_add_target_info("target_feature", "avx512vnni");
  if (TARGET_AVX512BF16)
    rust_add_target_info("target_feature", "avx512bf16");
  if (TARGET_AVX512BITALG)
    rust_add_target_info("target_feature", "avx512bitalg");
  if (TARGET_AVX512VP2INTERSECT)
    rust_add_target_info("target_feature", "avx512vp2intersect");
  if (TARGET_PCLMUL)
    rust_add_target_info("target_feature", "pclmul");
  if (TARGET_GFNI)
    rust_add_target_info("target_feature", "gfni");
  if (TARGET_FMA4)
    rust_add_target_info("target_feature", "fma4");
  if (TARGET_XOP)
    rust_add_target_info("target_feature", "xop");

  // this is only enabled by choice in llvm, never by default - TODO determine if gcc enables it
  // rust_add_target_info("target_feature", "sse-unaligned-mem");

  if (TARGET_VAES)
    rust_add_target_info("target_feature", "vaes");
  if (TARGET_LWP)
    rust_add_target_info("target_feature", "lwp");
  if (TARGET_FSGSBASE)
    rust_add_target_info("target_feature", "fsgsbase");
  if (TARGET_SHSTK)
    rust_add_target_info("target_feature", "shstk");
  if (TARGET_PRFCHW)
    rust_add_target_info("target_feature", "prfchw");
  if (TARGET_SAHF) // would this be better as TARGET_USE_SAHF?
    rust_add_target_info("target_feature", "sahf");
  if (TARGET_MWAITX)
    rust_add_target_info("target_feature", "mwaitx");
  if (TARGET_CLZERO)
    rust_add_target_info("target_feature", "clzero");
  if (TARGET_CLDEMOTE)
    rust_add_target_info("target_feature", "cldemote");
  if (TARGET_PTWRITE)
    rust_add_target_info("target_feature", "ptwrite");
  // TODO: add amx-tile, amx-int8, amx-bf16 features when gcc supports them 

  // TODO: can't find any gcc option relating to using LEA for adjusting stack pointer, so hardcoding
  if (ix86_arch == PROCESSOR_BONNELL)
    rust_add_target_info("target_feature", "lea-sp");

  // TODO: confirm that this is what it actually refers to
  if (TARGET_USE_8BIT_IDIV)
    rust_add_target_info("target_feature", "idivl-to-divb");

  /* TODO: can't find any gcc option corresponding to idivq-to-divl - does gcc perform this optimisation?
   * if so, add that feature (use 32-bit divide for positive values less than 2^32) */
  /* bool llvmHasSlowDivide64 = ix86_arch == PROCESSOR_SANDYBRIDGE || ix86_arch == PROCESSOR_HASWELL 
    || ix86_arch == PROCESSOR_SKYLAKE || ix86_arch == PROCESSOR_SKYLAKE_AVX512 
    || ix86_arch == PROCESSOR_CANNONLAKE || ix86_arch == PROCESSOR_ICELAKE_CLIENT 
    || ix86_arch == PROCESSOR_ICELAKE_SERVER || ix86_arch == PROCESSOR_CASCADELAKE 
    || ix86_arch == PROCESSOR_TIGERLAKE || ix86_arch == PROCESSOR_COOPERLAKE 
    || ix86_arch == PROCESSOR_BONNELL || ix86_arch == PROCESSOR_SILVERMONT || ix86_arch == PROCESSOR_KNL 
    || ix86_arch == PROCESSOR_KNM || ix86_arch == PROCESSOR_K8;*/

  if (TARGET_PAD_SHORT_FUNCTION)
    rust_add_target_info("target_feature", "pad-short-functions");

  // TODO: gcc seems to not record whether INVPCID exists, so basing it on llvm
  bool hasINVPCID = ix86_arch == PROCESSOR_HASWELL || ix86_arch == PROCESSOR_SKYLAKE 
    || ix86_arch == PROCESSOR_SKYLAKE_AVX512 || ix86_arch == PROCESSOR_CANNONLAKE 
    || ix86_arch == PROCESSOR_ICELAKE_CLIENT || ix86_arch == PROCESSOR_ICELAKE_SERVER 
    || ix86_arch == PROCESSOR_CASCADELAKE || ix86_arch == PROCESSOR_TIGERLAKE 
    || ix86_arch == PROCESSOR_COOPERLAKE; 
  if (hasINVPCID)
    rust_add_target_info("target_feature", "invpcid");
  if (TARGET_SGX)
    rust_add_target_info("target_feature", "sgx");
  if (TARGET_CLFLUSHOPT)
    rust_add_target_info("target_feature", "clflushopt");
  if (TARGET_CLWB)
    rust_add_target_info("target_feature", "clwb");
  if (TARGET_WBNOINVD)
    rust_add_target_info("target_feature", "wbnoinvd");
  if (TARGET_RDPID)
    rust_add_target_info("target_feature", "rdpid");
  if (TARGET_WAITPKG)
    rust_add_target_info("target_feature", "waitpkg");
  if (TARGET_ENQCMD)
    rust_add_target_info("target_feature", "enqcmd");

  // these are only enabled by choice in llvm, never by default - TODO determine if gcc supports them
  // rust_add_target_info("target_feature", "serialize");
  // rust_add_target_info("target_feature", "tsxldtrk");

  // TODO: gcc seems to not record whether to avoid memory operanded instructions, so basing it on llvm
  bool hasSlowTwoMemOps = ix86_arch == PROCESSOR_BONNELL || ix86_arch == PROCESSOR_SILVERMONT 
    || ix86_arch == PROCESSOR_GOLDMONT || ix86_arch == PROCESSOR_GOLDMONT_PLUS 
    || ix86_arch == PROCESSOR_TREMONT || ix86_arch == PROCESSOR_KNL || ix86_arch == PROCESSOR_KNM; 
  if (hasSlowTwoMemOps)
    rust_add_target_info("target_feature", "slow-two-mem-ops");

  // TODO: gcc seems to not record whether LEA needs input at AG stage, so basing it on llvm
  // TODO: maybe something to do with X86_TUNE_OPT_AGU?
  if (ix86_arch == PROCESSOR_BONNELL)
    rust_add_target_info("target_feature", "lea-uses-ag");

  // TODO: gcc seems to not record whether LEA with certain arguments is slow, so basing it on llvm
  // TODO: maybe TARGET_AVOID_LEA_FOR_ADDR has something to do with it?
  bool hasSlowLEA = ix86_arch == PROCESSOR_SILVERMONT || ix86_arch == PROCESSOR_GOLDMONT 
    || ix86_arch == PROCESSOR_GOLDMONT_PLUS || ix86_arch == PROCESSOR_TREMONT;
  if (hasSlowLEA)
    rust_add_target_info("target_feature", "slow-lea");
  
  // TODO: gcc seems to not record whether LEA with 3 ops or certain regs is slow, so basing it on llvm
  // TODO: maybe TARGET_AVOID_LEA_FOR_ADDR has something to do with it?
  bool hasSlow3OpsLEA = ix86_arch == PROCESSOR_SANDYBRIDGE || ix86_arch == PROCESSOR_HASWELL 
    || ix86_arch == PROCESSOR_SKYLAKE || ix86_arch == PROCESSOR_SKYLAKE_AVX512 
    || ix86_arch == PROCESSOR_CANNONLAKE || ix86_arch == PROCESSOR_ICELAKE_CLIENT 
    || ix86_arch == PROCESSOR_ICELAKE_SERVER || ix86_arch == PROCESSOR_CASCADELAKE 
    || ix86_arch == PROCESSOR_TIGERLAKE || ix86_arch == PROCESSOR_COOPERLAKE 
    || ix86_arch == PROCESSOR_KNL || ix86_arch == PROCESSOR_KNM || ix86_arch == PROCESSOR_K8; 
  if (hasSlow3OpsLEA)
    rust_add_target_info("target_feature", "slow-3ops-lea");

  // TODO: assuming that this is equivalent option - it strictly doesn't cover same cpus
  if (!TARGET_USE_INCDEC)
    rust_add_target_info("target_feature", "slow-incdec");
  // TODO: assuming that this mask actually refers to "hard float" and not x87 specifically
  if (!TARGET_80387)
    rust_add_target_info("target_feature", "soft-float");

  // TODO: gcc seems to not record if LZCNT/TZCNT has false deps on dest register, so basing it on llvm
  if (ix86_arch == PROCESSOR_HASWELL)
    rust_add_target_info("target_feature", "false-deps-lzcnt-tzcnt");

  if (TARGET_PCONFIG)
    rust_add_target_info("target_feature", "pconfig");

  // TODO: gcc seems to not record if variable-mask shuffles are fast, so basing it on llvm
  bool hasFastVariableShuffle = ix86_arch == PROCESSOR_HASWELL || ix86_arch == PROCESSOR_SKYLAKE 
    || ix86_arch == PROCESSOR_SKYLAKE_AVX512 || ix86_arch == PROCESSOR_CANNONLAKE 
    || ix86_arch == PROCESSOR_ICELAKE_CLIENT || ix86_arch == PROCESSOR_ICELAKE_SERVER 
    || ix86_arch == PROCESSOR_CASCADELAKE || ix86_arch == PROCESSOR_TIGERLAKE 
    || ix86_arch == PROCESSOR_COOPERLAKE; 
  if (hasFastVariableShuffle)
    rust_add_target_info("target_feature", "fast-variable-shuffle");

  // TODO: ensure that this actually refers to the right thing - difference in gcc and llvm description
  if (TARGET_VZEROUPPER)
    rust_add_target_info("target_feature", "vzeroupper");

  // option based on llvm arch analysis as gcc tuning costs seem to indicate a different result
  bool hasFastScalarFSQRT = ix86_arch == PROCESSOR_SANDYBRIDGE || ix86_arch == PROCESSOR_HASWELL 
    || ix86_arch == PROCESSOR_SKYLAKE || ix86_arch == PROCESSOR_SKYLAKE_AVX512 
    || ix86_arch == PROCESSOR_CANNONLAKE || ix86_arch == PROCESSOR_ICELAKE_CLIENT 
    || ix86_arch == PROCESSOR_ICELAKE_SERVER || ix86_arch == PROCESSOR_CASCADELAKE 
    || ix86_arch == PROCESSOR_TIGERLAKE || ix86_arch == PROCESSOR_COOPERLAKE;
  if (hasFastScalarFSQRT)
    rust_add_target_info("target_feature", "fast-scalar-fsqrt");

  // option also based on llvm arch analysis 
  bool hasFastVectorFSQRT = ix86_arch == PROCESSOR_SKYLAKE || ix86_arch == PROCESSOR_SKYLAKE_AVX512 
    || ix86_arch == PROCESSOR_CANNONLAKE || ix86_arch == PROCESSOR_ICELAKE_CLIENT 
    || ix86_arch == PROCESSOR_ICELAKE_SERVER || ix86_arch == PROCESSOR_CASCADELAKE 
    || ix86_arch == PROCESSOR_TIGERLAKE || ix86_arch == PROCESSOR_COOPERLAKE;
  if (hasFastVectorFSQRT)
    rust_add_target_info("target_feature", "fast-vector-fsqrt");

  bool hasFastLZCNT = ix86_arch == PROCESSOR_BTVER2 || ix86_arch == PROCESSOR_ZNVER1 
    || ix86_arch == PROCESSOR_ZNVER2;
  if (hasFastLZCNT)
    rust_add_target_info("target_feature", "fast-lzcnt");

  if (ix86_arch == PROCESSOR_SILVERMONT)
    rust_add_target_info("target_feature", "fast-7bytenop");

  bool hasFast11ByteNOP = ix86_arch == PROCESSOR_BDVER1 || ix86_arch == PROCESSOR_BDVER2 
    || ix86_arch == PROCESSOR_BDVER3 || ix86_arch == PROCESSOR_BDVER4;
  if (hasFast11ByteNOP)
    rust_add_target_info("target_feature", "fast-11bytenop");

  bool hasFast15ByteNOP = ix86_arch == PROCESSOR_SANDYBRIDGE || ix86_arch == PROCESSOR_HASWELL 
    || ix86_arch == PROCESSOR_SKYLAKE || ix86_arch == PROCESSOR_SKYLAKE_AVX512 
    || ix86_arch == PROCESSOR_CANNONLAKE || ix86_arch == PROCESSOR_ICELAKE_CLIENT 
    || ix86_arch == PROCESSOR_ICELAKE_SERVER || ix86_arch == PROCESSOR_CASCADELAKE 
    || ix86_arch == PROCESSOR_TIGERLAKE || ix86_arch == PROCESSOR_COOPERLAKE 
    || ix86_arch == PROCESSOR_BTVER1 || ix86_arch == PROCESSOR_BTVER2
    || ix86_arch == PROCESSOR_ZNVER1 || ix86_arch == PROCESSOR_ZNVER2;
  if (hasFast15ByteNOP)
    rust_add_target_info("target_feature", "fast-15bytenop");

  bool hasFastSHLDRotate = ix86_arch == PROCESSOR_SANDYBRIDGE || ix86_arch == PROCESSOR_HASWELL 
    || ix86_arch == PROCESSOR_SKYLAKE || ix86_arch == PROCESSOR_SKYLAKE_AVX512 
    || ix86_arch == PROCESSOR_CANNONLAKE || ix86_arch == PROCESSOR_ICELAKE_CLIENT 
    || ix86_arch == PROCESSOR_ICELAKE_SERVER || ix86_arch == PROCESSOR_CASCADELAKE 
    || ix86_arch == PROCESSOR_TIGERLAKE || ix86_arch == PROCESSOR_COOPERLAKE;
  if (hasFastSHLDRotate)
    rust_add_target_info("target_feature", "fast-shld-rotate");

  bool hasERMSB = ix86_arch == PROCESSOR_HASWELL || ix86_arch == PROCESSOR_SKYLAKE 
    || ix86_arch == PROCESSOR_SKYLAKE_AVX512 || ix86_arch == PROCESSOR_CANNONLAKE 
    || ix86_arch == PROCESSOR_ICELAKE_CLIENT || ix86_arch == PROCESSOR_ICELAKE_SERVER 
    || ix86_arch == PROCESSOR_CASCADELAKE || ix86_arch == PROCESSOR_TIGERLAKE 
    || ix86_arch == PROCESSOR_COOPERLAKE; 
  if (hasERMSB)
    rust_add_target_info("target_feature", "ermsbd");

  // TODO: may exist in gcc as tune macros, but not sure, so based on llvm arches
  bool hasBranchFusion = ix86_arch == PROCESSOR_BDVER1 || ix86_arch == PROCESSOR_BDVER2 
    || ix86_arch == PROCESSOR_BDVER3 || ix86_arch == PROCESSOR_BDVER4 || ix86_arch == PROCESSOR_ZNVER1 
    || ix86_arch == PROCESSOR_ZNVER2;
  if (hasBranchFusion)
    rust_add_target_info("target_feature", "branchfusion");

  // TODO: again, may exist as tune macros, but again based on llvm arches
  bool hasMacroFusion = ix86_arch == PROCESSOR_CORE2 || ix86_arch == PROCESSOR_NEHALEM 
    || ix86_arch == PROCESSOR_SANDYBRIDGE || ix86_arch == PROCESSOR_HASWELL 
    || ix86_arch == PROCESSOR_SKYLAKE || ix86_arch == PROCESSOR_SKYLAKE_AVX512 
    || ix86_arch == PROCESSOR_CANNONLAKE || ix86_arch == PROCESSOR_ICELAKE_CLIENT 
    || ix86_arch == PROCESSOR_ICELAKE_SERVER || ix86_arch == PROCESSOR_CASCADELAKE 
    || ix86_arch == PROCESSOR_TIGERLAKE || ix86_arch == PROCESSOR_COOPERLAKE || ix86_arch == PROCESSOR_K8;
  if (hasMacroFusion)
    rust_add_target_info("target_feature", "macrofusion");

  // TODO: is this equivalent to TARGET_USE_GATHER?
  bool hasFastGather = ix86_arch == PROCESSOR_SKYLAKE || ix86_arch == PROCESSOR_SKYLAKE_AVX512 
    || ix86_arch == PROCESSOR_CASCADELAKE || ix86_arch == PROCESSOR_COOPERLAKE 
    || ix86_arch == PROCESSOR_CANNONLAKE || ix86_arch == PROCESSOR_ICELAKE_CLIENT 
    || ix86_arch == PROCESSOR_ICELAKE_SERVER || ix86_arch == PROCESSOR_TIGERLAKE 
    || ix86_arch == PROCESSOR_KNL || ix86_arch == PROCESSOR_KNM;
  if (hasFastGather)
    rust_add_target_info("target_feature", "fast-gather");

  if (TARGET_PREFER_AVX128)
    rust_add_target_info("target_feature", "prefer-128-bit");
  if (TARGET_PREFER_AVX256)
    rust_add_target_info("target_feature", "prefer-256-bit");

  bool preferMaskRegisters = ix86_arch == PROCESSOR_KNL || ix86_arch == PROCESSOR_KNM;
  if (preferMaskRegisters)
    rust_add_target_info("target_feature", "prefer-mask-registers");

  /* TODO: add retpoline-indirect-calls, retpoline-indirect-branches, retpoline, retpoline-external-thunk, 
   * lvi-cfi (LVI control flow integrity), seses (speculative execution side-effect suppression)
   * lvi-load-hardening if gcc gets support */

  if (TARGET_MOVDIRI)
    rust_add_target_info("target_feature", "movdiri");
  if (TARGET_MOVDIR64B)
    rust_add_target_info("target_feature", "movdir64b");

  bool hasFastBEXTR = ix86_arch == PROCESSOR_BTVER2 || ix86_arch == PROCESSOR_BDVER2 
    || ix86_arch == PROCESSOR_BDVER3 || ix86_arch == PROCESSOR_BDVER4 || ix86_arch == PROCESSOR_ZNVER1 
    || ix86_arch == PROCESSOR_ZNVER2;
  if (hasFastBEXTR)
    rust_add_target_info("target_feature", "fast-bextr");

  if (ix86_arch == PROCESSOR_BTVER2)
    rust_add_target_info("target_feature", "fast-hops");

  bool hasFastScalarShiftMasks = ix86_arch == PROCESSOR_AMDFAM10 || ix86_arch == PROCESSOR_BTVER1 
    || ix86_arch == PROCESSOR_BTVER2 || ix86_arch == PROCESSOR_BDVER1 || ix86_arch == PROCESSOR_BDVER2 
    || ix86_arch == PROCESSOR_BDVER3 || ix86_arch == PROCESSOR_BDVER4 || ix86_arch == PROCESSOR_ZNVER1 
    || ix86_arch == PROCESSOR_ZNVER2 || ix86_arch == PROCESSOR_K8;
  if (hasFastScalarShiftMasks)
    rust_add_target_info("target_feature", "fast-scalar-shift-masks");

  bool hasFastVectorShiftMasks = ix86_arch == PROCESSOR_BTVER1 || ix86_arch == PROCESSOR_BTVER2;
  if (hasFastVectorShiftMasks)
    rust_add_target_info("target_feature", "fast-vector-shift-masks");

  bool useGoldmontDivSqrtCosts = ix86_arch == PROCESSOR_GOLDMONT || ix86_arch == PROCESSOR_GOLDMONT_PLUS 
    || ix86_arch == PROCESSOR_TREMONT;
  if (useGoldmontDivSqrtCosts)
    rust_add_target_info("target_feature", "use-glm-div-sqrt-costs");
  
  // TODO: determine if gcc supports alias analysis (in which case "use-aa" is defined)

  // features not supported by llvm but important enough for c frontend to define macros for
  /*if (TARGET_AVX5124VNNIW)
    rust_add_target_info("target_feature", "avx5124vnniw");
  if (TARGET_AVX5124FMAPS)
    rust_add_target_info("target_feature", "avx5124fmaps");
  if (TARGET_ABM)
    rust_add_target_info("target_feature", "abm");
  if ((ix86_fpmath & FPMATH_SSE) && TARGET_SSE)
    ; //def_or_undef (parse_in, "__SSE_MATH__");
  if ((ix86_fpmath & FPMATH_SSE) && TARGET_SSE2)
    ; //def_or_undef (parse_in, "__SSE2_MATH__");
  if (TARGET_MMX_WITH_SSE)
    ; //def_or_undef (parse_in, "__MMX_WITH_SSE__");
  if (TARGET_IAMCU)
    rust_add_target_info("target_feature", "iamcu");*/
}
