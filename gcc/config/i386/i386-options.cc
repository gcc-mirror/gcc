/* Copyright (C) 1988-2024 Free Software Foundation, Inc.

This file is part of GCC.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "cfgbuild.h"
#include "alias.h"
#include "fold-const.h"
#include "attribs.h"
#include "calls.h"
#include "stor-layout.h"
#include "varasm.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "explow.h"
#include "expr.h"
#include "cfgrtl.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "reload.h"
#include "gimplify.h"
#include "dwarf2.h"
#include "tm-constrs.h"
#include "cselib.h"
#include "sched-int.h"
#include "opts.h"
#include "tree-pass.h"
#include "context.h"
#include "pass_manager.h"
#include "target-globals.h"
#include "gimple-iterator.h"
#include "shrink-wrap.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "tree-iterator.h"
#include "dbgcnt.h"
#include "case-cfn-macros.h"
#include "dojump.h"
#include "fold-const-call.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "print-rtl.h"
#include "intl.h"
#include "ifcvt.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "wide-int-bitmask.h"
#include "tree-vector-builder.h"
#include "debug.h"
#include "dwarf2out.h"
#include "i386-options.h"

#include "x86-tune-costs.h"

#ifndef SUBTARGET32_DEFAULT_CPU
#define SUBTARGET32_DEFAULT_CPU "i386"
#endif

/* Processor feature/optimization bitmasks.  */
#define m_NONE HOST_WIDE_INT_0U
#define m_ALL (~HOST_WIDE_INT_0U)
#define m_386 (HOST_WIDE_INT_1U<<PROCESSOR_I386)
#define m_486 (HOST_WIDE_INT_1U<<PROCESSOR_I486)
#define m_PENT (HOST_WIDE_INT_1U<<PROCESSOR_PENTIUM)
#define m_LAKEMONT (HOST_WIDE_INT_1U<<PROCESSOR_LAKEMONT)
#define m_PPRO (HOST_WIDE_INT_1U<<PROCESSOR_PENTIUMPRO)
#define m_PENT4 (HOST_WIDE_INT_1U<<PROCESSOR_PENTIUM4)
#define m_NOCONA (HOST_WIDE_INT_1U<<PROCESSOR_NOCONA)
#define m_P4_NOCONA (m_PENT4 | m_NOCONA)
#define m_CORE2 (HOST_WIDE_INT_1U<<PROCESSOR_CORE2)
#define m_NEHALEM (HOST_WIDE_INT_1U<<PROCESSOR_NEHALEM)
#define m_SANDYBRIDGE (HOST_WIDE_INT_1U<<PROCESSOR_SANDYBRIDGE)
#define m_HASWELL (HOST_WIDE_INT_1U<<PROCESSOR_HASWELL)
#define m_BONNELL (HOST_WIDE_INT_1U<<PROCESSOR_BONNELL)
#define m_SILVERMONT (HOST_WIDE_INT_1U<<PROCESSOR_SILVERMONT)
#define m_SKYLAKE (HOST_WIDE_INT_1U<<PROCESSOR_SKYLAKE)
#define m_SKYLAKE_AVX512 (HOST_WIDE_INT_1U<<PROCESSOR_SKYLAKE_AVX512)
#define m_CANNONLAKE (HOST_WIDE_INT_1U<<PROCESSOR_CANNONLAKE)
#define m_ICELAKE_CLIENT (HOST_WIDE_INT_1U<<PROCESSOR_ICELAKE_CLIENT)
#define m_ICELAKE_SERVER (HOST_WIDE_INT_1U<<PROCESSOR_ICELAKE_SERVER)
#define m_CASCADELAKE (HOST_WIDE_INT_1U<<PROCESSOR_CASCADELAKE)
#define m_TIGERLAKE (HOST_WIDE_INT_1U<<PROCESSOR_TIGERLAKE)
#define m_COOPERLAKE (HOST_WIDE_INT_1U<<PROCESSOR_COOPERLAKE)
#define m_SAPPHIRERAPIDS (HOST_WIDE_INT_1U<<PROCESSOR_SAPPHIRERAPIDS)
#define m_ALDERLAKE (HOST_WIDE_INT_1U<<PROCESSOR_ALDERLAKE)
#define m_ROCKETLAKE (HOST_WIDE_INT_1U<<PROCESSOR_ROCKETLAKE)
#define m_GRANITERAPIDS (HOST_WIDE_INT_1U<<PROCESSOR_GRANITERAPIDS)
#define m_GRANITERAPIDS_D (HOST_WIDE_INT_1U<<PROCESSOR_GRANITERAPIDS_D)
#define m_ARROWLAKE (HOST_WIDE_INT_1U<<PROCESSOR_ARROWLAKE)
#define m_ARROWLAKE_S (HOST_WIDE_INT_1U<<PROCESSOR_ARROWLAKE_S)
#define m_PANTHERLAKE (HOST_WIDE_INT_1U<<PROCESSOR_PANTHERLAKE)
#define m_DIAMONDRAPIDS (HOST_WIDE_INT_1U<<PROCESSOR_DIAMONDRAPIDS)
#define m_CORE_AVX512 (m_SKYLAKE_AVX512 | m_CANNONLAKE \
		       | m_ICELAKE_CLIENT | m_ICELAKE_SERVER | m_CASCADELAKE \
		       | m_TIGERLAKE | m_COOPERLAKE | m_SAPPHIRERAPIDS \
		       | m_ROCKETLAKE | m_GRANITERAPIDS | m_GRANITERAPIDS_D \
		       | m_DIAMONDRAPIDS)
#define m_CORE_AVX2 (m_HASWELL | m_SKYLAKE | m_CORE_AVX512)
#define m_CORE_ALL (m_CORE2 | m_NEHALEM  | m_SANDYBRIDGE | m_CORE_AVX2)
#define m_CORE_HYBRID (m_ALDERLAKE | m_ARROWLAKE | m_ARROWLAKE_S \
		       | m_PANTHERLAKE)
#define m_GOLDMONT (HOST_WIDE_INT_1U<<PROCESSOR_GOLDMONT)
#define m_GOLDMONT_PLUS (HOST_WIDE_INT_1U<<PROCESSOR_GOLDMONT_PLUS)
#define m_TREMONT (HOST_WIDE_INT_1U<<PROCESSOR_TREMONT)
#define m_SIERRAFOREST (HOST_WIDE_INT_1U<<PROCESSOR_SIERRAFOREST)
#define m_GRANDRIDGE (HOST_WIDE_INT_1U<<PROCESSOR_GRANDRIDGE)
#define m_CLEARWATERFOREST (HOST_WIDE_INT_1U<<PROCESSOR_CLEARWATERFOREST)
#define m_CORE_ATOM (m_SIERRAFOREST | m_GRANDRIDGE | m_CLEARWATERFOREST)
#define m_INTEL (HOST_WIDE_INT_1U<<PROCESSOR_INTEL)
/* Gather Data Sampling / CVE-2022-40982 / INTEL-SA-00828.
   Software mitigation.  */
#define m_GDS (m_SKYLAKE | m_SKYLAKE_AVX512 | m_CANNONLAKE \
	       | m_ICELAKE_CLIENT | m_ICELAKE_SERVER | m_CASCADELAKE \
	       | m_TIGERLAKE | m_COOPERLAKE | m_ROCKETLAKE)

#define m_LUJIAZUI (HOST_WIDE_INT_1U<<PROCESSOR_LUJIAZUI)
#define m_YONGFENG (HOST_WIDE_INT_1U<<PROCESSOR_YONGFENG)
#define m_SHIJIDADAO (HOST_WIDE_INT_1U<<PROCESSOR_SHIJIDADAO)
#define m_ZHAOXIN  (m_LUJIAZUI | m_YONGFENG | m_SHIJIDADAO)

#define m_GEODE (HOST_WIDE_INT_1U<<PROCESSOR_GEODE)
#define m_K6 (HOST_WIDE_INT_1U<<PROCESSOR_K6)
#define m_K6_GEODE (m_K6 | m_GEODE)
#define m_K8 (HOST_WIDE_INT_1U<<PROCESSOR_K8)
#define m_ATHLON (HOST_WIDE_INT_1U<<PROCESSOR_ATHLON)
#define m_ATHLON_K8 (m_K8 | m_ATHLON)
#define m_AMDFAM10 (HOST_WIDE_INT_1U<<PROCESSOR_AMDFAM10)
#define m_BDVER1 (HOST_WIDE_INT_1U<<PROCESSOR_BDVER1)
#define m_BDVER2 (HOST_WIDE_INT_1U<<PROCESSOR_BDVER2)
#define m_BDVER3 (HOST_WIDE_INT_1U<<PROCESSOR_BDVER3)
#define m_BDVER4 (HOST_WIDE_INT_1U<<PROCESSOR_BDVER4)
#define m_ZNVER1 (HOST_WIDE_INT_1U<<PROCESSOR_ZNVER1)
#define m_ZNVER2 (HOST_WIDE_INT_1U<<PROCESSOR_ZNVER2)
#define m_ZNVER3 (HOST_WIDE_INT_1U<<PROCESSOR_ZNVER3)
#define m_ZNVER4 (HOST_WIDE_INT_1U<<PROCESSOR_ZNVER4)
#define m_ZNVER5 (HOST_WIDE_INT_1U<<PROCESSOR_ZNVER5)
#define m_BTVER1 (HOST_WIDE_INT_1U<<PROCESSOR_BTVER1)
#define m_BTVER2 (HOST_WIDE_INT_1U<<PROCESSOR_BTVER2)
#define m_BDVER	(m_BDVER1 | m_BDVER2 | m_BDVER3 | m_BDVER4)
#define m_BTVER (m_BTVER1 | m_BTVER2)
#define m_ZNVER (m_ZNVER1 | m_ZNVER2 | m_ZNVER3 | m_ZNVER4 | m_ZNVER5)
#define m_AMD_MULTIPLE (m_ATHLON_K8 | m_AMDFAM10 | m_BDVER | m_BTVER \
			| m_ZNVER)

#define m_GENERIC (HOST_WIDE_INT_1U<<PROCESSOR_GENERIC)

const char* ix86_tune_feature_names[X86_TUNE_LAST] = {
#undef DEF_TUNE
#define DEF_TUNE(tune, name, selector) name,
#include "x86-tune.def"
#undef DEF_TUNE
};

/* Feature tests against the various tunings.  */
unsigned char ix86_tune_features[X86_TUNE_LAST];

/* Feature tests against the various tunings used to create ix86_tune_features
   based on the processor mask.  */
static unsigned HOST_WIDE_INT initial_ix86_tune_features[X86_TUNE_LAST] = {
#undef DEF_TUNE
#define DEF_TUNE(tune, name, selector) selector,
#include "x86-tune.def"
#undef DEF_TUNE
};

/* Feature tests against the various architecture variations.  */
unsigned char ix86_arch_features[X86_ARCH_LAST];

struct ix86_target_opts
{
  const char *option;		/* option string */
  HOST_WIDE_INT mask;		/* isa mask options */
};

/* This table is ordered so that options like -msse4.2 that imply other
   ISAs come first.  Target string will be displayed in the same order.  */
static struct ix86_target_opts isa2_opts[] =
{
  { "-mcx16",		OPTION_MASK_ISA2_CX16 },
  { "-mvaes",		OPTION_MASK_ISA2_VAES },
  { "-mrdpid",		OPTION_MASK_ISA2_RDPID },
  { "-mpconfig",	OPTION_MASK_ISA2_PCONFIG },
  { "-mwbnoinvd",	OPTION_MASK_ISA2_WBNOINVD },
  { "-mavx512vp2intersect", OPTION_MASK_ISA2_AVX512VP2INTERSECT },
  { "-msgx",		OPTION_MASK_ISA2_SGX },
  { "-mhle",		OPTION_MASK_ISA2_HLE },
  { "-mmovbe",		OPTION_MASK_ISA2_MOVBE },
  { "-mclzero",		OPTION_MASK_ISA2_CLZERO },
  { "-mmwaitx",		OPTION_MASK_ISA2_MWAITX },
  { "-mmwait",		OPTION_MASK_ISA2_MWAIT },
  { "-mmovdir64b",	OPTION_MASK_ISA2_MOVDIR64B },
  { "-mwaitpkg",	OPTION_MASK_ISA2_WAITPKG },
  { "-mcldemote",	OPTION_MASK_ISA2_CLDEMOTE },
  { "-mptwrite",	OPTION_MASK_ISA2_PTWRITE },
  { "-mavx512bf16",	OPTION_MASK_ISA2_AVX512BF16 },
  { "-menqcmd",		OPTION_MASK_ISA2_ENQCMD },
  { "-mserialize",	OPTION_MASK_ISA2_SERIALIZE },
  { "-mtsxldtrk",	OPTION_MASK_ISA2_TSXLDTRK },
  { "-mamx-tile",	OPTION_MASK_ISA2_AMX_TILE },
  { "-mamx-int8",	OPTION_MASK_ISA2_AMX_INT8 },
  { "-mamx-bf16",	OPTION_MASK_ISA2_AMX_BF16 },
  { "-muintr",		OPTION_MASK_ISA2_UINTR },
  { "-mhreset",		OPTION_MASK_ISA2_HRESET },
  { "-mkl",		OPTION_MASK_ISA2_KL },
  { "-mwidekl", 	OPTION_MASK_ISA2_WIDEKL },
  { "-mavxvnni",	OPTION_MASK_ISA2_AVXVNNI },
  { "-mavx512fp16",	OPTION_MASK_ISA2_AVX512FP16 },
  { "-mavxifma",	OPTION_MASK_ISA2_AVXIFMA },
  { "-mavxvnniint8",	OPTION_MASK_ISA2_AVXVNNIINT8 },
  { "-mavxneconvert",   OPTION_MASK_ISA2_AVXNECONVERT },
  { "-mcmpccxadd",      OPTION_MASK_ISA2_CMPCCXADD },
  { "-mamx-fp16",       OPTION_MASK_ISA2_AMX_FP16 },
  { "-mprefetchi",      OPTION_MASK_ISA2_PREFETCHI },
  { "-mraoint", 	OPTION_MASK_ISA2_RAOINT },
  { "-mamx-complex",	OPTION_MASK_ISA2_AMX_COMPLEX },
  { "-mavxvnniint16",	OPTION_MASK_ISA2_AVXVNNIINT16 },
  { "-msm3",		OPTION_MASK_ISA2_SM3 },
  { "-msha512",		OPTION_MASK_ISA2_SHA512 },
  { "-msm4",            OPTION_MASK_ISA2_SM4 },
  { "-mevex512",	OPTION_MASK_ISA2_EVEX512 },
  { "-musermsr",	OPTION_MASK_ISA2_USER_MSR },
  { "-mavx10.1-256",	OPTION_MASK_ISA2_AVX10_1_256 },
  { "-mavx10.1-512",	OPTION_MASK_ISA2_AVX10_1_512 },
  { "-mavx10.2-256",	OPTION_MASK_ISA2_AVX10_2_256 },
  { "-mavx10.2-512",	OPTION_MASK_ISA2_AVX10_2_512 },
  { "-mamx-avx512",	OPTION_MASK_ISA2_AMX_AVX512 },
  { "-mamx-tf32",	OPTION_MASK_ISA2_AMX_TF32 },
  { "-mamx-transpose",	OPTION_MASK_ISA2_AMX_TRANSPOSE },
  { "-mamx-fp8", 	OPTION_MASK_ISA2_AMX_FP8 },
  { "-mmovrs",		OPTION_MASK_ISA2_MOVRS },
  { "-mamx-movrs",	OPTION_MASK_ISA2_AMX_MOVRS }
};
static struct ix86_target_opts isa_opts[] =
{
  { "-mavx512vpopcntdq", OPTION_MASK_ISA_AVX512VPOPCNTDQ },
  { "-mavx512bitalg",	OPTION_MASK_ISA_AVX512BITALG },
  { "-mvpclmulqdq",	OPTION_MASK_ISA_VPCLMULQDQ },
  { "-mgfni",		OPTION_MASK_ISA_GFNI },
  { "-mavx512vnni",	OPTION_MASK_ISA_AVX512VNNI },
  { "-mavx512vbmi2",	OPTION_MASK_ISA_AVX512VBMI2 },
  { "-mavx512vbmi",	OPTION_MASK_ISA_AVX512VBMI },
  { "-mavx512ifma",	OPTION_MASK_ISA_AVX512IFMA },
  { "-mavx512vl",	OPTION_MASK_ISA_AVX512VL },
  { "-mavx512bw",	OPTION_MASK_ISA_AVX512BW },
  { "-mavx512dq",	OPTION_MASK_ISA_AVX512DQ },
  { "-mavx512cd",	OPTION_MASK_ISA_AVX512CD },
  { "-mavx512f",	OPTION_MASK_ISA_AVX512F },
  { "-mavx2",		OPTION_MASK_ISA_AVX2 },
  { "-mfma",		OPTION_MASK_ISA_FMA },
  { "-mxop",		OPTION_MASK_ISA_XOP },
  { "-mfma4",		OPTION_MASK_ISA_FMA4 },
  { "-mf16c",		OPTION_MASK_ISA_F16C },
  { "-mavx",		OPTION_MASK_ISA_AVX },
/*{ "-msse4"		OPTION_MASK_ISA_SSE4 }, */
  { "-msse4.2",		OPTION_MASK_ISA_SSE4_2 },
  { "-msse4.1",		OPTION_MASK_ISA_SSE4_1 },
  { "-msse4a",		OPTION_MASK_ISA_SSE4A },
  { "-mssse3",		OPTION_MASK_ISA_SSSE3 },
  { "-msse3",		OPTION_MASK_ISA_SSE3 },
  { "-maes",		OPTION_MASK_ISA_AES },
  { "-msha",		OPTION_MASK_ISA_SHA },
  { "-mpclmul",		OPTION_MASK_ISA_PCLMUL },
  { "-msse2",		OPTION_MASK_ISA_SSE2 },
  { "-msse",		OPTION_MASK_ISA_SSE },
  { "-m3dnowa",		OPTION_MASK_ISA_3DNOW_A },
  { "-m3dnow",		OPTION_MASK_ISA_3DNOW },
  { "-mmmx",		OPTION_MASK_ISA_MMX },
  { "-mrtm",		OPTION_MASK_ISA_RTM },
  { "-mprfchw",		OPTION_MASK_ISA_PRFCHW },
  { "-mrdseed",		OPTION_MASK_ISA_RDSEED },
  { "-madx",		OPTION_MASK_ISA_ADX },
  { "-mclflushopt",	OPTION_MASK_ISA_CLFLUSHOPT },
  { "-mxsaves",		OPTION_MASK_ISA_XSAVES },
  { "-mxsavec",		OPTION_MASK_ISA_XSAVEC },
  { "-mxsaveopt",	OPTION_MASK_ISA_XSAVEOPT },
  { "-mxsave",		OPTION_MASK_ISA_XSAVE },
  { "-mabm",		OPTION_MASK_ISA_ABM },
  { "-mbmi",		OPTION_MASK_ISA_BMI },
  { "-mbmi2",		OPTION_MASK_ISA_BMI2 },
  { "-mlzcnt",		OPTION_MASK_ISA_LZCNT },
  { "-mtbm",		OPTION_MASK_ISA_TBM },
  { "-mpopcnt",		OPTION_MASK_ISA_POPCNT },
  { "-msahf",		OPTION_MASK_ISA_SAHF },
  { "-mcrc32",		OPTION_MASK_ISA_CRC32 },
  { "-mfsgsbase",	OPTION_MASK_ISA_FSGSBASE },
  { "-mrdrnd",		OPTION_MASK_ISA_RDRND },
  { "-mpku",		OPTION_MASK_ISA_PKU },
  { "-mlwp",		OPTION_MASK_ISA_LWP },
  { "-mfxsr",		OPTION_MASK_ISA_FXSR },
  { "-mclwb",		OPTION_MASK_ISA_CLWB },
  { "-mshstk",		OPTION_MASK_ISA_SHSTK },
  { "-mmovdiri",	OPTION_MASK_ISA_MOVDIRI }
};

/* Return 1 if TRAIT NAME is present in the OpenMP context's
   device trait set, return 0 if not present in any OpenMP context in the
   whole translation unit, or -1 if not present in the current OpenMP context
   but might be present in another OpenMP context in the same TU.  */

int
ix86_omp_device_kind_arch_isa (enum omp_device_kind_arch_isa trait,
			       const char *name)
{
  switch (trait)
    {
    case omp_device_kind:
      return strcmp (name, "cpu") == 0;
    case omp_device_arch:
      if (strcmp (name, "x86") == 0)
	return 1;
      if (TARGET_64BIT)
	{
	  if (TARGET_X32)
	    return strcmp (name, "x32") == 0;
	  else
	    return strcmp (name, "x86_64") == 0;
	}
      if (strcmp (name, "ia32") == 0 || strcmp (name, "i386") == 0)
	return 1;
      if (strcmp (name, "i486") == 0)
	return ix86_arch != PROCESSOR_I386 ? 1 : -1;
      if (strcmp (name, "i586") == 0)
	return (ix86_arch != PROCESSOR_I386
		&& ix86_arch != PROCESSOR_I486) ? 1 : -1;
      if (strcmp (name, "i686") == 0)
	return (ix86_arch != PROCESSOR_I386
		&& ix86_arch != PROCESSOR_I486
		&& ix86_arch != PROCESSOR_LAKEMONT
		&& ix86_arch != PROCESSOR_PENTIUM) ? 1 : -1;
      return 0;
    case omp_device_isa:
      for (int i = 0; i < 2; i++)
	{
	  struct ix86_target_opts *opts = i ? isa2_opts : isa_opts;
	  size_t nopts = i ? ARRAY_SIZE (isa2_opts) : ARRAY_SIZE (isa_opts);
	  HOST_WIDE_INT mask = i ? ix86_isa_flags2 : ix86_isa_flags;
	  for (size_t n = 0; n < nopts; n++)
	    {
	      /* Handle sse4 as an alias to sse4.2.  */
	      if (opts[n].mask == OPTION_MASK_ISA_SSE4_2)
		{
		  if (strcmp (name, "sse4") == 0)
		    return (mask & opts[n].mask) != 0 ? 1 : -1;
		}
	      if (strcmp (name, opts[n].option + 2) == 0)
		return (mask & opts[n].mask) != 0 ? 1 : -1;
	    }
	}
      return 0;
    default:
      gcc_unreachable ();
    }
}

/* Return a string that documents the current -m options.  The caller is
   responsible for freeing the string.  */

char *
ix86_target_string (HOST_WIDE_INT isa, HOST_WIDE_INT isa2,
		    int flags, int flags2,
		    const char *arch, const char *tune,
		    enum fpmath_unit fpmath,
		    enum prefer_vector_width pvw,
		    enum prefer_vector_width move_max,
		    enum prefer_vector_width store_max,
		    bool add_nl_p, bool add_abi_p)
{
  /* Flag options.  */
  static struct ix86_target_opts flag_opts[] =
  {
    { "-m128bit-long-double",		MASK_128BIT_LONG_DOUBLE },
    { "-mlong-double-128",		MASK_LONG_DOUBLE_128 },
    { "-mlong-double-64",		MASK_LONG_DOUBLE_64 },
    { "-m80387",			MASK_80387 },
    { "-maccumulate-outgoing-args",	MASK_ACCUMULATE_OUTGOING_ARGS },
    { "-malign-double",			MASK_ALIGN_DOUBLE },
    { "-mcld",				MASK_CLD },
    { "-mfp-ret-in-387",		MASK_FLOAT_RETURNS },
    { "-mieee-fp",			MASK_IEEE_FP },
    { "-minline-all-stringops",		MASK_INLINE_ALL_STRINGOPS },
    { "-minline-stringops-dynamically",	MASK_INLINE_STRINGOPS_DYNAMICALLY },
    { "-mms-bitfields",			MASK_MS_BITFIELD_LAYOUT },
    { "-mno-align-stringops",		MASK_NO_ALIGN_STRINGOPS },
    { "-mno-fancy-math-387",		MASK_NO_FANCY_MATH_387 },
    { "-mno-push-args",			MASK_NO_PUSH_ARGS },
    { "-mno-red-zone",			MASK_NO_RED_ZONE },
    { "-momit-leaf-frame-pointer",	MASK_OMIT_LEAF_FRAME_POINTER },
    { "-mrecip",			MASK_RECIP },
    { "-mrtd",				MASK_RTD },
    { "-msseregparm",			MASK_SSEREGPARM },
    { "-mstack-arg-probe",		MASK_STACK_PROBE },
    { "-mtls-direct-seg-refs",		MASK_TLS_DIRECT_SEG_REFS },
    { "-mvect8-ret-in-mem",		MASK_VECT8_RETURNS },
    { "-m8bit-idiv",			MASK_USE_8BIT_IDIV },
    { "-mvzeroupper",			MASK_VZEROUPPER },
    { "-mstv",				MASK_STV },
    { "-mavx256-split-unaligned-load",	MASK_AVX256_SPLIT_UNALIGNED_LOAD },
    { "-mavx256-split-unaligned-store",	MASK_AVX256_SPLIT_UNALIGNED_STORE },
    { "-mcall-ms2sysv-xlogues",		MASK_CALL_MS2SYSV_XLOGUES },
    { "-mrelax-cmpxchg-loop",		MASK_RELAX_CMPXCHG_LOOP }
  };

  /* Additional flag options.  */
  static struct ix86_target_opts flag2_opts[] =
  {
    { "-mgeneral-regs-only",		OPTION_MASK_GENERAL_REGS_ONLY }
  };

  const char *opts[ARRAY_SIZE (isa_opts) + ARRAY_SIZE (isa2_opts)
		   + ARRAY_SIZE (flag_opts) + ARRAY_SIZE (flag2_opts) + 6][2];

  char isa_other[40];
  char isa2_other[40];
  char flags_other[40];
  char flags2_other[40];
  unsigned num = 0;
  unsigned i, j;
  char *ret;
  char *ptr;
  size_t len;
  size_t line_len;
  size_t sep_len;
  const char *abi;

  memset (opts, '\0', sizeof (opts));

  /* Add -march= option.  */
  if (arch)
    {
      opts[num][0] = "-march=";
      opts[num++][1] = arch;
    }

  /* Add -mtune= option.  */
  if (tune)
    {
      opts[num][0] = "-mtune=";
      opts[num++][1] = tune;
    }

  /* Add -m32/-m64/-mx32.  */
  if (add_abi_p)
    {
      if ((isa & OPTION_MASK_ISA_64BIT) != 0)
	{
	  if ((isa & OPTION_MASK_ABI_64) != 0)
	    abi = "-m64";
	  else
	    abi = "-mx32";
	}
      else
	abi = "-m32";
      opts[num++][0] = abi;
    }
  isa &= ~(OPTION_MASK_ISA_64BIT | OPTION_MASK_ABI_64 | OPTION_MASK_ABI_X32);

  /* Pick out the options in isa2 options.  */
  for (i = 0; i < ARRAY_SIZE (isa2_opts); i++)
    {
      if ((isa2 & isa2_opts[i].mask) != 0)
	{
	  opts[num++][0] = isa2_opts[i].option;
	  isa2 &= ~ isa2_opts[i].mask;
	}
    }

  if (isa2 && add_nl_p)
    {
      opts[num++][0] = isa2_other;
      sprintf (isa2_other, "(other isa2: %#" HOST_WIDE_INT_PRINT "x)", isa2);
    }

  /* Pick out the options in isa options.  */
  for (i = 0; i < ARRAY_SIZE (isa_opts); i++)
    {
      if ((isa & isa_opts[i].mask) != 0)
	{
	  opts[num++][0] = isa_opts[i].option;
	  isa &= ~ isa_opts[i].mask;
	}
    }

  if (isa && add_nl_p)
    {
      opts[num++][0] = isa_other;
      sprintf (isa_other, "(other isa: %#" HOST_WIDE_INT_PRINT "x)", isa);
    }

  /* Add flag options.  */
  for (i = 0; i < ARRAY_SIZE (flag_opts); i++)
    {
      if ((flags & flag_opts[i].mask) != 0)
	{
	  opts[num++][0] = flag_opts[i].option;
	  flags &= ~ flag_opts[i].mask;
	}
    }

  if (flags && add_nl_p)
    {
      opts[num++][0] = flags_other;
      sprintf (flags_other, "(other flags: %#x)", flags);
    }

    /* Add additional flag options.  */
  for (i = 0; i < ARRAY_SIZE (flag2_opts); i++)
    {
      if ((flags2 & flag2_opts[i].mask) != 0)
	{
	  opts[num++][0] = flag2_opts[i].option;
	  flags2 &= ~ flag2_opts[i].mask;
	}
    }

  if (flags2 && add_nl_p)
    {
      opts[num++][0] = flags2_other;
      sprintf (flags2_other, "(other flags2: %#x)", flags2);
    }

  /* Add -mfpmath= option.  */
  if (fpmath)
    {
      opts[num][0] = "-mfpmath=";
      switch ((int) fpmath)
	{
	case FPMATH_387:
	  opts[num++][1] = "387";
	  break;

	case FPMATH_SSE:
	  opts[num++][1] = "sse";
	  break;

	case FPMATH_387 | FPMATH_SSE:
	  opts[num++][1] = "sse+387";
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  auto add_vector_width = [&opts, &num] (prefer_vector_width pvw,
					 const char *cmd)
    {
      opts[num][0] = cmd;
      switch ((int) pvw)
	{
	case PVW_AVX128:
	  opts[num++][1] = "128";
	  break;

	case PVW_AVX256:
	  opts[num++][1] = "256";
	  break;

	case PVW_AVX512:
	  opts[num++][1] = "512";
	  break;

	default:
	  gcc_unreachable ();
	}
    };

  /* Add -mprefer-vector-width= option.  */
  if (pvw)
    add_vector_width (pvw, "-mprefer-vector-width=");

  /* Add -mmove-max= option.  */
  if (move_max)
    add_vector_width (move_max, "-mmove-max=");

  /* Add -mstore-max= option.  */
  if (store_max)
    add_vector_width (store_max, "-mstore-max=");

  /* Any options?  */
  if (num == 0)
    return NULL;

  gcc_assert (num < ARRAY_SIZE (opts));

  /* Size the string.  */
  len = 0;
  sep_len = (add_nl_p) ? 3 : 1;
  for (i = 0; i < num; i++)
    {
      len += sep_len;
      for (j = 0; j < 2; j++)
	if (opts[i][j])
	  len += strlen (opts[i][j]);
    }

  /* Build the string.  */
  ret = ptr = (char *) xmalloc (len);
  line_len = 0;

  for (i = 0; i < num; i++)
    {
      size_t len2[2];

      for (j = 0; j < 2; j++)
	len2[j] = (opts[i][j]) ? strlen (opts[i][j]) : 0;

      if (i != 0)
	{
	  *ptr++ = ' ';
	  line_len++;

	  if (add_nl_p && line_len + len2[0] + len2[1] > 70)
	    {
	      *ptr++ = '\\';
	      *ptr++ = '\n';
	      line_len = 0;
	    }
	}

      for (j = 0; j < 2; j++)
	if (opts[i][j])
	  {
	    memcpy (ptr, opts[i][j], len2[j]);
	    ptr += len2[j];
	    line_len += len2[j];
	  }
    }

  *ptr = '\0';
  gcc_assert (ret + len >= ptr);

  return ret;
}

/* Function that is callable from the debugger to print the current
   options.  */
void ATTRIBUTE_UNUSED
ix86_debug_options (void)
{
  char *opts = ix86_target_string (ix86_isa_flags, ix86_isa_flags2,
				   target_flags, ix86_target_flags,
				   ix86_arch_string, ix86_tune_string,
				   ix86_fpmath, prefer_vector_width_type,
				   ix86_move_max, ix86_store_max,
				   true, true);

  if (opts)
    {
      fprintf (stderr, "%s\n\n", opts);
      free (opts);
    }
  else
    fputs ("<no options>\n\n", stderr);

  return;
}

/* Save the current options */

void
ix86_function_specific_save (struct cl_target_option *ptr,
			     struct gcc_options *opts,
			     struct gcc_options */* opts_set */)
{
  ptr->arch = ix86_arch;
  ptr->schedule = ix86_schedule;
  ptr->prefetch_sse = ix86_prefetch_sse;
  ptr->tune = ix86_tune;
  ptr->branch_cost = ix86_branch_cost;
  ptr->tune_defaulted = ix86_tune_defaulted;
  ptr->arch_specified = ix86_arch_specified;
  ptr->x_ix86_apx_features = opts->x_ix86_apx_features;
  ptr->x_ix86_isa_flags_explicit = opts->x_ix86_isa_flags_explicit;
  ptr->x_ix86_isa_flags2_explicit = opts->x_ix86_isa_flags2_explicit;
  ptr->x_ix86_no_avx512_explicit = opts->x_ix86_no_avx512_explicit;
  ptr->x_ix86_no_avx10_1_explicit = opts->x_ix86_no_avx10_1_explicit;
  ptr->x_recip_mask_explicit = opts->x_recip_mask_explicit;
  ptr->x_ix86_arch_string = opts->x_ix86_arch_string;
  ptr->x_ix86_tune_string = opts->x_ix86_tune_string;
  ptr->x_ix86_asm_dialect = opts->x_ix86_asm_dialect;
  ptr->x_ix86_branch_cost = opts->x_ix86_branch_cost;
  ptr->x_ix86_dump_tunes = opts->x_ix86_dump_tunes;
  ptr->x_ix86_force_align_arg_pointer = opts->x_ix86_force_align_arg_pointer;
  ptr->x_ix86_force_drap = opts->x_ix86_force_drap;
  ptr->x_ix86_recip_name = opts->x_ix86_recip_name;
  ptr->x_ix86_section_threshold = opts->x_ix86_section_threshold;
  ptr->x_ix86_sse2avx = opts->x_ix86_sse2avx;
  ptr->x_ix86_stack_protector_guard = opts->x_ix86_stack_protector_guard;
  ptr->x_ix86_stringop_alg = opts->x_ix86_stringop_alg;
  ptr->x_ix86_tls_dialect = opts->x_ix86_tls_dialect;
  ptr->x_ix86_tune_ctrl_string = opts->x_ix86_tune_ctrl_string;
  ptr->x_ix86_tune_memcpy_strategy = opts->x_ix86_tune_memcpy_strategy;
  ptr->x_ix86_tune_memset_strategy = opts->x_ix86_tune_memset_strategy;
  ptr->x_ix86_tune_no_default = opts->x_ix86_tune_no_default;

  /* The fields are char but the variables are not; make sure the
     values fit in the fields.  */
  gcc_assert (ptr->arch == ix86_arch);
  gcc_assert (ptr->schedule == ix86_schedule);
  gcc_assert (ptr->tune == ix86_tune);
  gcc_assert (ptr->branch_cost == ix86_branch_cost);
}

/* Feature tests against the various architecture variations, used to create
   ix86_arch_features based on the processor mask.  */
static unsigned HOST_WIDE_INT initial_ix86_arch_features[X86_ARCH_LAST] = {
  /* X86_ARCH_CMOV: Conditional move was added for pentiumpro.  */
  ~(m_386 | m_486 | m_PENT | m_LAKEMONT | m_K6),

  /* X86_ARCH_CMPXCHG: Compare and exchange was added for 80486.  */
  ~m_386,

  /* X86_ARCH_CMPXCHG8B: Compare and exchange 8 bytes was added for pentium. */
  ~(m_386 | m_486),

  /* X86_ARCH_XADD: Exchange and add was added for 80486.  */
  ~m_386,

  /* X86_ARCH_BSWAP: Byteswap was added for 80486.  */
  ~m_386,
};

/* This table must be in sync with enum processor_type in i386.h.  */
static const struct processor_costs *processor_cost_table[] =
{
  &generic_cost,
  &i386_cost,
  &i486_cost,
  &pentium_cost,
  &lakemont_cost,
  &pentiumpro_cost,
  &pentium4_cost,
  &nocona_cost,
  &core_cost,
  &core_cost,
  &core_cost,
  &core_cost,
  &atom_cost,
  &slm_cost,
  &slm_cost,
  &slm_cost,
  &tremont_cost,
  &alderlake_cost,
  &alderlake_cost,
  &alderlake_cost,
  &skylake_cost,
  &skylake_cost,
  &icelake_cost,
  &icelake_cost,
  &icelake_cost,
  &skylake_cost,
  &icelake_cost,
  &skylake_cost,
  &icelake_cost,
  &alderlake_cost,
  &icelake_cost,
  &icelake_cost,
  &icelake_cost,
  &alderlake_cost,
  &alderlake_cost,
  &alderlake_cost,
  &icelake_cost,
  &intel_cost,
  &lujiazui_cost,
  &yongfeng_cost,
  &shijidadao_cost,
  &geode_cost,
  &k6_cost,
  &athlon_cost,
  &k8_cost,
  &amdfam10_cost,
  &bdver_cost,
  &bdver_cost,
  &bdver_cost,
  &bdver_cost,
  &btver1_cost,
  &btver2_cost,
  &znver1_cost,
  &znver2_cost,
  &znver3_cost,
  &znver4_cost,
  &znver5_cost
};

/* Guarantee that the array is aligned with enum processor_type.  */
STATIC_ASSERT (ARRAY_SIZE (processor_cost_table) == PROCESSOR_max);

static bool
ix86_option_override_internal (bool main_args_p,
			       struct gcc_options *opts,
			       struct gcc_options *opts_set);
static void
set_ix86_tune_features (struct gcc_options *opts,
			enum processor_type ix86_tune, bool dump);

/* Restore the current options */

void
ix86_function_specific_restore (struct gcc_options *opts,
				struct gcc_options */* opts_set */,
				struct cl_target_option *ptr)
{
  enum processor_type old_tune = ix86_tune;
  enum processor_type old_arch = ix86_arch;
  unsigned HOST_WIDE_INT ix86_arch_mask;
  int i;

  /* We don't change -fPIC.  */
  opts->x_flag_pic = flag_pic;

  ix86_arch = (enum processor_type) ptr->arch;
  ix86_schedule = (enum attr_cpu) ptr->schedule;
  ix86_tune = (enum processor_type) ptr->tune;
  ix86_prefetch_sse = ptr->prefetch_sse;
  ix86_tune_defaulted = ptr->tune_defaulted;
  ix86_arch_specified = ptr->arch_specified;
  opts->x_ix86_apx_features = ptr->x_ix86_apx_features;
  opts->x_ix86_isa_flags_explicit = ptr->x_ix86_isa_flags_explicit;
  opts->x_ix86_isa_flags2_explicit = ptr->x_ix86_isa_flags2_explicit;
  opts->x_ix86_no_avx512_explicit = ptr->x_ix86_no_avx512_explicit;
  opts->x_ix86_no_avx10_1_explicit = ptr->x_ix86_no_avx10_1_explicit;
  opts->x_recip_mask_explicit = ptr->x_recip_mask_explicit;
  opts->x_ix86_arch_string = ptr->x_ix86_arch_string;
  opts->x_ix86_tune_string = ptr->x_ix86_tune_string;
  opts->x_ix86_asm_dialect = ptr->x_ix86_asm_dialect;
  opts->x_ix86_branch_cost = ptr->x_ix86_branch_cost;
  opts->x_ix86_dump_tunes = ptr->x_ix86_dump_tunes;
  opts->x_ix86_force_align_arg_pointer = ptr->x_ix86_force_align_arg_pointer;
  opts->x_ix86_force_drap = ptr->x_ix86_force_drap;
  opts->x_ix86_recip_name = ptr->x_ix86_recip_name;
  opts->x_ix86_section_threshold = ptr->x_ix86_section_threshold;
  opts->x_ix86_sse2avx = ptr->x_ix86_sse2avx;
  opts->x_ix86_stack_protector_guard = ptr->x_ix86_stack_protector_guard;
  opts->x_ix86_stringop_alg = ptr->x_ix86_stringop_alg;
  opts->x_ix86_tls_dialect = ptr->x_ix86_tls_dialect;
  opts->x_ix86_tune_ctrl_string = ptr->x_ix86_tune_ctrl_string;
  opts->x_ix86_tune_memcpy_strategy = ptr->x_ix86_tune_memcpy_strategy;
  opts->x_ix86_tune_memset_strategy = ptr->x_ix86_tune_memset_strategy;
  opts->x_ix86_tune_no_default = ptr->x_ix86_tune_no_default;
  ix86_tune_cost = processor_cost_table[ix86_tune];
  /* TODO: ix86_cost should be chosen at instruction or function granuality
     so for cold code we use size_cost even in !optimize_size compilation.  */
  if (opts->x_optimize_size)
    ix86_cost = &ix86_size_cost;
  else
    ix86_cost = ix86_tune_cost;

  /* Recreate the arch feature tests if the arch changed */
  if (old_arch != ix86_arch)
    {
      ix86_arch_mask = HOST_WIDE_INT_1U << ix86_arch;
      for (i = 0; i < X86_ARCH_LAST; ++i)
	ix86_arch_features[i]
	  = !!(initial_ix86_arch_features[i] & ix86_arch_mask);
    }

  /* Recreate the tune optimization tests */
  if (old_tune != ix86_tune)
    set_ix86_tune_features (opts, ix86_tune, false);
}

/* Adjust target options after streaming them in.  This is mainly about
   reconciling them with global options.  */

void
ix86_function_specific_post_stream_in (struct cl_target_option *ptr)
{
  /* flag_pic is a global option, but ix86_cmodel is target saved option
     partly computed from flag_pic.  If flag_pic is on, adjust x_ix86_cmodel
     for PIC, or error out.  */
  if (flag_pic)
    switch (ptr->x_ix86_cmodel)
      {
      case CM_SMALL:
	ptr->x_ix86_cmodel = CM_SMALL_PIC;
	break;

      case CM_MEDIUM:
	ptr->x_ix86_cmodel = CM_MEDIUM_PIC;
	break;

      case CM_LARGE:
	ptr->x_ix86_cmodel = CM_LARGE_PIC;
	break;

      case CM_KERNEL:
	error ("code model %s does not support PIC mode", "kernel");
	break;

      default:
	break;
      }
  else
    switch (ptr->x_ix86_cmodel)
      {
      case CM_SMALL_PIC:
	ptr->x_ix86_cmodel = CM_SMALL;
	break;

      case CM_MEDIUM_PIC:
	ptr->x_ix86_cmodel = CM_MEDIUM;
	break;

      case CM_LARGE_PIC:
	ptr->x_ix86_cmodel = CM_LARGE;
	break;

      default:
	break;
      }
}

/* Print the current options */

void
ix86_function_specific_print (FILE *file, int indent,
			      struct cl_target_option *ptr)
{
  char *target_string
    = ix86_target_string (ptr->x_ix86_isa_flags, ptr->x_ix86_isa_flags2,
			  ptr->x_target_flags, ptr->x_ix86_target_flags,
			  NULL, NULL, ptr->x_ix86_fpmath,
			  ptr->x_prefer_vector_width_type,
			  ptr->x_ix86_move_max, ptr->x_ix86_store_max,
			  false, true);

  gcc_assert (ptr->arch < PROCESSOR_max);
  fprintf (file, "%*sarch = %d (%s)\n",
	   indent, "",
	   ptr->arch, processor_names[ptr->arch]);

  gcc_assert (ptr->tune < PROCESSOR_max);
  fprintf (file, "%*stune = %d (%s)\n",
	   indent, "",
	   ptr->tune, processor_names[ptr->tune]);

  fprintf (file, "%*sbranch_cost = %d\n", indent, "", ptr->branch_cost);

  if (target_string)
    {
      fprintf (file, "%*s%s\n", indent, "", target_string);
      free (target_string);
    }
}


/* Inner function to process the attribute((target(...))), take an argument and
   set the current options from the argument. If we have a list, recursively go
   over the list.  */

static bool
ix86_valid_target_attribute_inner_p (tree fndecl, tree args, char *p_strings[],
				     struct gcc_options *opts,
				     struct gcc_options *opts_set,
				     struct gcc_options *enum_opts_set,
				     bool target_clone_attr)
{
  char *next_optstr;
  bool ret = true;

#define IX86_ATTR_ISA(S,O)   { S, sizeof (S)-1, ix86_opt_isa, O, 0 }
#define IX86_ATTR_STR(S,O)   { S, sizeof (S)-1, ix86_opt_str, O, 0 }
#define IX86_ATTR_ENUM(S,O)  { S, sizeof (S)-1, ix86_opt_enum, O, 0 }
#define IX86_ATTR_YES(S,O,M) { S, sizeof (S)-1, ix86_opt_yes, O, M }
#define IX86_ATTR_NO(S,O,M)  { S, sizeof (S)-1, ix86_opt_no,  O, M }
#define IX86_ATTR_IX86_YES(S,O,M) \
  { S, sizeof (S)-1, ix86_opt_ix86_yes, O, M }
#define IX86_ATTR_IX86_NO(S,O,M) \
  { S, sizeof (S)-1, ix86_opt_ix86_no,  O, M }

  enum ix86_opt_type
  {
    ix86_opt_unknown,
    ix86_opt_yes,
    ix86_opt_no,
    ix86_opt_ix86_yes,
    ix86_opt_ix86_no,
    ix86_opt_str,
    ix86_opt_enum,
    ix86_opt_isa
  };

  static const struct
  {
    const char *string;
    size_t len;
    enum ix86_opt_type type;
    int opt;
    int mask;
  } attrs[] = {
    /* isa options */
    IX86_ATTR_ISA ("pconfig",	OPT_mpconfig),
    IX86_ATTR_ISA ("wbnoinvd",	OPT_mwbnoinvd),
    IX86_ATTR_ISA ("sgx",	OPT_msgx),
    IX86_ATTR_ISA ("avx512vpopcntdq", OPT_mavx512vpopcntdq),
    IX86_ATTR_ISA ("avx512vbmi2", OPT_mavx512vbmi2),
    IX86_ATTR_ISA ("avx512vnni", OPT_mavx512vnni),
    IX86_ATTR_ISA ("avx512bitalg", OPT_mavx512bitalg),
    IX86_ATTR_ISA ("avx512vp2intersect", OPT_mavx512vp2intersect),

    IX86_ATTR_ISA ("avx512vbmi", OPT_mavx512vbmi),
    IX86_ATTR_ISA ("avx512ifma", OPT_mavx512ifma),
    IX86_ATTR_ISA ("avx512vl",	OPT_mavx512vl),
    IX86_ATTR_ISA ("avx512bw",	OPT_mavx512bw),
    IX86_ATTR_ISA ("avx512dq",	OPT_mavx512dq),
    IX86_ATTR_ISA ("avx512cd",	OPT_mavx512cd),
    IX86_ATTR_ISA ("avx512f",	OPT_mavx512f),
    IX86_ATTR_ISA ("avx2",	OPT_mavx2),
    IX86_ATTR_ISA ("fma",	OPT_mfma),
    IX86_ATTR_ISA ("xop",	OPT_mxop),
    IX86_ATTR_ISA ("fma4",	OPT_mfma4),
    IX86_ATTR_ISA ("f16c",	OPT_mf16c),
    IX86_ATTR_ISA ("avx",	OPT_mavx),
    IX86_ATTR_ISA ("sse4",	OPT_msse4),
    IX86_ATTR_ISA ("sse4.2",	OPT_msse4_2),
    IX86_ATTR_ISA ("sse4.1",	OPT_msse4_1),
    IX86_ATTR_ISA ("sse4a",	OPT_msse4a),
    IX86_ATTR_ISA ("ssse3",	OPT_mssse3),
    IX86_ATTR_ISA ("sse3",	OPT_msse3),
    IX86_ATTR_ISA ("aes",	OPT_maes),
    IX86_ATTR_ISA ("sha",	OPT_msha),
    IX86_ATTR_ISA ("pclmul",	OPT_mpclmul),
    IX86_ATTR_ISA ("sse2",	OPT_msse2),
    IX86_ATTR_ISA ("sse",	OPT_msse),
    IX86_ATTR_ISA ("3dnowa",	OPT_m3dnowa),
    IX86_ATTR_ISA ("3dnow",	OPT_m3dnow),
    IX86_ATTR_ISA ("mmx",	OPT_mmmx),
    IX86_ATTR_ISA ("rtm",	OPT_mrtm),
    IX86_ATTR_ISA ("prfchw",	OPT_mprfchw),
    IX86_ATTR_ISA ("rdseed",	OPT_mrdseed),
    IX86_ATTR_ISA ("adx",	OPT_madx),
    IX86_ATTR_ISA ("clflushopt", OPT_mclflushopt),
    IX86_ATTR_ISA ("xsaves",	OPT_mxsaves),
    IX86_ATTR_ISA ("xsavec",	OPT_mxsavec),
    IX86_ATTR_ISA ("xsaveopt",	OPT_mxsaveopt),
    IX86_ATTR_ISA ("xsave",	OPT_mxsave),
    IX86_ATTR_ISA ("abm",	OPT_mabm),
    IX86_ATTR_ISA ("bmi",	OPT_mbmi),
    IX86_ATTR_ISA ("bmi2",	OPT_mbmi2),
    IX86_ATTR_ISA ("lzcnt",	OPT_mlzcnt),
    IX86_ATTR_ISA ("tbm",	OPT_mtbm),
    IX86_ATTR_ISA ("popcnt",	OPT_mpopcnt),
    IX86_ATTR_ISA ("cx16",	OPT_mcx16),
    IX86_ATTR_ISA ("sahf",	OPT_msahf),
    IX86_ATTR_ISA ("movbe",	OPT_mmovbe),
    IX86_ATTR_ISA ("crc32",	OPT_mcrc32),
    IX86_ATTR_ISA ("fsgsbase",	OPT_mfsgsbase),
    IX86_ATTR_ISA ("rdrnd",	OPT_mrdrnd),
    IX86_ATTR_ISA ("mwaitx",	OPT_mmwaitx),
    IX86_ATTR_ISA ("mwait",	OPT_mmwait),
    IX86_ATTR_ISA ("clzero",	OPT_mclzero),
    IX86_ATTR_ISA ("pku",	OPT_mpku),
    IX86_ATTR_ISA ("lwp",	OPT_mlwp),
    IX86_ATTR_ISA ("hle",	OPT_mhle),
    IX86_ATTR_ISA ("fxsr",	OPT_mfxsr),
    IX86_ATTR_ISA ("clwb",	OPT_mclwb),
    IX86_ATTR_ISA ("rdpid",	OPT_mrdpid),
    IX86_ATTR_ISA ("gfni",	OPT_mgfni),
    IX86_ATTR_ISA ("shstk",	OPT_mshstk),
    IX86_ATTR_ISA ("vaes",	OPT_mvaes),
    IX86_ATTR_ISA ("vpclmulqdq", OPT_mvpclmulqdq),
    IX86_ATTR_ISA ("movdiri", OPT_mmovdiri),
    IX86_ATTR_ISA ("movdir64b", OPT_mmovdir64b),
    IX86_ATTR_ISA ("waitpkg", OPT_mwaitpkg),
    IX86_ATTR_ISA ("cldemote", OPT_mcldemote),
    IX86_ATTR_ISA ("uintr", OPT_muintr),
    IX86_ATTR_ISA ("ptwrite",   OPT_mptwrite),
    IX86_ATTR_ISA ("kl", OPT_mkl),
    IX86_ATTR_ISA ("widekl",	OPT_mwidekl),
    IX86_ATTR_ISA ("avx512bf16",   OPT_mavx512bf16),
    IX86_ATTR_ISA ("enqcmd", OPT_menqcmd),
    IX86_ATTR_ISA ("serialize", OPT_mserialize),
    IX86_ATTR_ISA ("tsxldtrk", OPT_mtsxldtrk),
    IX86_ATTR_ISA ("amx-tile", OPT_mamx_tile),
    IX86_ATTR_ISA ("amx-int8", OPT_mamx_int8),
    IX86_ATTR_ISA ("amx-bf16", OPT_mamx_bf16),
    IX86_ATTR_ISA ("hreset", OPT_mhreset),
    IX86_ATTR_ISA ("avxvnni",   OPT_mavxvnni),
    IX86_ATTR_ISA ("avx512fp16", OPT_mavx512fp16),
    IX86_ATTR_ISA ("avxifma", OPT_mavxifma),
    IX86_ATTR_ISA ("avxvnniint8", OPT_mavxvnniint8),
    IX86_ATTR_ISA ("avxneconvert", OPT_mavxneconvert),
    IX86_ATTR_ISA ("cmpccxadd",   OPT_mcmpccxadd),
    IX86_ATTR_ISA ("amx-fp16", OPT_mamx_fp16),
    IX86_ATTR_ISA ("prefetchi",   OPT_mprefetchi),
    IX86_ATTR_ISA ("raoint", OPT_mraoint),
    IX86_ATTR_ISA ("amx-complex", OPT_mamx_complex),
    IX86_ATTR_ISA ("avxvnniint16", OPT_mavxvnniint16),
    IX86_ATTR_ISA ("sm3", OPT_msm3),
    IX86_ATTR_ISA ("sha512", OPT_msha512),
    IX86_ATTR_ISA ("sm4", OPT_msm4),
    IX86_ATTR_ISA ("apxf", OPT_mapxf),
    IX86_ATTR_ISA ("evex512", OPT_mevex512),
    IX86_ATTR_ISA ("usermsr", OPT_musermsr),
    IX86_ATTR_ISA ("avx10.1", OPT_mavx10_1_256),
    IX86_ATTR_ISA ("avx10.1-256", OPT_mavx10_1_256),
    IX86_ATTR_ISA ("avx10.1-512", OPT_mavx10_1_512),
    IX86_ATTR_ISA ("avx10.2", OPT_mavx10_2_256),
    IX86_ATTR_ISA ("avx10.2-256", OPT_mavx10_2_256),
    IX86_ATTR_ISA ("avx10.2-512", OPT_mavx10_2_512),
    IX86_ATTR_ISA ("amx-avx512", OPT_mamx_avx512),
    IX86_ATTR_ISA ("amx-tf32", OPT_mamx_tf32),
    IX86_ATTR_ISA ("amx-transpose", OPT_mamx_transpose),
    IX86_ATTR_ISA ("amx-fp8", OPT_mamx_fp8),
    IX86_ATTR_ISA ("movrs", OPT_mmovrs),
    IX86_ATTR_ISA ("amx-movrs", OPT_mamx_movrs),

    /* enum options */
    IX86_ATTR_ENUM ("fpmath=",	OPT_mfpmath_),
    IX86_ATTR_ENUM ("prefer-vector-width=", OPT_mprefer_vector_width_),

    /* string options */
    IX86_ATTR_STR ("arch=",	IX86_FUNCTION_SPECIFIC_ARCH),
    IX86_ATTR_STR ("tune=",	IX86_FUNCTION_SPECIFIC_TUNE),

    /* flag options */
    IX86_ATTR_YES ("cld",
		   OPT_mcld,
		   MASK_CLD),

    IX86_ATTR_NO ("fancy-math-387",
		  OPT_mfancy_math_387,
		  MASK_NO_FANCY_MATH_387),

    IX86_ATTR_YES ("ieee-fp",
		   OPT_mieee_fp,
		   MASK_IEEE_FP),

    IX86_ATTR_YES ("inline-all-stringops",
		   OPT_minline_all_stringops,
		   MASK_INLINE_ALL_STRINGOPS),

    IX86_ATTR_YES ("inline-stringops-dynamically",
		   OPT_minline_stringops_dynamically,
		   MASK_INLINE_STRINGOPS_DYNAMICALLY),

    IX86_ATTR_NO ("align-stringops",
		  OPT_mno_align_stringops,
		  MASK_NO_ALIGN_STRINGOPS),

    IX86_ATTR_YES ("recip",
		   OPT_mrecip,
		   MASK_RECIP),

    IX86_ATTR_IX86_YES ("general-regs-only",
			OPT_mgeneral_regs_only,
			OPTION_MASK_GENERAL_REGS_ONLY),

    IX86_ATTR_YES ("relax-cmpxchg-loop",
		   OPT_mrelax_cmpxchg_loop,
		   MASK_RELAX_CMPXCHG_LOOP),
  };

  location_t loc
    = fndecl == NULL ? UNKNOWN_LOCATION : DECL_SOURCE_LOCATION (fndecl);
  const char *attr_name = target_clone_attr ? "target_clone" : "target";

  /* If this is a list, recurse to get the options.  */
  if (TREE_CODE (args) == TREE_LIST)
    {
      for (; args; args = TREE_CHAIN (args))
	if (TREE_VALUE (args)
	    && !ix86_valid_target_attribute_inner_p (fndecl, TREE_VALUE (args),
						     p_strings, opts, opts_set,
						     enum_opts_set,
						     target_clone_attr))
	  ret = false;

      return ret;
    }

  else if (TREE_CODE (args) != STRING_CST)
    {
      error_at (loc, "attribute %qs argument is not a string", attr_name);
      return false;
    }

  /* Handle multiple arguments separated by commas.  */
  next_optstr = ASTRDUP (TREE_STRING_POINTER (args));

  while (next_optstr && *next_optstr != '\0')
    {
      char *p = next_optstr;
      char *orig_p = p;
      char *comma = strchr (next_optstr, ',');
      size_t len, opt_len;
      int opt;
      bool opt_set_p;
      char ch;
      unsigned i;
      enum ix86_opt_type type = ix86_opt_unknown;
      int mask = 0;

      if (comma)
	{
	  *comma = '\0';
	  len = comma - next_optstr;
	  next_optstr = comma + 1;
	}
      else
	{
	  len = strlen (p);
	  next_optstr = NULL;
	}

      /* Recognize no-xxx.  */
      if (len > 3 && p[0] == 'n' && p[1] == 'o' && p[2] == '-')
	{
	  opt_set_p = false;
	  p += 3;
	  len -= 3;
	}
      else
	opt_set_p = true;

      /* Find the option.  */
      ch = *p;
      opt = N_OPTS;
      for (i = 0; i < ARRAY_SIZE (attrs); i++)
	{
	  type = attrs[i].type;
	  opt_len = attrs[i].len;
	  if (ch == attrs[i].string[0]
	      && ((type != ix86_opt_str && type != ix86_opt_enum)
		  ? len == opt_len
		  : len > opt_len)
	      && memcmp (p, attrs[i].string, opt_len) == 0)
	    {
	      opt = attrs[i].opt;
	      mask = attrs[i].mask;
	      break;
	    }
	}

      /* Process the option.  */
      if (opt == N_OPTS)
	{
	  error_at (loc, "attribute %qs argument %qs is unknown",
		    attr_name, orig_p);
	  ret = false;
	}

      else if (type == ix86_opt_isa)
	{
	  struct cl_decoded_option decoded;

	  generate_option (opt, NULL, opt_set_p, CL_TARGET, &decoded);
	  ix86_handle_option (opts, opts_set,
			      &decoded, input_location);
	}

      else if (type == ix86_opt_yes || type == ix86_opt_no)
	{
	  if (type == ix86_opt_no)
	    opt_set_p = !opt_set_p;

	  if (opt_set_p)
	    opts->x_target_flags |= mask;
	  else
	    opts->x_target_flags &= ~mask;
	}

      else if (type == ix86_opt_ix86_yes || type == ix86_opt_ix86_no)
	{
	  if (mask == OPTION_MASK_GENERAL_REGS_ONLY)
	    {
	      if (!opt_set_p)
		{
		  error_at (loc, "pragma or attribute %<target(\"%s\")%> "
			    "does not allow a negated form", p);
		  return false;
		}

	      if (type != ix86_opt_ix86_yes)
		gcc_unreachable ();

	      opts->x_ix86_target_flags |= mask;

	      struct cl_decoded_option decoded;
	      generate_option (opt, NULL, opt_set_p, CL_TARGET,
			       &decoded);
	      ix86_handle_option (opts, opts_set, &decoded,
				  input_location);
	    }
	  else
	    {
	      if (type == ix86_opt_ix86_no)
		opt_set_p = !opt_set_p;

	      if (opt_set_p)
		opts->x_ix86_target_flags |= mask;
	      else
		opts->x_ix86_target_flags &= ~mask;
	    }
	}

      else if (type == ix86_opt_str)
	{
	  if (p_strings[opt])
	    {
	      error_at (loc, "attribute value %qs was already specified "
			"in %qs attribute", orig_p, attr_name);
	      ret = false;
	    }
	  else
	    {
	      p_strings[opt] = xstrdup (p + opt_len);
	      if (opt == IX86_FUNCTION_SPECIFIC_ARCH)
		{
		  /* If arch= is set,  clear all bits in x_ix86_isa_flags,
		     except for ISA_64BIT, ABI_64, ABI_X32, and CODE16
		     and all bits in x_ix86_isa_flags2.  */
		  opts->x_ix86_isa_flags &= (OPTION_MASK_ISA_64BIT
					     | OPTION_MASK_ABI_64
					     | OPTION_MASK_ABI_X32
					     | OPTION_MASK_CODE16);
		  opts->x_ix86_isa_flags_explicit &= (OPTION_MASK_ISA_64BIT
						      | OPTION_MASK_ABI_64
						      | OPTION_MASK_ABI_X32
						      | OPTION_MASK_CODE16);
		  opts->x_ix86_isa_flags2 = 0;
		  opts->x_ix86_isa_flags2_explicit = 0;
		}
	    }
	}

      else if (type == ix86_opt_enum)
	{
	  bool arg_ok;
	  int value;

	  arg_ok = opt_enum_arg_to_value (opt, p + opt_len, &value, CL_TARGET);
	  if (arg_ok)
	    set_option (opts, enum_opts_set, opt, value,
			p + opt_len, DK_UNSPECIFIED, input_location,
			global_dc);
	  else
	    {
	      error_at (loc, "attribute value %qs is unknown in %qs attribute",
			orig_p, attr_name);
	      ret = false;
	    }
	}

      else
	gcc_unreachable ();
    }

  return ret;
}

/* Release allocated strings.  */
static void
release_options_strings (char **option_strings)
{
  /* Free up memory allocated to hold the strings */
  for (unsigned i = 0; i < IX86_FUNCTION_SPECIFIC_MAX; i++)
    free (option_strings[i]);
}

/* Return a TARGET_OPTION_NODE tree of the target options listed or NULL.  */

tree
ix86_valid_target_attribute_tree (tree fndecl, tree args,
				  struct gcc_options *opts,
				  struct gcc_options *opts_set,
				  bool target_clone_attr)
{
  const char *orig_arch_string = opts->x_ix86_arch_string;
  const char *orig_tune_string = opts->x_ix86_tune_string;
  enum fpmath_unit orig_fpmath_set = opts_set->x_ix86_fpmath;
  enum prefer_vector_width orig_pvw_set = opts_set->x_prefer_vector_width_type;
  enum prefer_vector_width orig_ix86_move_max_set
    = opts_set->x_ix86_move_max;
  enum prefer_vector_width orig_ix86_store_max_set
    = opts_set->x_ix86_store_max;
  int orig_tune_defaulted = ix86_tune_defaulted;
  int orig_arch_specified = ix86_arch_specified;
  char *option_strings[IX86_FUNCTION_SPECIFIC_MAX] = { NULL, NULL };
  tree t = NULL_TREE;
  struct cl_target_option *def
    = TREE_TARGET_OPTION (target_option_default_node);
  struct gcc_options enum_opts_set;

  memset (&enum_opts_set, 0, sizeof (enum_opts_set));

  /* Process each of the options on the chain.  */
  if (!ix86_valid_target_attribute_inner_p (fndecl, args, option_strings, opts,
					    opts_set, &enum_opts_set,
					    target_clone_attr))
    return error_mark_node;

  /* AVX10.1-256 will enable only 256 bit AVX512F features by setting all
     AVX512 related ISA flags and not setting EVEX512.  When it is used
     with avx512 related function attribute, we need to enable 512 bit to
     align with the command line behavior.  Manually set EVEX512 for this
     scenario.  */
  if ((def->x_ix86_isa_flags2 & OPTION_MASK_ISA2_AVX10_1_256)
      && (opts->x_ix86_isa_flags & OPTION_MASK_ISA_AVX512F)
      && (opts->x_ix86_isa_flags_explicit & OPTION_MASK_ISA_AVX512F)
      && !(def->x_ix86_isa_flags2_explicit & OPTION_MASK_ISA2_EVEX512)
      && !(opts->x_ix86_isa_flags2_explicit & OPTION_MASK_ISA2_EVEX512))
    opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_EVEX512;

  /* If the changed options are different from the default, rerun
     ix86_option_override_internal, and then save the options away.
     The string options are attribute options, and will be undone
     when we copy the save structure.  */
  if (opts->x_ix86_isa_flags != def->x_ix86_isa_flags
      || opts->x_ix86_isa_flags2 != def->x_ix86_isa_flags2
      || opts->x_target_flags != def->x_target_flags
      || option_strings[IX86_FUNCTION_SPECIFIC_ARCH]
      || option_strings[IX86_FUNCTION_SPECIFIC_TUNE]
      || enum_opts_set.x_ix86_fpmath
      || enum_opts_set.x_prefer_vector_width_type
      || (!(def->x_ix86_isa_flags2_explicit & OPTION_MASK_ISA2_AVX10_1_256)
	  && (opts->x_ix86_isa_flags2_explicit
	      & OPTION_MASK_ISA2_AVX10_1_256)))
    {
      /* If we are using the default tune= or arch=, undo the string assigned,
	 and use the default.  */
      if (option_strings[IX86_FUNCTION_SPECIFIC_ARCH])
	opts->x_ix86_arch_string
	  = ggc_strdup (option_strings[IX86_FUNCTION_SPECIFIC_ARCH]);
      else if (!orig_arch_specified)
	opts->x_ix86_arch_string = NULL;

      if (option_strings[IX86_FUNCTION_SPECIFIC_TUNE])
	opts->x_ix86_tune_string
	  = ggc_strdup (option_strings[IX86_FUNCTION_SPECIFIC_TUNE]);
      /* If we have explicit arch string and no tune string specified, set
	 tune_string to NULL and later it will be overriden by arch_string
	 so target clones can get proper optimization.  */
      else if (option_strings[IX86_FUNCTION_SPECIFIC_ARCH]
	       || orig_tune_defaulted)
	opts->x_ix86_tune_string = NULL;

      /* If fpmath= is not set, and we now have sse2 on 32-bit, use it.  */
      if (enum_opts_set.x_ix86_fpmath)
	opts_set->x_ix86_fpmath = (enum fpmath_unit) 1;
      if (enum_opts_set.x_prefer_vector_width_type)
	opts_set->x_prefer_vector_width_type = (enum prefer_vector_width) 1;

      /* Do any overrides, such as arch=xxx, or tune=xxx support.  */
      bool r = ix86_option_override_internal (false, opts, opts_set);
      if (!r)
	{
	  release_options_strings (option_strings);
	  return error_mark_node;
	}

      /* Add any builtin functions with the new isa if any.  */
      ix86_add_new_builtins (opts->x_ix86_isa_flags, opts->x_ix86_isa_flags2);

      enum excess_precision orig_ix86_excess_precision
	= opts->x_ix86_excess_precision;
      bool orig_ix86_unsafe_math_optimizations
	= opts->x_ix86_unsafe_math_optimizations;
      opts->x_ix86_excess_precision = opts->x_flag_excess_precision;
      opts->x_ix86_unsafe_math_optimizations
	= opts->x_flag_unsafe_math_optimizations;

      /* Save the current options unless we are validating options for
	 #pragma.  */
      t = build_target_option_node (opts, opts_set);

      opts->x_ix86_arch_string = orig_arch_string;
      opts->x_ix86_tune_string = orig_tune_string;
      opts_set->x_ix86_fpmath = orig_fpmath_set;
      opts_set->x_prefer_vector_width_type = orig_pvw_set;
      opts_set->x_ix86_move_max = orig_ix86_move_max_set;
      opts_set->x_ix86_store_max = orig_ix86_store_max_set;
      opts->x_ix86_excess_precision = orig_ix86_excess_precision;
      opts->x_ix86_unsafe_math_optimizations
	= orig_ix86_unsafe_math_optimizations;

      release_options_strings (option_strings);
    }

  return t;
}

static GTY(()) tree target_attribute_cache[3];

/* Hook to validate attribute((target("string"))).  */

bool
ix86_valid_target_attribute_p (tree fndecl,
			       tree ARG_UNUSED (name),
			       tree args,
			       int flags)
{
  struct gcc_options func_options, func_options_set;
  tree new_target, new_optimize;
  bool ret = true;

  /* attribute((target("default"))) does nothing, beyond
     affecting multi-versioning.  */
  if (TREE_VALUE (args)
      && TREE_CODE (TREE_VALUE (args)) == STRING_CST
      && TREE_CHAIN (args) == NULL_TREE
      && strcmp (TREE_STRING_POINTER (TREE_VALUE (args)), "default") == 0)
    return true;

  if ((DECL_FUNCTION_SPECIFIC_TARGET (fndecl) == target_attribute_cache[1]
       || DECL_FUNCTION_SPECIFIC_TARGET (fndecl) == NULL_TREE)
      && (DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl)
	  == target_attribute_cache[2]
	  || DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) == NULL_TREE)
      && simple_cst_list_equal (args, target_attribute_cache[0]))
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = target_attribute_cache[1];
      DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl)
	= target_attribute_cache[2];
      return true;
    }

  tree old_optimize = build_optimization_node (&global_options,
					       &global_options_set);

  /* Get the optimization options of the current function.  */
  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  if (!func_optimize)
    func_optimize = old_optimize;

  /* Init func_options.  */
  memset (&func_options, 0, sizeof (func_options));
  init_options_struct (&func_options, NULL);
  lang_hooks.init_options_struct (&func_options);
  memset (&func_options_set, 0, sizeof (func_options_set));

  cl_optimization_restore (&func_options, &func_options_set,
			   TREE_OPTIMIZATION (func_optimize));

  /* Initialize func_options to the default before its target options can
     be set.  */
  tree old_target = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);
  if (old_target == NULL_TREE)
    old_target = target_option_default_node;
  cl_target_option_restore (&func_options, &func_options_set,
			    TREE_TARGET_OPTION (old_target));

  /* FLAGS == 1 is used for target_clones attribute.  */
  new_target
    = ix86_valid_target_attribute_tree (fndecl, args, &func_options,
					&func_options_set, flags == 1);

  new_optimize = build_optimization_node (&func_options, &func_options_set);

  if (new_target == error_mark_node)
    ret = false;

  else if (new_target)
    {
      if (DECL_FUNCTION_SPECIFIC_TARGET (fndecl) == NULL_TREE
	  && DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) == NULL_TREE)
	{
	  target_attribute_cache[0] = copy_list (args);
	  target_attribute_cache[1] = new_target;
	  target_attribute_cache[2]
	    = old_optimize != new_optimize ? new_optimize : NULL_TREE;
	}

      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;

      if (old_optimize != new_optimize)
	DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;
    }

  return ret;
}

const char *stringop_alg_names[] = {
#define DEF_ALG(alg, name) #name,
#include "stringop.def"
#undef DEF_ALG
};

/* Parse parameter string passed to -mmemcpy-strategy= or -mmemset-strategy=.
   The string is of the following form (or comma separated list of it):

     strategy_alg:max_size:[align|noalign]

   where the full size range for the strategy is either [0, max_size] or
   [min_size, max_size], in which min_size is the max_size + 1 of the
   preceding range.  The last size range must have max_size == -1.

   Examples:

    1.
       -mmemcpy-strategy=libcall:-1:noalign

      this is equivalent to (for known size memcpy) -mstringop-strategy=libcall


   2.
      -mmemset-strategy=rep_8byte:16:noalign,vector_loop:2048:align,libcall:-1:noalign

      This is to tell the compiler to use the following strategy for memset
      1) when the expected size is between [1, 16], use rep_8byte strategy;
      2) when the size is between [17, 2048], use vector_loop;
      3) when the size is > 2048, use libcall.  */

struct stringop_size_range
{
  int max;
  stringop_alg alg;
  bool noalign;
};

static void
ix86_parse_stringop_strategy_string (char *strategy_str, bool is_memset)
{
  const struct stringop_algs *default_algs;
  stringop_size_range input_ranges[MAX_STRINGOP_ALGS];
  char *curr_range_str, *next_range_str;
  const char *opt = is_memset ? "-mmemset_strategy=" : "-mmemcpy_strategy=";
  int i = 0, n = 0;

  if (is_memset)
    default_algs = &ix86_cost->memset[TARGET_64BIT != 0];
  else
    default_algs = &ix86_cost->memcpy[TARGET_64BIT != 0];

  curr_range_str = strategy_str;

  do
    {
      int maxs;
      char alg_name[128];
      char align[16];
      next_range_str = strchr (curr_range_str, ',');
      if (next_range_str)
        *next_range_str++ = '\0';

      if (sscanf (curr_range_str, "%20[^:]:%d:%10s", alg_name, &maxs,
		  align) != 3)
        {
	  error ("wrong argument %qs to option %qs", curr_range_str, opt);
          return;
        }

      if (n > 0 && (maxs < (input_ranges[n - 1].max + 1) && maxs != -1))
        {
	  error ("size ranges of option %qs should be increasing", opt);
          return;
        }

      for (i = 0; i < last_alg; i++)
	if (!strcmp (alg_name, stringop_alg_names[i]))
	  break;

      if (i == last_alg)
        {
	  error ("wrong strategy name %qs specified for option %qs",
		 alg_name, opt);

	  auto_vec <const char *> candidates;
	  for (i = 0; i < last_alg; i++)
	    if ((stringop_alg) i != rep_prefix_8_byte || TARGET_64BIT)
	      candidates.safe_push (stringop_alg_names[i]);

	  char *s;
	  const char *hint
	    = candidates_list_and_hint (alg_name, s, candidates);
	  if (hint)
	    inform (input_location,
		    "valid arguments to %qs are: %s; did you mean %qs?",
		    opt, s, hint);
	  else
	    inform (input_location, "valid arguments to %qs are: %s",
		    opt, s);
	  XDELETEVEC (s);
          return;
        }

      if ((stringop_alg) i == rep_prefix_8_byte
	  && !TARGET_64BIT)
	{
	  /* rep; movq isn't available in 32-bit code.  */
	  error ("strategy name %qs specified for option %qs "
		 "not supported for 32-bit code", alg_name, opt);
	  return;
	}

      input_ranges[n].max = maxs;
      input_ranges[n].alg = (stringop_alg) i;
      if (!strcmp (align, "align"))
        input_ranges[n].noalign = false;
      else if (!strcmp (align, "noalign"))
        input_ranges[n].noalign = true;
      else
        {
	  error ("unknown alignment %qs specified for option %qs", align, opt);
          return;
        }
      n++;
      curr_range_str = next_range_str;
    }
  while (curr_range_str);

  if (input_ranges[n - 1].max != -1)
    {
      error ("the max value for the last size range should be -1"
             " for option %qs", opt);
      return;
    }

  if (n > MAX_STRINGOP_ALGS)
    {
      error ("too many size ranges specified in option %qs", opt);
      return;
    }

  /* Now override the default algs array.  */
  for (i = 0; i < n; i++)
    {
      *const_cast<int *>(&default_algs->size[i].max) = input_ranges[i].max;
      *const_cast<stringop_alg *>(&default_algs->size[i].alg)
          = input_ranges[i].alg;
      *const_cast<int *>(&default_algs->size[i].noalign)
          = input_ranges[i].noalign;
    }
}


/* parse -mtune-ctrl= option. When DUMP is true,
   print the features that are explicitly set.  */

static void
parse_mtune_ctrl_str (struct gcc_options *opts, bool dump)
{
  if (!opts->x_ix86_tune_ctrl_string)
    return;

  char *next_feature_string = NULL;
  char *curr_feature_string = xstrdup (opts->x_ix86_tune_ctrl_string);
  char *orig = curr_feature_string;
  int i;
  do
    {
      bool clear = false;

      next_feature_string = strchr (curr_feature_string, ',');
      if (next_feature_string)
        *next_feature_string++ = '\0';
      if (*curr_feature_string == '^')
        {
          curr_feature_string++;
          clear = true;
        }

      if (!strcmp (curr_feature_string, "use_gather"))
	{
	  ix86_tune_features[X86_TUNE_USE_GATHER_2PARTS] = !clear;
	  ix86_tune_features[X86_TUNE_USE_GATHER_4PARTS] = !clear;
	  ix86_tune_features[X86_TUNE_USE_GATHER_8PARTS] = !clear;
	  if (dump)
	    fprintf (stderr, "Explicitly %s features use_gather_2parts,"
		     " use_gather_4parts, use_gather_8parts\n",
		     clear ? "clear" : "set");

	}
      else if (!strcmp (curr_feature_string, "use_scatter"))
	{
	  ix86_tune_features[X86_TUNE_USE_SCATTER_2PARTS] = !clear;
	  ix86_tune_features[X86_TUNE_USE_SCATTER_4PARTS] = !clear;
	  ix86_tune_features[X86_TUNE_USE_SCATTER_8PARTS] = !clear;
	  if (dump)
	    fprintf (stderr, "Explicitly %s features use_scatter_2parts,"
		     " use_scatter_4parts, use_scatter_8parts\n",
		     clear ? "clear" : "set");
	}
      else
	{
	  for (i = 0; i < X86_TUNE_LAST; i++)
	    {
	      if (!strcmp (curr_feature_string, ix86_tune_feature_names[i]))
		{
		  ix86_tune_features[i] = !clear;
		  if (dump)
		    fprintf (stderr, "Explicitly %s feature %s\n",
			     clear ? "clear" : "set", ix86_tune_feature_names[i]);
		  break;
		}
	    }

	  if (i == X86_TUNE_LAST)
	    error ("unknown parameter to option %<-mtune-ctrl%>: %s",
		   clear ? curr_feature_string - 1 : curr_feature_string);
	}
      curr_feature_string = next_feature_string;
    }
  while (curr_feature_string);
  free (orig);
}

/* Helper function to set ix86_tune_features. IX86_TUNE is the
   processor type.  */

static void
set_ix86_tune_features (struct gcc_options *opts,
			enum processor_type ix86_tune, bool dump)
{
  unsigned HOST_WIDE_INT ix86_tune_mask = HOST_WIDE_INT_1U << ix86_tune;
  int i;

  for (i = 0; i < X86_TUNE_LAST; ++i)
    {
      if (ix86_tune_no_default)
        ix86_tune_features[i] = 0;
      else
	ix86_tune_features[i]
	  = !!(initial_ix86_tune_features[i] & ix86_tune_mask);
    }

  if (dump)
    {
      fprintf (stderr, "List of x86 specific tuning parameter names:\n");
      for (i = 0; i < X86_TUNE_LAST; i++)
        fprintf (stderr, "%s : %s\n", ix86_tune_feature_names[i],
                 ix86_tune_features[i] ? "on" : "off");
    }

  parse_mtune_ctrl_str (opts, dump);
}


/* Default align_* from the processor table.  */

static void
ix86_default_align (struct gcc_options *opts)
{
  /* -falign-foo without argument: supply one.  */
  if (opts->x_flag_align_loops && !opts->x_str_align_loops)
    opts->x_str_align_loops = processor_cost_table[ix86_tune]->align_loop;
  if (opts->x_flag_align_jumps && !opts->x_str_align_jumps)
    opts->x_str_align_jumps = processor_cost_table[ix86_tune]->align_jump;
  if (opts->x_flag_align_labels && !opts->x_str_align_labels)
    opts->x_str_align_labels = processor_cost_table[ix86_tune]->align_label;
  if (opts->x_flag_align_functions && !opts->x_str_align_functions)
    opts->x_str_align_functions = processor_cost_table[ix86_tune]->align_func;
}

#ifndef USE_IX86_FRAME_POINTER
#define USE_IX86_FRAME_POINTER 0
#endif

/* (Re)compute option overrides affected by optimization levels in
   target-specific ways.  */

static void
ix86_recompute_optlev_based_flags (struct gcc_options *opts,
				   struct gcc_options *opts_set)
{
  /* Set the default values for switches whose default depends on TARGET_64BIT
     in case they weren't overwritten by command line options.  */
  if (TARGET_64BIT_P (opts->x_ix86_isa_flags))
    {
      if (opts->x_optimize >= 1)
	SET_OPTION_IF_UNSET (opts, opts_set, flag_omit_frame_pointer,
			     !USE_IX86_FRAME_POINTER);
      if (opts->x_flag_asynchronous_unwind_tables
	  && TARGET_64BIT_MS_ABI)
	SET_OPTION_IF_UNSET (opts, opts_set, flag_unwind_tables, 1);
      if (opts->x_flag_asynchronous_unwind_tables == 2)
	opts->x_flag_unwind_tables
	  = opts->x_flag_asynchronous_unwind_tables = 1;
      if (opts->x_flag_pcc_struct_return == 2)
	opts->x_flag_pcc_struct_return = 0;
    }
  else
    {
      if (opts->x_optimize >= 1)
	  SET_OPTION_IF_UNSET (opts, opts_set, flag_omit_frame_pointer,
			       !(USE_IX86_FRAME_POINTER || opts->x_optimize_size));
      if (opts->x_flag_asynchronous_unwind_tables == 2)
	opts->x_flag_asynchronous_unwind_tables = !USE_IX86_FRAME_POINTER;
      if (opts->x_flag_pcc_struct_return == 2)
	{
	  /* Intel MCU psABI specifies that -freg-struct-return should
	     be on.  Instead of setting DEFAULT_PCC_STRUCT_RETURN to 0,
	     we check -miamcu so that -freg-struct-return is always
	     turned on if -miamcu is used.  */
	  if (TARGET_IAMCU_P (opts->x_target_flags))
	    opts->x_flag_pcc_struct_return = 0;
	  else
	    opts->x_flag_pcc_struct_return = DEFAULT_PCC_STRUCT_RETURN;
	}
    }

  /* Keep nonleaf frame pointers.  */
  if (opts->x_flag_omit_frame_pointer)
    opts->x_target_flags &= ~MASK_OMIT_LEAF_FRAME_POINTER;
  else if (TARGET_OMIT_LEAF_FRAME_POINTER_P (opts->x_target_flags))
    opts->x_flag_omit_frame_pointer = 1;
}

/* Implement part of TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE hook.  */

static void
ix86_override_options_after_change_1 (struct gcc_options *opts,
				      struct gcc_options *opts_set)
{
#define OPTS_SET_P(OPTION) opts_set->x_ ## OPTION
#define OPTS(OPTION) opts->x_ ## OPTION

  /* Disable unrolling small loops when there's explicit
     -f{,no}unroll-loop.  */
  if ((OPTS_SET_P (flag_unroll_loops))
     || (OPTS_SET_P (flag_unroll_all_loops)
	 && OPTS (flag_unroll_all_loops)))
    {
      if (!OPTS_SET_P (ix86_unroll_only_small_loops))
	OPTS (ix86_unroll_only_small_loops) = 0;
      /* Re-enable -frename-registers and -fweb if funroll-loops
	 enabled.  */
      if (!OPTS_SET_P (flag_web))
	OPTS (flag_web) = OPTS (flag_unroll_loops);
      if (!OPTS_SET_P (flag_rename_registers))
	OPTS (flag_rename_registers) = OPTS (flag_unroll_loops);
      /* -fcunroll-grow-size default follws -f[no]-unroll-loops.  */
      if (!OPTS_SET_P (flag_cunroll_grow_size))
	OPTS (flag_cunroll_grow_size)
	  = (OPTS (flag_unroll_loops)
	     || OPTS (flag_peel_loops)
	     || OPTS (optimize) >= 3);
    }
  else
    {
      if (!OPTS_SET_P (flag_cunroll_grow_size))
	OPTS (flag_cunroll_grow_size)
	  = (OPTS (flag_peel_loops)
	     || OPTS (optimize) >= 3);
    }

#undef OPTS
#undef OPTS_SET_P
}

/* Implement TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE hook.  */

void
ix86_override_options_after_change (void)
{
  ix86_default_align (&global_options);

  ix86_recompute_optlev_based_flags (&global_options, &global_options_set);

  ix86_override_options_after_change_1 (&global_options, &global_options_set);
}

/* Clear stack slot assignments remembered from previous functions.
   This is called from INIT_EXPANDERS once before RTL is emitted for each
   function.  */

static struct machine_function *
ix86_init_machine_status (void)
{
  struct machine_function *f;

  f = ggc_cleared_alloc<machine_function> ();
  f->call_abi = ix86_abi;
  f->stack_frame_required = true;
  f->silent_p = true;

  return f;
}

/* Override various settings based on options.  If MAIN_ARGS_P, the
   options are from the command line, otherwise they are from
   attributes.  Return true if there's an error related to march
   option.  */

static bool
ix86_option_override_internal (bool main_args_p,
			       struct gcc_options *opts,
			       struct gcc_options *opts_set)
{
  unsigned int i;
  unsigned HOST_WIDE_INT ix86_arch_mask, avx512_isa_flags, avx512_isa_flags2;
  const bool ix86_tune_specified = (opts->x_ix86_tune_string != NULL);

  /* -mrecip options.  */
  static struct
    {
      const char *string;           /* option name */
      unsigned int mask;            /* mask bits to set */
    }
  const recip_options[] =
    {
      { "all",       RECIP_MASK_ALL },
      { "none",      RECIP_MASK_NONE },
      { "div",       RECIP_MASK_DIV },
      { "sqrt",      RECIP_MASK_SQRT },
      { "vec-div",   RECIP_MASK_VEC_DIV },
      { "vec-sqrt",  RECIP_MASK_VEC_SQRT },
    };

  avx512_isa_flags = OPTION_MASK_ISA_AVX512F | OPTION_MASK_ISA_AVX512CD
    | OPTION_MASK_ISA_AVX512DQ | OPTION_MASK_ISA_AVX512BW
    | OPTION_MASK_ISA_AVX512VL | OPTION_MASK_ISA_AVX512IFMA
    | OPTION_MASK_ISA_AVX512VBMI | OPTION_MASK_ISA_AVX512VBMI2
    | OPTION_MASK_ISA_AVX512VNNI | OPTION_MASK_ISA_AVX512VPOPCNTDQ
    | OPTION_MASK_ISA_AVX512BITALG;
  avx512_isa_flags2 = OPTION_MASK_ISA2_AVX512FP16
    | OPTION_MASK_ISA2_AVX512BF16;

  /* Turn off both OPTION_MASK_ABI_64 and OPTION_MASK_ABI_X32 if
     TARGET_64BIT_DEFAULT is true and TARGET_64BIT is false.  */
  if (TARGET_64BIT_DEFAULT && !TARGET_64BIT_P (opts->x_ix86_isa_flags))
    opts->x_ix86_isa_flags &= ~(OPTION_MASK_ABI_64 | OPTION_MASK_ABI_X32);
#ifdef TARGET_BI_ARCH
  else
    {
#if TARGET_BI_ARCH == 1
      /* When TARGET_BI_ARCH == 1, by default, OPTION_MASK_ABI_64
	 is on and OPTION_MASK_ABI_X32 is off.  We turn off
	 OPTION_MASK_ABI_64 if OPTION_MASK_ABI_X32 is turned on by
	 -mx32.  */
      if (TARGET_X32_P (opts->x_ix86_isa_flags))
	opts->x_ix86_isa_flags &= ~OPTION_MASK_ABI_64;
#else
      /* When TARGET_BI_ARCH == 2, by default, OPTION_MASK_ABI_X32 is
	 on and OPTION_MASK_ABI_64 is off.  We turn off
	 OPTION_MASK_ABI_X32 if OPTION_MASK_ABI_64 is turned on by
	 -m64 or OPTION_MASK_CODE16 is turned on by -m16.  */
      if (TARGET_LP64_P (opts->x_ix86_isa_flags)
	  || TARGET_16BIT_P (opts->x_ix86_isa_flags))
	opts->x_ix86_isa_flags &= ~OPTION_MASK_ABI_X32;
#endif
      if (TARGET_64BIT_P (opts->x_ix86_isa_flags)
	  && TARGET_IAMCU_P (opts->x_target_flags))
	sorry ("Intel MCU psABI isn%'t supported in %s mode",
	       TARGET_X32_P (opts->x_ix86_isa_flags) ? "x32" : "64-bit");
    }
#endif

  if (TARGET_X32_P (opts->x_ix86_isa_flags))
    {
      /* Always turn on OPTION_MASK_ISA_64BIT and turn off
	 OPTION_MASK_ABI_64 for TARGET_X32.  */
      opts->x_ix86_isa_flags |= OPTION_MASK_ISA_64BIT;
      opts->x_ix86_isa_flags &= ~OPTION_MASK_ABI_64;
    }
  else if (TARGET_16BIT_P (opts->x_ix86_isa_flags))
    opts->x_ix86_isa_flags &= ~(OPTION_MASK_ISA_64BIT
				| OPTION_MASK_ABI_X32
				| OPTION_MASK_ABI_64);
  else if (TARGET_LP64_P (opts->x_ix86_isa_flags))
    {
      /* Always turn on OPTION_MASK_ISA_64BIT and turn off
	 OPTION_MASK_ABI_X32 for TARGET_LP64.  */
      opts->x_ix86_isa_flags |= OPTION_MASK_ISA_64BIT;
      opts->x_ix86_isa_flags &= ~OPTION_MASK_ABI_X32;
    }

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

#ifdef SUBSUBTARGET_OVERRIDE_OPTIONS
  SUBSUBTARGET_OVERRIDE_OPTIONS;
#endif

#ifdef HAVE_LD_BROKEN_PE_DWARF5
  /* If the PE linker has broken DWARF 5 support, make
     DWARF 4 the default.  */
  if (TARGET_PECOFF)
    SET_OPTION_IF_UNSET (opts, opts_set, dwarf_version, 4);
#endif

  /* -fPIC is the default for x86_64.  */
  if (TARGET_MACHO && TARGET_64BIT_P (opts->x_ix86_isa_flags))
    opts->x_flag_pic = 2;

  /* Need to check -mtune=generic first.  */
  if (opts->x_ix86_tune_string)
    {
      /* As special support for cross compilers we read -mtune=native
	     as -mtune=generic.  With native compilers we won't see the
	     -mtune=native, as it was changed by the driver.  */
      if (!strcmp (opts->x_ix86_tune_string, "native"))
	opts->x_ix86_tune_string = "generic";
      else if (!strcmp (opts->x_ix86_tune_string, "x86-64"))
        warning (OPT_Wdeprecated,
		 main_args_p
		 ? G_("%<-mtune=x86-64%> is deprecated; use %<-mtune=k8%> "
		      "or %<-mtune=generic%> instead as appropriate")
		 : G_("%<target(\"tune=x86-64\")%> is deprecated; use "
		      "%<target(\"tune=k8\")%> or %<target(\"tune=generic\")%>"
		      " instead as appropriate"));
    }
  else
    {
      if (opts->x_ix86_arch_string)
	opts->x_ix86_tune_string = opts->x_ix86_arch_string;
      if (!opts->x_ix86_tune_string)
	{
	  opts->x_ix86_tune_string = processor_names[TARGET_CPU_DEFAULT];
	  ix86_tune_defaulted = 1;
	}

      /* opts->x_ix86_tune_string is set to opts->x_ix86_arch_string
	 or defaulted.  We need to use a sensible tune option.  */
      if (startswith (opts->x_ix86_tune_string, "x86-64")
	  && (opts->x_ix86_tune_string[6] == '\0'
	      || (!strcmp (opts->x_ix86_tune_string + 6, "-v2")
		  || !strcmp (opts->x_ix86_tune_string + 6, "-v3")
		  || !strcmp (opts->x_ix86_tune_string + 6, "-v4"))))
	opts->x_ix86_tune_string = "generic";
    }

  if (opts->x_ix86_stringop_alg == rep_prefix_8_byte
      && !TARGET_64BIT_P (opts->x_ix86_isa_flags))
    {
      /* rep; movq isn't available in 32-bit code.  */
      error ("%<-mstringop-strategy=rep_8byte%> not supported for 32-bit code");
      opts->x_ix86_stringop_alg = no_stringop;
    }

  if (TARGET_APX_F_P (opts->x_ix86_isa_flags2)
      && !TARGET_64BIT_P (opts->x_ix86_isa_flags))
    error ("%<-mapxf%> is not supported for 32-bit code");
  else if (opts->x_ix86_apx_features != apx_none
	   && !TARGET_64BIT_P (opts->x_ix86_isa_flags))
    error ("%<-mapx-features=%> option is not supported for 32-bit code");

  if (TARGET_UINTR_P (opts->x_ix86_isa_flags2)
      && !TARGET_64BIT_P (opts->x_ix86_isa_flags))
    error ("%<-muintr%> not supported for 32-bit code");

  if (ix86_lam_type && !TARGET_LP64_P (opts->x_ix86_isa_flags))
    error ("%<-mlam=%> option: [u48|u57] not supported for 32-bit code");

  if (!opts->x_ix86_arch_string)
    opts->x_ix86_arch_string
      = TARGET_64BIT_P (opts->x_ix86_isa_flags)
	? "x86-64" : SUBTARGET32_DEFAULT_CPU;
  else
    ix86_arch_specified = 1;

  if (opts_set->x_ix86_pmode)
    {
      if ((TARGET_LP64_P (opts->x_ix86_isa_flags)
	   && opts->x_ix86_pmode == PMODE_SI)
	  || (!TARGET_64BIT_P (opts->x_ix86_isa_flags)
	       && opts->x_ix86_pmode == PMODE_DI))
	error ("address mode %qs not supported in the %s bit mode",
	       TARGET_64BIT_P (opts->x_ix86_isa_flags) ? "short" : "long",
	       TARGET_64BIT_P (opts->x_ix86_isa_flags) ? "64" : "32");
    }
  else
    opts->x_ix86_pmode = TARGET_LP64_P (opts->x_ix86_isa_flags)
			 ? PMODE_DI : PMODE_SI;

  SET_OPTION_IF_UNSET (opts, opts_set, ix86_abi, DEFAULT_ABI);

  if (opts->x_ix86_abi == MS_ABI && TARGET_X32_P (opts->x_ix86_isa_flags))
    error ("%<-mabi=ms%> not supported with X32 ABI");
  gcc_assert (opts->x_ix86_abi == SYSV_ABI || opts->x_ix86_abi == MS_ABI);

  const char *abi_name = opts->x_ix86_abi == MS_ABI ? "ms" : "sysv";
  if ((opts->x_flag_sanitize & SANITIZE_USER_ADDRESS)
      && opts->x_ix86_abi != DEFAULT_ABI)
    error ("%<-mabi=%s%> not supported with %<-fsanitize=address%>", abi_name);
  if ((opts->x_flag_sanitize & SANITIZE_KERNEL_ADDRESS)
      && opts->x_ix86_abi != DEFAULT_ABI)
    error ("%<-mabi=%s%> not supported with %<-fsanitize=kernel-address%>",
	   abi_name);
  if ((opts->x_flag_sanitize & SANITIZE_THREAD)
      && opts->x_ix86_abi != DEFAULT_ABI)
    error ("%<-mabi=%s%> not supported with %<-fsanitize=thread%>", abi_name);

  /* Hwasan is supported with lam_u57 only.  */
  if (opts->x_flag_sanitize & SANITIZE_HWADDRESS)
    {
      if (ix86_lam_type == lam_u48)
	warning (0, "%<-mlam=u48%> is not compatible with Hardware-assisted "
		 "AddressSanitizer, override to %<-mlam=u57%>");
      ix86_lam_type = lam_u57;
    }

  /* For targets using ms ABI enable ms-extensions, if not
     explicit turned off.  For non-ms ABI we turn off this
     option.  */
  SET_OPTION_IF_UNSET (opts, opts_set, flag_ms_extensions,
		       (MS_ABI == DEFAULT_ABI));

  if (opts_set->x_ix86_cmodel)
    {
      switch (opts->x_ix86_cmodel)
	{
	case CM_SMALL:
	case CM_SMALL_PIC:
	  if (opts->x_flag_pic)
	    opts->x_ix86_cmodel = CM_SMALL_PIC;
	  if (!TARGET_64BIT_P (opts->x_ix86_isa_flags))
	    error ("code model %qs not supported in the %s bit mode",
		   "small", "32");
	  break;

	case CM_MEDIUM:
	case CM_MEDIUM_PIC:
	  if (opts->x_flag_pic)
	    opts->x_ix86_cmodel = CM_MEDIUM_PIC;
	  if (!TARGET_64BIT_P (opts->x_ix86_isa_flags))
	    error ("code model %qs not supported in the %s bit mode",
		   "medium", "32");
	  else if (TARGET_X32_P (opts->x_ix86_isa_flags))
	    error ("code model %qs not supported in x32 mode",
		   "medium");
	  break;

	case CM_LARGE:
	case CM_LARGE_PIC:
	  if (opts->x_flag_pic)
	    opts->x_ix86_cmodel = CM_LARGE_PIC;
	  if (!TARGET_64BIT_P (opts->x_ix86_isa_flags))
	    error ("code model %qs not supported in the %s bit mode",
		   "large", "32");
	  else if (TARGET_X32_P (opts->x_ix86_isa_flags))
	    error ("code model %qs not supported in x32 mode",
		   "large");
	  break;

	case CM_32:
	  if (opts->x_flag_pic)
	    error ("code model %s does not support PIC mode", "32");
	  if (TARGET_64BIT_P (opts->x_ix86_isa_flags))
	    error ("code model %qs not supported in the %s bit mode",
		   "32", "64");
	  break;

	case CM_KERNEL:
	  if (opts->x_flag_pic)
	    {
	      error ("code model %s does not support PIC mode", "kernel");
	      opts->x_ix86_cmodel = CM_32;
	    }
	  if (!TARGET_64BIT_P (opts->x_ix86_isa_flags))
	    error ("code model %qs not supported in the %s bit mode",
		   "kernel", "32");
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      /* For TARGET_64BIT and MS_ABI, force pic on, in order to enable the
	 use of rip-relative addressing.  This eliminates fixups that
	 would otherwise be needed if this object is to be placed in a
	 DLL, and is essentially just as efficient as direct addressing.  */
      if (TARGET_64BIT_P (opts->x_ix86_isa_flags)
	  && (TARGET_RDOS || TARGET_PECOFF))
	opts->x_ix86_cmodel = CM_MEDIUM_PIC, opts->x_flag_pic = 1;
      else if (TARGET_64BIT_P (opts->x_ix86_isa_flags))
	opts->x_ix86_cmodel = opts->x_flag_pic ? CM_SMALL_PIC : CM_SMALL;
      else
	opts->x_ix86_cmodel = CM_32;
    }
  if (TARGET_MACHO && opts->x_ix86_asm_dialect == ASM_INTEL)
    {
      error ("%<-masm=intel%> not supported in this configuration");
      opts->x_ix86_asm_dialect = ASM_ATT;
    }
  if ((TARGET_64BIT_P (opts->x_ix86_isa_flags) != 0)
      != ((opts->x_ix86_isa_flags & OPTION_MASK_ISA_64BIT) != 0))
    sorry ("%i-bit mode not compiled in",
	   (opts->x_ix86_isa_flags & OPTION_MASK_ISA_64BIT) ? 64 : 32);

  /* Last processor_alias_table must point to "generic" entry.  */
  gcc_checking_assert (strcmp (processor_alias_table[pta_size - 1].name,
			       "generic") == 0);
  for (i = 0; i < pta_size; i++)
    if (! strcmp (opts->x_ix86_arch_string, processor_alias_table[i].name))
      {
	if (!strcmp (opts->x_ix86_arch_string, "generic"))
	  {
	    error (main_args_p
		   ? G_("%<generic%> CPU can be used only for %<-mtune=%> "
			"switch")
		   : G_("%<generic%> CPU can be used only for "
			"%<target(\"tune=\")%> attribute"));
	    return false;
	  }
	else if (!strcmp (opts->x_ix86_arch_string, "intel"))
	  {
	    error (main_args_p
		   ? G_("%<intel%> CPU can be used only for %<-mtune=%> "
			"switch")
		   : G_("%<intel%> CPU can be used only for "
			"%<target(\"tune=\")%> attribute"));
	    return false;
	  }

	if (TARGET_64BIT_P (opts->x_ix86_isa_flags)
	    && !((processor_alias_table[i].flags & PTA_64BIT) != 0))
	  {
	    error ("CPU you selected does not support x86-64 "
		   "instruction set");
	    return false;
	  }

	ix86_schedule = processor_alias_table[i].schedule;
	ix86_arch = processor_alias_table[i].processor;

	/* Default cpu tuning to the architecture, unless the table
	   entry requests not to do this.  Used by the x86-64 psABI
	   micro-architecture levels.  */
	if ((processor_alias_table[i].flags & PTA_NO_TUNE) == 0)
	  ix86_tune = ix86_arch;
	else
	  ix86_tune = PROCESSOR_GENERIC;

	/* Enable PTA flags that are enabled by default by a -march option.  */
#define TARGET_EXPLICIT_NO_SAHF_P(opts) (false)
#define SET_TARGET_NO_SAHF(opts) {}
#define TARGET_EXPLICIT_PREFETCH_SSE_P(opts) (false)
#define SET_TARGET_PREFETCH_SSE(opts) {}
#define TARGET_EXPLICIT_NO_TUNE_P(opts) (false)
#define SET_TARGET_NO_TUNE(opts) {}
#define TARGET_EXPLICIT_NO_80387_P(opts) (false)
#define SET_TARGET_NO_80387(opts) {}

#define DEF_PTA(NAME) \
	if (((processor_alias_table[i].flags & PTA_ ## NAME) != 0) \
	    && PTA_ ## NAME != PTA_64BIT \
	    && (TARGET_64BIT || (PTA_ ## NAME != PTA_UINTR \
				 && PTA_ ## NAME != PTA_APX_F))\
	    && !TARGET_EXPLICIT_ ## NAME ## _P (opts)) \
	  SET_TARGET_ ## NAME (opts);
#include "i386-isa.def"
#undef DEF_PTA


       if (!(TARGET_64BIT_P (opts->x_ix86_isa_flags)
	     && ((processor_alias_table[i].flags & PTA_NO_SAHF) != 0))
	   && !TARGET_EXPLICIT_SAHF_P (opts))
	    SET_TARGET_SAHF (opts);

	if (((processor_alias_table[i].flags & PTA_ABM) != 0)
	    && !TARGET_EXPLICIT_ABM_P (opts))
	  {
	    if (!TARGET_EXPLICIT_LZCNT_P (opts))
	      SET_TARGET_LZCNT (opts);
	    if (!TARGET_EXPLICIT_POPCNT_P (opts))
	      SET_TARGET_POPCNT (opts);
	  }

	/* Enable apx if apxf or apx_features are not
	   explicitly set for -march.  */
	if (TARGET_64BIT_P (opts->x_ix86_isa_flags)
	     && ((processor_alias_table[i].flags & PTA_APX_F) != 0)
	     && !TARGET_EXPLICIT_APX_F_P (opts)
	     && !OPTION_SET_P (ix86_apx_features))
	  opts->x_ix86_apx_features = apx_all;

	if ((processor_alias_table[i].flags
	   & (PTA_PREFETCH_SSE | PTA_SSE)) != 0)
	  ix86_prefetch_sse = true;

	/* Don't enable x87 instructions if only general registers are
	   allowed by target("general-regs-only") function attribute or
	   -mgeneral-regs-only.  */
	if (!(opts->x_ix86_target_flags & OPTION_MASK_GENERAL_REGS_ONLY)
	    && !(opts_set->x_target_flags & MASK_80387))
	  {
	    if (((processor_alias_table[i].flags & PTA_NO_80387) != 0))
	      opts->x_target_flags &= ~MASK_80387;
	    else
	      opts->x_target_flags |= MASK_80387;
	  }
	break;
      }

  if (i == pta_size)
    {
      error (main_args_p
	     ? G_("bad value %qs for %<-march=%> switch")
	     : G_("bad value %qs for %<target(\"arch=\")%> attribute"),
	     opts->x_ix86_arch_string);

      auto_vec <const char *> candidates;
      for (i = 0; i < pta_size; i++)
	if (strcmp (processor_alias_table[i].name, "generic")
	    && strcmp (processor_alias_table[i].name, "intel")
	    && (!TARGET_64BIT_P (opts->x_ix86_isa_flags)
		|| ((processor_alias_table[i].flags & PTA_64BIT) != 0)))
	  candidates.safe_push (processor_alias_table[i].name);

#ifdef HAVE_LOCAL_CPU_DETECT
      /* Add also "native" as possible value.  */
      candidates.safe_push ("native");
#endif

      char *s;
      const char *hint
	= candidates_list_and_hint (opts->x_ix86_arch_string, s, candidates);
      if (hint)
	inform (input_location,
		main_args_p
		? G_("valid arguments to %<-march=%> switch are: "
		     "%s; did you mean %qs?")
		: G_("valid arguments to %<target(\"arch=\")%> attribute are: "
		     "%s; did you mean %qs?"), s, hint);
      else
	inform (input_location,
		main_args_p
		? G_("valid arguments to %<-march=%> switch are: %s")
		: G_("valid arguments to %<target(\"arch=\")%> attribute "
		     "are: %s"), s);
      XDELETEVEC (s);
    }

  ix86_arch_mask = HOST_WIDE_INT_1U << ix86_arch;
  for (i = 0; i < X86_ARCH_LAST; ++i)
    ix86_arch_features[i] = !!(initial_ix86_arch_features[i] & ix86_arch_mask);

  for (i = 0; i < pta_size; i++)
    if (! strcmp (opts->x_ix86_tune_string, processor_alias_table[i].name)
	&& (processor_alias_table[i].flags & PTA_NO_TUNE) == 0)
      {
	ix86_schedule = processor_alias_table[i].schedule;
	ix86_tune = processor_alias_table[i].processor;
	if (TARGET_64BIT_P (opts->x_ix86_isa_flags))
	  {
	    if (!((processor_alias_table[i].flags & PTA_64BIT) != 0))
	      {
		if (ix86_tune_defaulted)
		  {
		    opts->x_ix86_tune_string = "x86-64";
		    for (i = 0; i < pta_size; i++)
		      if (! strcmp (opts->x_ix86_tune_string,
				    processor_alias_table[i].name))
			break;
		    ix86_schedule = processor_alias_table[i].schedule;
		    ix86_tune = processor_alias_table[i].processor;
		  }
		else
		  error ("CPU you selected does not support x86-64 "
			 "instruction set");
	      }
	  }
	/* Intel CPUs have always interpreted SSE prefetch instructions as
	   NOPs; so, we can enable SSE prefetch instructions even when
	   -mtune (rather than -march) points us to a processor that has them.
	   However, the VIA C3 gives a SIGILL, so we only do that for i686 and
	   higher processors.  */
	if (TARGET_CMOV
	    && ((processor_alias_table[i].flags
	      & (PTA_PREFETCH_SSE | PTA_SSE)) != 0))
	  ix86_prefetch_sse = true;
	break;
      }

  if (ix86_tune_specified && i == pta_size)
    {
      error (main_args_p
	     ? G_("bad value %qs for %<-mtune=%> switch")
	     : G_("bad value %qs for %<target(\"tune=\")%> attribute"),
	     opts->x_ix86_tune_string);

      auto_vec <const char *> candidates;
      for (i = 0; i < pta_size; i++)
	if ((!TARGET_64BIT_P (opts->x_ix86_isa_flags)
	     || ((processor_alias_table[i].flags & PTA_64BIT) != 0))
	    && (processor_alias_table[i].flags & PTA_NO_TUNE) == 0)
	  candidates.safe_push (processor_alias_table[i].name);

#ifdef HAVE_LOCAL_CPU_DETECT
      /* Add also "native" as possible value.  */
      candidates.safe_push ("native");
#endif

      char *s;
      const char *hint
	= candidates_list_and_hint (opts->x_ix86_tune_string, s, candidates);
      if (hint)
	inform (input_location,
		main_args_p
		? G_("valid arguments to %<-mtune=%> switch are: "
		     "%s; did you mean %qs?")
		: G_("valid arguments to %<target(\"tune=\")%> attribute are: "
		     "%s; did you mean %qs?"), s, hint);
      else
	inform (input_location,
		main_args_p
		? G_("valid arguments to %<-mtune=%> switch are: %s")
		: G_("valid arguments to %<target(\"tune=\")%> attribute "
		     "are: %s"), s);
      XDELETEVEC (s);
    }

  set_ix86_tune_features (opts, ix86_tune, opts->x_ix86_dump_tunes);

  ix86_recompute_optlev_based_flags (opts, opts_set);

  ix86_override_options_after_change_1 (opts, opts_set);

  ix86_tune_cost = processor_cost_table[ix86_tune];
  /* TODO: ix86_cost should be chosen at instruction or function granuality
     so for cold code we use size_cost even in !optimize_size compilation.  */
  if (opts->x_optimize_size)
    ix86_cost = &ix86_size_cost;
  else
    ix86_cost = ix86_tune_cost;

  /* Arrange to set up i386_stack_locals for all functions.  */
  init_machine_status = ix86_init_machine_status;

  /* Override APX flag here if ISA bit is set.  */
  if (TARGET_APX_F_P (opts->x_ix86_isa_flags2)
      && !OPTION_SET_P (ix86_apx_features))
    opts->x_ix86_apx_features = apx_all;

  /* Validate -mregparm= value.  */
  if (opts_set->x_ix86_regparm)
    {
      if (TARGET_64BIT_P (opts->x_ix86_isa_flags))
	warning (0, "%<-mregparm%> is ignored in 64-bit mode");
      else if (TARGET_IAMCU_P (opts->x_target_flags))
	warning (0, "%<-mregparm%> is ignored for Intel MCU psABI");
      if (opts->x_ix86_regparm > REGPARM_MAX)
	{
	  error ("%<-mregparm=%d%> is not between 0 and %d",
		 opts->x_ix86_regparm, REGPARM_MAX);
	  opts->x_ix86_regparm = 0;
	}
    }
  if (TARGET_IAMCU_P (opts->x_target_flags)
      || TARGET_64BIT_P (opts->x_ix86_isa_flags))
    opts->x_ix86_regparm = REGPARM_MAX;

  /* Default align_* from the processor table.  */
  ix86_default_align (&global_options);

  /* Provide default for -mbranch-cost= value.  */
  SET_OPTION_IF_UNSET (opts, opts_set, ix86_branch_cost,
		       ix86_tune_cost->branch_cost);

  if (TARGET_64BIT_P (opts->x_ix86_isa_flags))
    {
      opts->x_target_flags
	|= TARGET_SUBTARGET64_DEFAULT & ~opts_set->x_target_flags;

      if (!ix86_arch_specified)
	opts->x_ix86_isa_flags
	  |= TARGET_SUBTARGET64_ISA_DEFAULT & ~opts->x_ix86_isa_flags_explicit;

      if (!TARGET_128BIT_LONG_DOUBLE_P (opts->x_target_flags))
	error ("%<-m96bit-long-double%> is not compatible with this target");

      if (TARGET_RTD_P (opts->x_target_flags))
	warning (0,
		 main_args_p
		 ? G_("%<-mrtd%> is ignored in 64bit mode")
		 : G_("%<target(\"rtd\")%> is ignored in 64bit mode"));
    }
  else
    {
      opts->x_target_flags
	|= TARGET_SUBTARGET32_DEFAULT & ~opts_set->x_target_flags;

      if (!ix86_arch_specified)
        opts->x_ix86_isa_flags
	  |= TARGET_SUBTARGET32_ISA_DEFAULT & ~opts->x_ix86_isa_flags_explicit;

      /* i386 ABI does not specify red zone.  It still makes sense to use it
         when programmer takes care to stack from being destroyed.  */
      if (!(opts_set->x_target_flags & MASK_NO_RED_ZONE))
        opts->x_target_flags |= MASK_NO_RED_ZONE;
    }

  /* If we're doing fast math, we don't care about comparison order
     wrt NaNs.  This lets us use a shorter comparison sequence.  */
  if (opts->x_flag_finite_math_only)
    opts->x_target_flags &= ~MASK_IEEE_FP;

  /* If the architecture always has an FPU, turn off NO_FANCY_MATH_387,
     since the insns won't need emulation.  */
  if (ix86_tune_features [X86_TUNE_ALWAYS_FANCY_MATH_387])
    opts->x_target_flags &= ~MASK_NO_FANCY_MATH_387;

  /* Likewise, if the target doesn't have a 387, or we've specified
     software floating point, don't use 387 inline intrinsics.  */
  if (!TARGET_80387_P (opts->x_target_flags))
    opts->x_target_flags |= MASK_NO_FANCY_MATH_387;

  /* Turn on MMX builtins for -msse.  */
  if (TARGET_SSE_P (opts->x_ix86_isa_flags))
    opts->x_ix86_isa_flags
      |= OPTION_MASK_ISA_MMX & ~opts->x_ix86_isa_flags_explicit;

  /* Enable SSE prefetch.  */
  if (TARGET_SSE_P (opts->x_ix86_isa_flags)
      || (TARGET_PRFCHW_P (opts->x_ix86_isa_flags)
	  && !TARGET_3DNOW_P (opts->x_ix86_isa_flags)))
    ix86_prefetch_sse = true;

  /* Enable mwait/monitor instructions for -msse3.  */
  if (TARGET_SSE3_P (opts->x_ix86_isa_flags))
    opts->x_ix86_isa_flags2
      |= OPTION_MASK_ISA2_MWAIT & ~opts->x_ix86_isa_flags2_explicit;

  /* Enable popcnt instruction for -msse4.2 or -mabm.  */
  if (TARGET_SSE4_2_P (opts->x_ix86_isa_flags)
      || TARGET_ABM_P (opts->x_ix86_isa_flags))
    opts->x_ix86_isa_flags
      |= OPTION_MASK_ISA_POPCNT & ~opts->x_ix86_isa_flags_explicit;

  /* Enable crc32 instruction for -msse4.2.  */
  if (TARGET_SSE4_2_P (opts->x_ix86_isa_flags))
    opts->x_ix86_isa_flags
      |= OPTION_MASK_ISA_CRC32 & ~opts->x_ix86_isa_flags_explicit;

  /* Enable lzcnt instruction for -mabm.  */
  if (TARGET_ABM_P(opts->x_ix86_isa_flags))
    opts->x_ix86_isa_flags
      |= OPTION_MASK_ISA_LZCNT & ~opts->x_ix86_isa_flags_explicit;

  /* Disable BMI, BMI2 and TBM instructions for -m16.  */
  if (TARGET_16BIT_P(opts->x_ix86_isa_flags))
    opts->x_ix86_isa_flags
      &= ~((OPTION_MASK_ISA_BMI | OPTION_MASK_ISA_BMI2 | OPTION_MASK_ISA_TBM)
	   & ~opts->x_ix86_isa_flags_explicit);

  /* Emit a warning if AVX10.1 options is used with AVX512/EVEX512 options except
     for the following option combinations:
     1. Both AVX10.1-512 and AVX512 with 512 bit vector width are enabled with no
	explicit disable on other AVX512 features.
     2. Both AVX10.1-256 and AVX512 w/o 512 bit vector width are enabled with no
	explicit disable on other AVX512 features.
     3. Both AVX10.1 and AVX512 are disabled.  */
  if (TARGET_AVX10_1_512_P (opts->x_ix86_isa_flags2))
    {
      if (opts->x_ix86_no_avx512_explicit
	  && (((~(avx512_isa_flags & opts->x_ix86_isa_flags)
	       & (avx512_isa_flags & opts->x_ix86_isa_flags_explicit)))
	      || ((~((avx512_isa_flags2 | OPTION_MASK_ISA2_EVEX512)
		     & opts->x_ix86_isa_flags2)
		   & ((avx512_isa_flags2 | OPTION_MASK_ISA2_EVEX512)
		      & opts->x_ix86_isa_flags2_explicit)))))
	warning (0, "%<-mno-evex512%> or %<-mno-avx512XXX%> cannot disable "
		    "AVX10 instructions when AVX10.1-512 is available");
    }
  else if (TARGET_AVX10_1_256_P (opts->x_ix86_isa_flags2))
    {
      if (TARGET_EVEX512_P (opts->x_ix86_isa_flags2)
	  && (OPTION_MASK_ISA2_EVEX512 & opts->x_ix86_isa_flags2_explicit))
	{
	  if (!TARGET_AVX512F_P (opts->x_ix86_isa_flags)
	      || !(OPTION_MASK_ISA_AVX512F & opts->x_ix86_isa_flags_explicit))
	    {
	      /* We should not emit 512 bit instructions under AVX10.1-256
		 when EVEX512 is enabled w/o any AVX512 features enabled.
		 Disable EVEX512 bit for this.  */
	      warning (0, "Using %<-mevex512%> without any AVX512 features "
			  "enabled together with AVX10.1 only will not enable "
			  "any AVX512 or AVX10.1-512 features, using 256 as "
			  "max vector size");
	      opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_EVEX512;
	    }
	  else
	    warning (0, "Vector size conflicts between AVX10.1 and AVX512, "
			"using 512 as max vector size");
	}
      else if (TARGET_AVX512F_P (opts->x_ix86_isa_flags)
	       && !(OPTION_MASK_ISA2_EVEX512
		    & opts->x_ix86_isa_flags2_explicit))
	warning (0, "Vector size conflicts between AVX10.1 and AVX512, using "
		    "512 as max vector size");
      else if (opts->x_ix86_no_avx512_explicit
	       && (((~(avx512_isa_flags & opts->x_ix86_isa_flags)
		    & (avx512_isa_flags & opts->x_ix86_isa_flags_explicit)))
		   || ((~(avx512_isa_flags2 & opts->x_ix86_isa_flags2)
			& (avx512_isa_flags2
			   & opts->x_ix86_isa_flags2_explicit)))))
	warning (0, "%<-mno-avx512XXX%> cannot disable AVX10 instructions "
		    "when AVX10 is available");
    }
  else if (TARGET_AVX512F_P (opts->x_ix86_isa_flags)
	   && (OPTION_MASK_ISA_AVX512F & opts->x_ix86_isa_flags_explicit))
    {
      if (opts->x_ix86_no_avx10_1_explicit
	  && ((OPTION_MASK_ISA2_AVX10_1_256 | OPTION_MASK_ISA2_AVX10_1_512)
	      & opts->x_ix86_isa_flags2_explicit))
	{
	  warning (0, "%<-mno-avx10.1, -mno-avx10.1-256, -mno-avx10.1-512%> "
		      "cannot disable AVX512 instructions when "
		      "%<-mavx512XXX%>");
	  /* Reset those unset AVX512 flags set by AVX10 options when AVX10 is
	     disabled.  */
	  if (OPTION_MASK_ISA2_AVX10_1_256 & opts->x_ix86_isa_flags2_explicit)
	    {
	      opts->x_ix86_isa_flags = (~avx512_isa_flags
					& opts->x_ix86_isa_flags)
		| (avx512_isa_flags & opts->x_ix86_isa_flags
		   & opts->x_ix86_isa_flags_explicit);
	      opts->x_ix86_isa_flags2 = (~avx512_isa_flags2
					 & opts->x_ix86_isa_flags2)
		| (avx512_isa_flags2 & opts->x_ix86_isa_flags2
		   & opts->x_ix86_isa_flags2_explicit);
	    }
	}
    }

  /* Set EVEX512 if one of the following conditions meets:
     1. AVX512 is enabled while EVEX512 is not explicitly set/unset.
     2. AVX10.1-512 is enabled.  */
  if (TARGET_AVX10_1_512_P (opts->x_ix86_isa_flags2)
      || (TARGET_AVX512F_P (opts->x_ix86_isa_flags)
	  && !(opts->x_ix86_isa_flags2_explicit & OPTION_MASK_ISA2_EVEX512)))
    opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_EVEX512;

  /* Enable all AVX512 related ISAs when AVX10.1 is enabled.  */
  if (TARGET_AVX10_1_256_P (opts->x_ix86_isa_flags2))
    {
      opts->x_ix86_isa_flags |= avx512_isa_flags;
      opts->x_ix86_isa_flags2 |= avx512_isa_flags2;
    }

  /* Validate -mpreferred-stack-boundary= value or default it to
     PREFERRED_STACK_BOUNDARY_DEFAULT.  */
  ix86_preferred_stack_boundary = PREFERRED_STACK_BOUNDARY_DEFAULT;
  if (opts_set->x_ix86_preferred_stack_boundary_arg)
    {
      int min = TARGET_64BIT_P (opts->x_ix86_isa_flags)? 3 : 2;
      int max = TARGET_SEH ? 4 : 12;

      if (opts->x_ix86_preferred_stack_boundary_arg < min
	  || opts->x_ix86_preferred_stack_boundary_arg > max)
	{
	  if (min == max)
	    error ("%<-mpreferred-stack-boundary%> is not supported "
		   "for this target");
	  else
	    error ("%<-mpreferred-stack-boundary=%d%> is not between %d and %d",
		   opts->x_ix86_preferred_stack_boundary_arg, min, max);
	}
      else
	ix86_preferred_stack_boundary
	  = (1 << opts->x_ix86_preferred_stack_boundary_arg) * BITS_PER_UNIT;
    }

  /* Set the default value for -mstackrealign.  */
  SET_OPTION_IF_UNSET (opts, opts_set, ix86_force_align_arg_pointer,
		       STACK_REALIGN_DEFAULT);

  ix86_default_incoming_stack_boundary = PREFERRED_STACK_BOUNDARY;

  /* Validate -mincoming-stack-boundary= value or default it to
     MIN_STACK_BOUNDARY/PREFERRED_STACK_BOUNDARY.  */
  ix86_incoming_stack_boundary = ix86_default_incoming_stack_boundary;
  if (opts_set->x_ix86_incoming_stack_boundary_arg)
    {
      int min = TARGET_64BIT_P (opts->x_ix86_isa_flags) ? 3 : 2;

      if (opts->x_ix86_incoming_stack_boundary_arg < min
	  || opts->x_ix86_incoming_stack_boundary_arg > 12)
	error ("%<-mincoming-stack-boundary=%d%> is not between %d and 12",
	       opts->x_ix86_incoming_stack_boundary_arg, min);
      else
	{
	  ix86_user_incoming_stack_boundary
	    = (1 << opts->x_ix86_incoming_stack_boundary_arg) * BITS_PER_UNIT;
	  ix86_incoming_stack_boundary
	    = ix86_user_incoming_stack_boundary;
	}
    }

#ifndef NO_PROFILE_COUNTERS
  if (flag_nop_mcount)
    error ("%<-mnop-mcount%> is not compatible with this target");
#endif
  if (flag_nop_mcount && flag_pic)
    error ("%<-mnop-mcount%> is not implemented for %<-fPIC%>");

  /* Accept -msseregparm only if at least SSE support is enabled.  */
  if (TARGET_SSEREGPARM_P (opts->x_target_flags)
      && ! TARGET_SSE_P (opts->x_ix86_isa_flags))
    error (main_args_p
	   ? G_("%<-msseregparm%> used without SSE enabled")
	   : G_("%<target(\"sseregparm\")%> used without SSE enabled"));

  if (opts_set->x_ix86_fpmath)
    {
      if (opts->x_ix86_fpmath & FPMATH_SSE)
	{
	  if (!TARGET_SSE_P (opts->x_ix86_isa_flags))
	    {
	      if (TARGET_80387_P (opts->x_target_flags))
		{
		  warning (0, "SSE instruction set disabled, using 387 arithmetics");
		  opts->x_ix86_fpmath = FPMATH_387;
		}
	    }
	  else if ((opts->x_ix86_fpmath & FPMATH_387)
		   && !TARGET_80387_P (opts->x_target_flags))
	    {
	      warning (0, "387 instruction set disabled, using SSE arithmetics");
	      opts->x_ix86_fpmath = FPMATH_SSE;
	    }
	}
    }
  /* For all chips supporting SSE2, -mfpmath=sse performs better than
     fpmath=387.  The second is however default at many targets since the
     extra 80bit precision of temporaries is considered to be part of ABI.
     Overwrite the default at least for -ffast-math.
     TODO: -mfpmath=both seems to produce same performing code with bit
     smaller binaries.  It is however not clear if register allocation is
     ready for this setting.
     Also -mfpmath=387 is overall a lot more compact (bout 4-5%) than SSE
     codegen.  We may switch to 387 with -ffast-math for size optimized
     functions. */
  else if (fast_math_flags_set_p (&global_options)
	   && TARGET_SSE2_P (opts->x_ix86_isa_flags))
    opts->x_ix86_fpmath = FPMATH_SSE;
  else
    opts->x_ix86_fpmath = TARGET_FPMATH_DEFAULT_P (opts->x_ix86_isa_flags);

  /* Use external vectorized library in vectorizing intrinsics.  */
  if (opts_set->x_ix86_veclibabi_type)
    switch (opts->x_ix86_veclibabi_type)
      {
      case ix86_veclibabi_type_svml:
	ix86_veclib_handler = &ix86_veclibabi_svml;
	break;

      case ix86_veclibabi_type_acml:
	ix86_veclib_handler = &ix86_veclibabi_acml;
	break;

      case ix86_veclibabi_type_aocl:
	ix86_veclib_handler = &ix86_veclibabi_aocl;
	break;

      default:
	gcc_unreachable ();
      }

  if (ix86_tune_features [X86_TUNE_ACCUMULATE_OUTGOING_ARGS]
      && !(opts_set->x_target_flags & MASK_ACCUMULATE_OUTGOING_ARGS))
    opts->x_target_flags |= MASK_ACCUMULATE_OUTGOING_ARGS;

  /* If stack probes are required, the space used for large function
     arguments on the stack must also be probed, so enable
     -maccumulate-outgoing-args so this happens in the prologue.  */
  if (TARGET_STACK_PROBE_P (opts->x_target_flags)
      && !(opts->x_target_flags & MASK_ACCUMULATE_OUTGOING_ARGS))
    {
      if (opts_set->x_target_flags & MASK_ACCUMULATE_OUTGOING_ARGS)
	warning (0,
		 main_args_p
		 ? G_("stack probing requires %<-maccumulate-outgoing-args%> "
		      "for correctness")
		 : G_("stack probing requires "
		      "%<target(\"accumulate-outgoing-args\")%> for "
		      "correctness"));
      opts->x_target_flags |= MASK_ACCUMULATE_OUTGOING_ARGS;
    }

  /* Stack realignment without -maccumulate-outgoing-args requires %ebp,
     so enable -maccumulate-outgoing-args when %ebp is fixed.  */
  if (fixed_regs[BP_REG]
      && !(opts->x_target_flags & MASK_ACCUMULATE_OUTGOING_ARGS))
    {
      if (opts_set->x_target_flags & MASK_ACCUMULATE_OUTGOING_ARGS)
	warning (0,
		 main_args_p
		 ? G_("fixed ebp register requires "
		      "%<-maccumulate-outgoing-args%>")
		 : G_("fixed ebp register requires "
		      "%<target(\"accumulate-outgoing-args\")%>"));
      opts->x_target_flags |= MASK_ACCUMULATE_OUTGOING_ARGS;
    }

  /* Figure out what ASM_GENERATE_INTERNAL_LABEL builds as a prefix.  */
  {
    char *p;
    ASM_GENERATE_INTERNAL_LABEL (internal_label_prefix, "LX", 0);
    p = strchr (internal_label_prefix, 'X');
    internal_label_prefix_len = p - internal_label_prefix;
    *p = '\0';
  }

  /* When scheduling description is not available, disable scheduler pass
     so it won't slow down the compilation and make x87 code slower.  */
  if (!TARGET_SCHEDULE)
    opts->x_flag_schedule_insns_after_reload = opts->x_flag_schedule_insns = 0;

  SET_OPTION_IF_UNSET (opts, opts_set, param_simultaneous_prefetches,
		       ix86_tune_cost->simultaneous_prefetches);
  SET_OPTION_IF_UNSET (opts, opts_set, param_l1_cache_line_size,
		       ix86_tune_cost->prefetch_block);
  SET_OPTION_IF_UNSET (opts, opts_set, param_l1_cache_size,
		       ix86_tune_cost->l1_cache_size);
  SET_OPTION_IF_UNSET (opts, opts_set, param_l2_cache_size,
		       ix86_tune_cost->l2_cache_size);

  /* 64B is the accepted value for these for all x86.  */
  SET_OPTION_IF_UNSET (&global_options, &global_options_set,
		       param_destruct_interfere_size, 64);
  SET_OPTION_IF_UNSET (&global_options, &global_options_set,
		       param_construct_interfere_size, 64);

  /* Enable sw prefetching at -O3 for CPUS that prefetching is helpful.  */
  if (opts->x_flag_prefetch_loop_arrays < 0
      && HAVE_prefetch
      && (opts->x_optimize >= 3 || opts->x_flag_profile_use)
      && !opts->x_optimize_size
      && TARGET_SOFTWARE_PREFETCHING_BENEFICIAL)
    opts->x_flag_prefetch_loop_arrays = 1;

  /* If using typedef char *va_list, signal that __builtin_va_start (&ap, 0)
     can be opts->x_optimized to ap = __builtin_next_arg (0).  */
  if (!TARGET_64BIT_P (opts->x_ix86_isa_flags) && !opts->x_flag_split_stack)
    targetm.expand_builtin_va_start = NULL;

#ifdef USE_IX86_CLD
  /* Use -mcld by default for 32-bit code if configured with --enable-cld.  */
  if (!TARGET_64BIT_P (opts->x_ix86_isa_flags))
    opts->x_target_flags |= MASK_CLD & ~opts_set->x_target_flags;
#endif

  /* Set the default value for -mfentry.  */
  if (!opts_set->x_flag_fentry)
    opts->x_flag_fentry = TARGET_SEH;
  else
    {
      if (!TARGET_64BIT_P (opts->x_ix86_isa_flags) && opts->x_flag_pic
	  && opts->x_flag_fentry)
	sorry ("%<-mfentry%> isn%'t supported for 32-bit in combination "
	       "with %<-fpic%>");
      else if (TARGET_SEH && !opts->x_flag_fentry)
	sorry ("%<-mno-fentry%> isn%'t compatible with SEH");
    }

  if (TARGET_SEH && TARGET_CALL_MS2SYSV_XLOGUES)
    sorry ("%<-mcall-ms2sysv-xlogues%> isn%'t currently supported with SEH");

  if (!(opts_set->x_target_flags & MASK_VZEROUPPER)
      && flag_expensive_optimizations
      && !optimize_size)
    opts->x_target_flags |= MASK_VZEROUPPER;
  if (!(opts_set->x_target_flags & MASK_STV))
    opts->x_target_flags |= MASK_STV;
  /* Disable STV if -mpreferred-stack-boundary={2,3} or
     -mincoming-stack-boundary={2,3} or -mstackrealign - the needed
     stack realignment will be extra cost the pass doesn't take into
     account and the pass can't realign the stack.  */
  if (ix86_preferred_stack_boundary < 128
      || ix86_incoming_stack_boundary < 128
      || opts->x_ix86_force_align_arg_pointer)
    opts->x_target_flags &= ~MASK_STV;
  if (!ix86_tune_features[X86_TUNE_AVX256_UNALIGNED_LOAD_OPTIMAL]
      && !(opts_set->x_target_flags & MASK_AVX256_SPLIT_UNALIGNED_LOAD))
    opts->x_target_flags |= MASK_AVX256_SPLIT_UNALIGNED_LOAD;
  else if (!main_args_p
	   && ix86_tune_features[X86_TUNE_AVX256_UNALIGNED_LOAD_OPTIMAL])
    opts->x_target_flags &= ~MASK_AVX256_SPLIT_UNALIGNED_LOAD;

  if (!ix86_tune_features[X86_TUNE_AVX256_UNALIGNED_STORE_OPTIMAL]
      && !(opts_set->x_target_flags & MASK_AVX256_SPLIT_UNALIGNED_STORE))
    opts->x_target_flags |= MASK_AVX256_SPLIT_UNALIGNED_STORE;
  else if (!main_args_p
	   && ix86_tune_features[X86_TUNE_AVX256_UNALIGNED_STORE_OPTIMAL])
    opts->x_target_flags &= ~MASK_AVX256_SPLIT_UNALIGNED_STORE;

  /* Enable 128-bit AVX instruction generation
     for the auto-vectorizer.  */
  if (ix86_tune_features[X86_TUNE_AVX128_OPTIMAL]
      && (opts_set->x_prefer_vector_width_type == PVW_NONE))
    opts->x_prefer_vector_width_type = PVW_AVX128;

  /* Use 256-bit AVX instruction generation
     in the auto-vectorizer.  */
  if (ix86_tune_features[X86_TUNE_AVX256_OPTIMAL]
      && (opts_set->x_prefer_vector_width_type == PVW_NONE))
    opts->x_prefer_vector_width_type = PVW_AVX256;

  if (opts_set->x_ix86_move_max == PVW_NONE)
    {
      /* Set the maximum number of bits can be moved from memory to
	 memory efficiently.  */
      if (opts_set->x_prefer_vector_width_type != PVW_NONE)
	opts->x_ix86_move_max = opts->x_prefer_vector_width_type;
      else if (ix86_tune_features[X86_TUNE_AVX512_MOVE_BY_PIECES])
	opts->x_ix86_move_max = PVW_AVX512;
      else if (ix86_tune_features[X86_TUNE_AVX256_MOVE_BY_PIECES])
	opts->x_ix86_move_max = PVW_AVX256;
      else
	{
	  opts->x_ix86_move_max = opts->x_prefer_vector_width_type;
	  if (opts_set->x_ix86_move_max == PVW_NONE)
	    {
	      if (TARGET_AVX512F_P (opts->x_ix86_isa_flags)
		  && TARGET_EVEX512_P (opts->x_ix86_isa_flags2))
		opts->x_ix86_move_max = PVW_AVX512;
	      /* Align with vectorizer to avoid potential STLF issue.  */
	      else if (TARGET_AVX_P (opts->x_ix86_isa_flags))
		opts->x_ix86_move_max = PVW_AVX256;
	      else
		opts->x_ix86_move_max = PVW_AVX128;
	    }
	}
    }

  if (opts_set->x_ix86_store_max == PVW_NONE)
    {
      /* Set the maximum number of bits can be stored to memory
	 efficiently.  */
      if (opts_set->x_prefer_vector_width_type != PVW_NONE)
	opts->x_ix86_store_max = opts->x_prefer_vector_width_type;
      else if (ix86_tune_features[X86_TUNE_AVX512_STORE_BY_PIECES])
	opts->x_ix86_store_max = PVW_AVX512;
      else if (ix86_tune_features[X86_TUNE_AVX256_STORE_BY_PIECES])
	opts->x_ix86_store_max = PVW_AVX256;
      else
	{
	  opts->x_ix86_store_max = opts->x_prefer_vector_width_type;
	  if (opts_set->x_ix86_store_max == PVW_NONE)
	    {
	      if (TARGET_AVX512F_P (opts->x_ix86_isa_flags)
		  && TARGET_EVEX512_P (opts->x_ix86_isa_flags2))
		opts->x_ix86_store_max = PVW_AVX512;
	      /* Align with vectorizer to avoid potential STLF issue.  */
	      else if (TARGET_AVX_P (opts->x_ix86_isa_flags))
		opts->x_ix86_store_max = PVW_AVX256;
	      else
		opts->x_ix86_store_max = PVW_AVX128;
	    }
	}
    }

  if (opts->x_ix86_recip_name)
    {
      char *p = ASTRDUP (opts->x_ix86_recip_name);
      char *q;
      unsigned int mask;
      bool invert;

      while ((q = strtok (p, ",")) != NULL)
	{
	  p = NULL;
	  if (*q == '!')
	    {
	      invert = true;
	      q++;
	    }
	  else
	    invert = false;

	  if (!strcmp (q, "default"))
	    mask = RECIP_MASK_ALL;
	  else
	    {
	      for (i = 0; i < ARRAY_SIZE (recip_options); i++)
		if (!strcmp (q, recip_options[i].string))
		  {
		    mask = recip_options[i].mask;
		    break;
		  }

	      if (i == ARRAY_SIZE (recip_options))
		{
		  error ("unknown option for %<-mrecip=%s%>", q);
		  invert = false;
		  mask = RECIP_MASK_NONE;
		}
	    }

	  opts->x_recip_mask_explicit |= mask;
	  if (invert)
	    opts->x_recip_mask &= ~mask;
	  else
	    opts->x_recip_mask |= mask;
	}
    }

  if (TARGET_RECIP_P (opts->x_target_flags))
    opts->x_recip_mask |= RECIP_MASK_ALL & ~opts->x_recip_mask_explicit;
  else if (opts_set->x_target_flags & MASK_RECIP)
    opts->x_recip_mask &= ~(RECIP_MASK_ALL & ~opts->x_recip_mask_explicit);

  /* Default long double to 64-bit for 32-bit Bionic and to __float128
     for 64-bit Bionic.  Also default long double to 64-bit for Intel
     MCU psABI.  */
  if ((TARGET_HAS_BIONIC || TARGET_IAMCU)
      && !(opts_set->x_target_flags
	   & (MASK_LONG_DOUBLE_64 | MASK_LONG_DOUBLE_128)))
    opts->x_target_flags |= (TARGET_64BIT
			     ? MASK_LONG_DOUBLE_128
			     : MASK_LONG_DOUBLE_64);

  /* Only one of them can be active.  */
  gcc_assert ((opts->x_target_flags & MASK_LONG_DOUBLE_64) == 0
	      || (opts->x_target_flags & MASK_LONG_DOUBLE_128) == 0);

  /* Handle stack protector */
  if (!opts_set->x_ix86_stack_protector_guard)
    {
#ifdef TARGET_THREAD_SSP_OFFSET
      if (!TARGET_HAS_BIONIC)
	opts->x_ix86_stack_protector_guard = SSP_TLS;
      else
#endif
	opts->x_ix86_stack_protector_guard = SSP_GLOBAL;
    }

  if (opts_set->x_ix86_stack_protector_guard_offset_str)
    {
      char *endp;
      const char *str = opts->x_ix86_stack_protector_guard_offset_str;

      errno = 0;
      int64_t offset;

#if defined(INT64_T_IS_LONG)
      offset = strtol (str, &endp, 0);
#else
      offset = strtoll (str, &endp, 0);
#endif

      if (!*str || *endp || errno)
	error ("%qs is not a valid number "
	       "in %<-mstack-protector-guard-offset=%>", str);

      if (!IN_RANGE (offset, HOST_WIDE_INT_C (-0x80000000),
		     HOST_WIDE_INT_C (0x7fffffff)))
	error ("%qs is not a valid offset "
	       "in %<-mstack-protector-guard-offset=%>", str);

      opts->x_ix86_stack_protector_guard_offset = offset;
    }
#ifdef TARGET_THREAD_SSP_OFFSET
  else
    opts->x_ix86_stack_protector_guard_offset = TARGET_THREAD_SSP_OFFSET;
#endif

  if (opts_set->x_ix86_stack_protector_guard_reg_str)
    {
      const char *str = opts->x_ix86_stack_protector_guard_reg_str;
      addr_space_t seg = ADDR_SPACE_GENERIC;

      /* Discard optional register prefix.  */
      if (str[0] == '%')
	str++;

      if (strlen (str) == 2 && str[1] == 's')
	{
	  if (str[0] == 'f')
	    seg = ADDR_SPACE_SEG_FS;
	  else if (str[0] == 'g')
	    seg = ADDR_SPACE_SEG_GS;
	}

      if (seg == ADDR_SPACE_GENERIC)
	error ("%qs is not a valid base register "
	       "in %<-mstack-protector-guard-reg=%>",
	       opts->x_ix86_stack_protector_guard_reg_str);

      opts->x_ix86_stack_protector_guard_reg = seg;
    }
  else
    {
      opts->x_ix86_stack_protector_guard_reg = DEFAULT_TLS_SEG_REG;

      /* The kernel uses a different segment register for performance
	 reasons; a system call would not have to trash the userspace
	 segment register, which would be expensive.  */
      if (opts->x_ix86_cmodel == CM_KERNEL)
	opts->x_ix86_stack_protector_guard_reg = ADDR_SPACE_SEG_GS;
    }

  /* Handle -mmemcpy-strategy= and -mmemset-strategy=  */
  if (opts->x_ix86_tune_memcpy_strategy)
    {
      char *str = xstrdup (opts->x_ix86_tune_memcpy_strategy);
      ix86_parse_stringop_strategy_string (str, false);
      free (str);
    }

  if (opts->x_ix86_tune_memset_strategy)
    {
      char *str = xstrdup (opts->x_ix86_tune_memset_strategy);
      ix86_parse_stringop_strategy_string (str, true);
      free (str);
    }

  /* Save the initial options in case the user does function specific
     options.  */
  if (main_args_p)
    {
      opts->x_ix86_excess_precision
	= opts->x_flag_excess_precision;
      opts->x_ix86_unsafe_math_optimizations
	= opts->x_flag_unsafe_math_optimizations;
      target_option_default_node = target_option_current_node
        = build_target_option_node (opts, opts_set);
    }

  const bool cf_okay_p = (TARGET_64BIT || TARGET_CMOV);
  /* When -fhardened, enable -fcf-protection=full, but only when it's
     compatible with this target, and when it wasn't already specified
     on the command line.  */
  if (opts->x_flag_hardened && cf_okay_p)
    {
      if (!opts_set->x_flag_cf_protection)
	opts->x_flag_cf_protection = CF_FULL;
      else if (opts->x_flag_cf_protection != CF_FULL)
	warning_at (UNKNOWN_LOCATION, OPT_Whardened,
		    "%<-fcf-protection=full%> is not enabled by "
		    "%<-fhardened%> because it was specified on the command "
		    "line");
    }

  if (opts->x_flag_cf_protection != CF_NONE)
    {
      if ((opts->x_flag_cf_protection & CF_BRANCH) == CF_BRANCH
	  && !cf_okay_p)
	error ("%<-fcf-protection%> is not compatible with this target");

      opts->x_flag_cf_protection
      = (cf_protection_level) (opts->x_flag_cf_protection | CF_SET);
    }

  if (ix86_tune_features [X86_TUNE_AVOID_512FMA_CHAINS])
    SET_OPTION_IF_UNSET (opts, opts_set, param_avoid_fma_max_bits, 512);
  else if (ix86_tune_features [X86_TUNE_AVOID_256FMA_CHAINS])
    SET_OPTION_IF_UNSET (opts, opts_set, param_avoid_fma_max_bits, 256);
  else if (ix86_tune_features [X86_TUNE_AVOID_128FMA_CHAINS])
    SET_OPTION_IF_UNSET (opts, opts_set, param_avoid_fma_max_bits, 128);

  /* PR86952: jump table usage with retpolines is slow.
     The PR provides some numbers about the slowness.  */
  if (ix86_indirect_branch != indirect_branch_keep)
    SET_OPTION_IF_UNSET (opts, opts_set, flag_jump_tables, 0);

  SET_OPTION_IF_UNSET (opts, opts_set, param_ira_consider_dup_in_all_alts, 0);

  /* Fully masking the main or the epilogue vectorized loop is not
     profitable generally so leave it disabled until we get more
     fine grained control & costing.  */
  SET_OPTION_IF_UNSET (opts, opts_set, param_vect_partial_vector_usage, 0);

  return true;
}

/* Implement the TARGET_OPTION_OVERRIDE hook.  */

void
ix86_option_override (void)
{
  ix86_option_override_internal (true, &global_options, &global_options_set);
}

/* Remember the last target of ix86_set_current_function.  */
static GTY(()) tree ix86_previous_fndecl;

/* Set targets globals to the default (or current #pragma GCC target
   if active).  Invalidate ix86_previous_fndecl cache.  */

void
ix86_reset_previous_fndecl (void)
{
  tree new_tree = target_option_current_node;
  cl_target_option_restore (&global_options, &global_options_set,
			    TREE_TARGET_OPTION (new_tree));
  if (TREE_TARGET_GLOBALS (new_tree))
    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
  else if (new_tree == target_option_default_node)
    restore_target_globals (&default_target_globals);
  else
    TREE_TARGET_GLOBALS (new_tree) = save_target_globals_default_opts ();
  ix86_previous_fndecl = NULL_TREE;
}

/* Add target attribute to SIMD clone NODE if needed.  */

void
ix86_simd_clone_adjust (struct cgraph_node *node)
{
  const char *str = NULL;

  /* Attributes need to be adjusted for definitions, not declarations.  */
  if (!node->definition)
    return;

  gcc_assert (node->decl == cfun->decl);
  switch (node->simdclone->vecsize_mangle)
    {
    case 'b':
      if (!TARGET_SSE2)
	str = "sse2";
      break;
    case 'c':
      if (TARGET_PREFER_AVX128)
	{
	  if (!TARGET_AVX)
	    str = "avx,prefer-vector-width=256";
	  else
	    str = "prefer-vector-width=256";
	}
      else if (!TARGET_AVX)
	str = "avx";
      break;
    case 'd':
      if (TARGET_PREFER_AVX128)
	{
	  if (!TARGET_AVX2)
	    str = "avx2,prefer-vector-width=256";
	  else
	    str = "prefer-vector-width=256";
	}
      else if (!TARGET_AVX2)
	str = "avx2";
      break;
    case 'e':
      if (TARGET_PREFER_AVX256)
	{
	  if (!TARGET_AVX512F || !TARGET_EVEX512)
	    str = "avx512f,evex512,prefer-vector-width=512";
	  else
	    str = "prefer-vector-width=512";
	}
      else if (!TARGET_AVX512F || !TARGET_EVEX512)
	str = "avx512f,evex512";
      break;
    default:
      gcc_unreachable ();
    }
  if (str == NULL)
    return;
  push_cfun (NULL);
  tree args = build_tree_list (NULL_TREE, build_string (strlen (str), str));
  bool ok = ix86_valid_target_attribute_p (node->decl, NULL, args, 0);
  gcc_assert (ok);
  pop_cfun ();
  ix86_reset_previous_fndecl ();
  ix86_set_current_function (node->decl);
}



/* Set the func_type field from the function FNDECL.  */

static void
ix86_set_func_type (tree fndecl)
{
  /* No need to save and restore callee-saved registers for a noreturn
     function with nothrow or compiled with -fno-exceptions unless when
     compiling with -O0 or -Og, except that it interferes with debugging
     of callers.  So that backtrace works for those at least
     in most cases, save the bp register if it is used, because it often
     is used in callers to compute CFA.

     NB: Can't use just TREE_THIS_VOLATILE to check if this is a noreturn
     function.  The local-pure-const pass turns an interrupt function
     into a noreturn function by setting TREE_THIS_VOLATILE.  Normally
     the local-pure-const pass is run after ix86_set_func_type is called.
     When the local-pure-const pass is enabled for LTO, the interrupt
     function is marked with TREE_THIS_VOLATILE in the IR output, which
     leads to the incompatible attribute error in LTO1.  Ignore the
     interrupt function in this case.  */
  enum call_saved_registers_type no_callee_saved_registers
    = TYPE_DEFAULT_CALL_SAVED_REGISTERS;
  if (lookup_attribute ("no_callee_saved_registers",
			TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))
    no_callee_saved_registers = TYPE_NO_CALLEE_SAVED_REGISTERS;
  else if (ix86_noreturn_no_callee_saved_registers
	   && TREE_THIS_VOLATILE (fndecl)
	   && optimize
	   && !optimize_debug
	   && (TREE_NOTHROW (fndecl) || !flag_exceptions)
	   && !lookup_attribute ("interrupt",
				 TYPE_ATTRIBUTES (TREE_TYPE (fndecl)))
	   && !lookup_attribute ("no_caller_saved_registers",
				 TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))
    no_callee_saved_registers = TYPE_NO_CALLEE_SAVED_REGISTERS_EXCEPT_BP;

  if (cfun->machine->func_type == TYPE_UNKNOWN)
    {
      if (lookup_attribute ("interrupt",
			    TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))
	{
	  if (ix86_function_naked (fndecl))
	    error_at (DECL_SOURCE_LOCATION (fndecl),
		      "interrupt and naked attributes are not compatible");

	  if (no_callee_saved_registers)
	    error_at (DECL_SOURCE_LOCATION (fndecl),
		      "%qs and %qs attributes are not compatible",
		      "interrupt", "no_callee_saved_registers");

	  int nargs = 0;
	  for (tree arg = DECL_ARGUMENTS (fndecl);
	       arg;
	       arg = TREE_CHAIN (arg))
	    nargs++;
	  cfun->machine->call_saved_registers
	    = TYPE_NO_CALLER_SAVED_REGISTERS;
	  cfun->machine->func_type
	    = nargs == 2 ? TYPE_EXCEPTION : TYPE_INTERRUPT;

	  ix86_optimize_mode_switching[X86_DIRFLAG] = 1;

	  /* Only dwarf2out.cc can handle -WORD(AP) as a pointer argument.  */
	  if (write_symbols != NO_DEBUG && write_symbols != DWARF2_DEBUG)
	    sorry ("only DWARF debug format is supported for interrupt "
		   "service routine");
	}
      else
	{
	  cfun->machine->func_type = TYPE_NORMAL;
	  if (lookup_attribute ("no_caller_saved_registers",
				TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))
	    cfun->machine->call_saved_registers
	      = TYPE_NO_CALLER_SAVED_REGISTERS;
	  if (no_callee_saved_registers)
	    {
	      if (cfun->machine->call_saved_registers
		  == TYPE_NO_CALLER_SAVED_REGISTERS)
		error_at (DECL_SOURCE_LOCATION (fndecl),
			  "%qs and %qs attributes are not compatible",
			  "no_caller_saved_registers",
			  "no_callee_saved_registers");
	      cfun->machine->call_saved_registers
		= no_callee_saved_registers;
	    }
	}
    }
}

/* Set the indirect_branch_type field from the function FNDECL.  */

static void
ix86_set_indirect_branch_type (tree fndecl)
{
  if (cfun->machine->indirect_branch_type == indirect_branch_unset)
    {
      tree attr = lookup_attribute ("indirect_branch",
				    DECL_ATTRIBUTES (fndecl));
      if (attr != NULL)
	{
	  tree args = TREE_VALUE (attr);
	  if (args == NULL)
	    gcc_unreachable ();
	  tree cst = TREE_VALUE (args);
	  if (strcmp (TREE_STRING_POINTER (cst), "keep") == 0)
	    cfun->machine->indirect_branch_type = indirect_branch_keep;
	  else if (strcmp (TREE_STRING_POINTER (cst), "thunk") == 0)
	    cfun->machine->indirect_branch_type = indirect_branch_thunk;
	  else if (strcmp (TREE_STRING_POINTER (cst), "thunk-inline") == 0)
	    cfun->machine->indirect_branch_type = indirect_branch_thunk_inline;
	  else if (strcmp (TREE_STRING_POINTER (cst), "thunk-extern") == 0)
	    cfun->machine->indirect_branch_type = indirect_branch_thunk_extern;
	  else
	    gcc_unreachable ();
	}
      else
	cfun->machine->indirect_branch_type = ix86_indirect_branch;

      /* -mcmodel=large is not compatible with -mindirect-branch=thunk
	 nor -mindirect-branch=thunk-extern.  */
      if ((ix86_cmodel == CM_LARGE || ix86_cmodel == CM_LARGE_PIC)
	  && ((cfun->machine->indirect_branch_type
	       == indirect_branch_thunk_extern)
	      || (cfun->machine->indirect_branch_type
		  == indirect_branch_thunk)))
	error ("%<-mindirect-branch=%s%> and %<-mcmodel=large%> are not "
	       "compatible",
	       ((cfun->machine->indirect_branch_type
		 == indirect_branch_thunk_extern)
		? "thunk-extern" : "thunk"));

      if (cfun->machine->indirect_branch_type != indirect_branch_keep
	  && (cfun->machine->indirect_branch_type
	      != indirect_branch_thunk_extern)
	  && (flag_cf_protection & CF_RETURN))
	error ("%<-mindirect-branch%> and %<-fcf-protection%> are not "
	       "compatible");
    }

  if (cfun->machine->function_return_type == indirect_branch_unset)
    {
      tree attr = lookup_attribute ("function_return",
				    DECL_ATTRIBUTES (fndecl));
      if (attr != NULL)
	{
	  tree args = TREE_VALUE (attr);
	  if (args == NULL)
	    gcc_unreachable ();
	  tree cst = TREE_VALUE (args);
	  if (strcmp (TREE_STRING_POINTER (cst), "keep") == 0)
	    cfun->machine->function_return_type = indirect_branch_keep;
	  else if (strcmp (TREE_STRING_POINTER (cst), "thunk") == 0)
	    cfun->machine->function_return_type = indirect_branch_thunk;
	  else if (strcmp (TREE_STRING_POINTER (cst), "thunk-inline") == 0)
	    cfun->machine->function_return_type = indirect_branch_thunk_inline;
	  else if (strcmp (TREE_STRING_POINTER (cst), "thunk-extern") == 0)
	    cfun->machine->function_return_type = indirect_branch_thunk_extern;
	  else
	    gcc_unreachable ();
	}
      else
	cfun->machine->function_return_type = ix86_function_return;

      /* -mcmodel=large is not compatible with -mfunction-return=thunk
	 nor -mfunction-return=thunk-extern.  */
      if ((ix86_cmodel == CM_LARGE || ix86_cmodel == CM_LARGE_PIC)
	  && ((cfun->machine->function_return_type
	       == indirect_branch_thunk_extern)
	      || (cfun->machine->function_return_type
		  == indirect_branch_thunk)))
	error ("%<-mfunction-return=%s%> and %<-mcmodel=large%> are not "
	       "compatible",
	       ((cfun->machine->function_return_type
		 == indirect_branch_thunk_extern)
		? "thunk-extern" : "thunk"));

      if (cfun->machine->function_return_type != indirect_branch_keep
	  && (cfun->machine->function_return_type
	      != indirect_branch_thunk_extern)
	  && (flag_cf_protection & CF_RETURN))
	error ("%<-mfunction-return%> and %<-fcf-protection%> are not "
	       "compatible");
    }
}

/* Establish appropriate back-end context for processing the function
   FNDECL.  The argument might be NULL to indicate processing at top
   level, outside of any function scope.  */
void
ix86_set_current_function (tree fndecl)
{
  /* Only change the context if the function changes.  This hook is called
     several times in the course of compiling a function, and we don't want to
     slow things down too much or call target_reinit when it isn't safe.  */
  if (fndecl == ix86_previous_fndecl)
    {
      /* There may be 2 function bodies for the same function FNDECL,
	 one is extern inline and one isn't.  Call ix86_set_func_type
	 to set the func_type field.  */
      if (fndecl != NULL_TREE)
	{
	  ix86_set_func_type (fndecl);
	  ix86_set_indirect_branch_type (fndecl);
	}
      return;
    }

  tree old_tree;
  if (ix86_previous_fndecl == NULL_TREE)
    old_tree = target_option_current_node;
  else if (DECL_FUNCTION_SPECIFIC_TARGET (ix86_previous_fndecl))
    old_tree = DECL_FUNCTION_SPECIFIC_TARGET (ix86_previous_fndecl);
  else
    old_tree = target_option_default_node;

  if (fndecl == NULL_TREE)
    {
      if (old_tree != target_option_current_node)
	ix86_reset_previous_fndecl ();
      return;
    }

  ix86_set_func_type (fndecl);
  ix86_set_indirect_branch_type (fndecl);

  tree new_tree = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);
  if (new_tree == NULL_TREE)
    new_tree = target_option_default_node;

  bool fp_flag_change
    = (flag_unsafe_math_optimizations
       != TREE_TARGET_OPTION (new_tree)->x_ix86_unsafe_math_optimizations
       || (flag_excess_precision
	   != TREE_TARGET_OPTION (new_tree)->x_ix86_excess_precision));
  if (old_tree != new_tree || fp_flag_change)
    {
      cl_target_option_restore (&global_options, &global_options_set,
				TREE_TARGET_OPTION (new_tree));
      if (fp_flag_change)
	{
	  ix86_excess_precision = flag_excess_precision;
	  ix86_unsafe_math_optimizations = flag_unsafe_math_optimizations;
	  DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_tree
	    = build_target_option_node (&global_options, &global_options_set);
	}
      if (TREE_TARGET_GLOBALS (new_tree))
	restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
      else if (new_tree == target_option_default_node)
	restore_target_globals (&default_target_globals);
      else
	TREE_TARGET_GLOBALS (new_tree) = save_target_globals_default_opts ();
    }
  ix86_previous_fndecl = fndecl;

  static call_saved_registers_type prev_call_saved_registers;

  /* 64-bit MS and SYSV ABI have different set of call used registers.
     Avoid expensive re-initialization of init_regs each time we switch
     function context.  */
  if (TARGET_64BIT
      && (call_used_or_fixed_reg_p (SI_REG)
	  == (cfun->machine->call_abi == MS_ABI)))
    reinit_regs ();
  /* Need to re-initialize init_regs if caller-saved registers are
     changed.  */
  else if (prev_call_saved_registers
	   != cfun->machine->call_saved_registers)
    reinit_regs ();

  if (cfun->machine->func_type != TYPE_NORMAL
      || (cfun->machine->call_saved_registers
	  == TYPE_NO_CALLER_SAVED_REGISTERS))
    {
      /* Don't allow SSE, MMX nor x87 instructions since they
	 may change processor state.  */
      const char *isa;
      if (TARGET_SSE)
	isa = "SSE";
      else if (TARGET_MMX)
	isa = "MMX/3Dnow";
      else if (TARGET_80387)
	isa = "80387";
      else
	isa = NULL;
      if (isa != NULL)
	{
	  if (cfun->machine->func_type != TYPE_NORMAL)
	    sorry (cfun->machine->func_type == TYPE_EXCEPTION
		   ? G_("%s instructions aren%'t allowed in an"
			" exception service routine")
		   : G_("%s instructions aren%'t allowed in an"
			" interrupt service routine"),
		   isa);
	  else
	    sorry ("%s instructions aren%'t allowed in a function with "
		   "the %<no_caller_saved_registers%> attribute", isa);
	  /* Don't issue the same error twice.  */
	  cfun->machine->func_type = TYPE_NORMAL;
	  cfun->machine->call_saved_registers
	    = TYPE_DEFAULT_CALL_SAVED_REGISTERS;
	}
    }

  prev_call_saved_registers = cfun->machine->call_saved_registers;
}

/* Implement the TARGET_OFFLOAD_OPTIONS hook.  */
char *
ix86_offload_options (void)
{
  if (TARGET_LP64)
    return xstrdup ("-foffload-abi=lp64 -foffload-abi-host-opts=-m64");
  return xstrdup ("-foffload-abi=ilp32 -foffload-abi-host-opts=-m32");
}

/* Handle "cdecl", "stdcall", "fastcall", "regparm", "thiscall",
   and "sseregparm" calling convention attributes;
   arguments as in struct attribute_spec.handler.  */

static tree
ix86_handle_cconv_attribute (tree *node, tree name, tree args, int,
			     bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != METHOD_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Can combine regparm with all attributes but fastcall, and thiscall.  */
  if (is_attribute_p ("regparm", name))
    {
      tree cst;

      if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and regparm attributes are not compatible");
	}

      if (lookup_attribute ("thiscall", TYPE_ATTRIBUTES (*node)))
	{
	  error ("regparam and thiscall attributes are not compatible");
	}

      cst = TREE_VALUE (args);
      if (TREE_CODE (cst) != INTEGER_CST)
	{
	  warning (OPT_Wattributes,
		   "%qE attribute requires an integer constant argument",
		   name);
	  *no_add_attrs = true;
	}
      else if (compare_tree_int (cst, REGPARM_MAX) > 0)
	{
	  warning (OPT_Wattributes, "argument to %qE attribute larger than %d",
		   name, REGPARM_MAX);
	  *no_add_attrs = true;
	}

      return NULL_TREE;
    }

  if (TARGET_64BIT)
    {
      /* Do not warn when emulating the MS ABI.  */
      if ((TREE_CODE (*node) != FUNCTION_TYPE
	   && TREE_CODE (*node) != METHOD_TYPE)
	  || ix86_function_type_abi (*node) != MS_ABI)
	warning (OPT_Wattributes, "%qE attribute ignored",
	         name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Can combine fastcall with stdcall (redundant) and sseregparm.  */
  if (is_attribute_p ("fastcall", name))
    {
      if (lookup_attribute ("cdecl", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and cdecl attributes are not compatible");
	}
      if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and stdcall attributes are not compatible");
	}
      if (lookup_attribute ("regparm", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and regparm attributes are not compatible");
	}
      if (lookup_attribute ("thiscall", TYPE_ATTRIBUTES (*node)))
	{
	  error ("fastcall and thiscall attributes are not compatible");
	}
    }

  /* Can combine stdcall with fastcall (redundant), regparm and
     sseregparm.  */
  else if (is_attribute_p ("stdcall", name))
    {
      if (lookup_attribute ("cdecl", TYPE_ATTRIBUTES (*node)))
        {
	  error ("stdcall and cdecl attributes are not compatible");
	}
      if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("stdcall and fastcall attributes are not compatible");
	}
      if (lookup_attribute ("thiscall", TYPE_ATTRIBUTES (*node)))
	{
	  error ("stdcall and thiscall attributes are not compatible");
	}
    }

  /* Can combine cdecl with regparm and sseregparm.  */
  else if (is_attribute_p ("cdecl", name))
    {
      if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("stdcall and cdecl attributes are not compatible");
	}
      if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and cdecl attributes are not compatible");
	}
      if (lookup_attribute ("thiscall", TYPE_ATTRIBUTES (*node)))
	{
	  error ("cdecl and thiscall attributes are not compatible");
	}
    }
  else if (is_attribute_p ("thiscall", name))
    {
      if (TREE_CODE (*node) != METHOD_TYPE && pedantic)
	warning (OPT_Wattributes, "%qE attribute is used for non-class method",
	         name);
      if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (*node)))
	{
	  error ("stdcall and thiscall attributes are not compatible");
	}
      if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (*node)))
	{
	  error ("fastcall and thiscall attributes are not compatible");
	}
      if (lookup_attribute ("cdecl", TYPE_ATTRIBUTES (*node)))
	{
	  error ("cdecl and thiscall attributes are not compatible");
	}
    }

  /* Can combine sseregparm with all attributes.  */

  return NULL_TREE;
}

#ifndef CHECK_STACK_LIMIT
#define CHECK_STACK_LIMIT (-1)
#endif

/* The transactional memory builtins are implicitly regparm or fastcall
   depending on the ABI.  Override the generic do-nothing attribute that
   these builtins were declared with, and replace it with one of the two
   attributes that we expect elsewhere.  */

static tree
ix86_handle_tm_regparm_attribute (tree *node, tree, tree,
				  int flags, bool *no_add_attrs)
{
  tree alt;

  /* In no case do we want to add the placeholder attribute.  */
  *no_add_attrs = true;

  /* The 64-bit ABI is unchanged for transactional memory.  */
  if (TARGET_64BIT)
    return NULL_TREE;

  /* ??? Is there a better way to validate 32-bit windows?  We have
     cfun->machine->call_abi, but that seems to be set only for 64-bit.  */
  if (CHECK_STACK_LIMIT > 0)
    alt = tree_cons (get_identifier ("fastcall"), NULL, NULL);
  else
    {
      alt = tree_cons (NULL, build_int_cst (NULL, 2), NULL);
      alt = tree_cons (get_identifier ("regparm"), alt, NULL);
    }
  decl_attributes (node, alt, flags);

  return NULL_TREE;
}

/* Handle a "force_align_arg_pointer" attribute.  */

static tree
ix86_handle_force_align_arg_pointer_attribute (tree *node, tree name,
					       tree, int, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != METHOD_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "ms_struct" or "gcc_struct" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
ix86_handle_struct_attribute (tree *node, tree name, tree, int,
			      bool *no_add_attrs)
{
  tree *type = NULL;
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) == TYPE_DECL)
	type = &TREE_TYPE (*node);
    }
  else
    type = node;

  if (!(type && RECORD_OR_UNION_TYPE_P (*type)))
    {
      warning (OPT_Wattributes, "%qE attribute ignored",
	       name);
      *no_add_attrs = true;
    }

  else if ((is_attribute_p ("ms_struct", name)
	    && lookup_attribute ("gcc_struct", TYPE_ATTRIBUTES (*type)))
	   || ((is_attribute_p ("gcc_struct", name)
		&& lookup_attribute ("ms_struct", TYPE_ATTRIBUTES (*type)))))
    {
      warning (OPT_Wattributes, "%qE incompatible attribute ignored",
               name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "callee_pop_aggregate_return" attribute; arguments as
   in struct attribute_spec handler.  */

static tree
ix86_handle_callee_pop_aggregate_return (tree *node, tree name, tree args, int,
					 bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != METHOD_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
      return NULL_TREE;
    }
  if (TARGET_64BIT)
    {
      warning (OPT_Wattributes, "%qE attribute only available for 32-bit",
	       name);
      *no_add_attrs = true;
      return NULL_TREE;
    }
  if (is_attribute_p ("callee_pop_aggregate_return", name))
    {
      tree cst;

      cst = TREE_VALUE (args);
      if (TREE_CODE (cst) != INTEGER_CST)
	{
	  warning (OPT_Wattributes,
		   "%qE attribute requires an integer constant argument",
		   name);
	  *no_add_attrs = true;
	}
      else if (compare_tree_int (cst, 0) != 0
	       && compare_tree_int (cst, 1) != 0)
	{
	  warning (OPT_Wattributes,
		   "argument to %qE attribute is neither zero, nor one",
		   name);
	  *no_add_attrs = true;
	}

      return NULL_TREE;
    }

  return NULL_TREE;
}

/* Handle a "ms_abi" or "sysv" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
ix86_handle_abi_attribute (tree *node, tree name, tree, int,
			   bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != METHOD_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Can combine regparm with all attributes but fastcall.  */
  if (is_attribute_p ("ms_abi", name))
    {
      if (lookup_attribute ("sysv_abi", TYPE_ATTRIBUTES (*node)))
        {
	  error ("%qs and %qs attributes are not compatible",
		 "ms_abi", "sysv_abi");
	}

      return NULL_TREE;
    }
  else if (is_attribute_p ("sysv_abi", name))
    {
      if (lookup_attribute ("ms_abi", TYPE_ATTRIBUTES (*node)))
        {
	  error ("%qs and %qs attributes are not compatible",
		 "ms_abi", "sysv_abi");
	}

      return NULL_TREE;
    }

  return NULL_TREE;
}

static tree
ix86_handle_fndecl_attribute (tree *node, tree name, tree args, int,
			      bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
               name);
      *no_add_attrs = true;
    }

  if (is_attribute_p ("indirect_branch", name))
    {
      tree cst = TREE_VALUE (args);
      if (TREE_CODE (cst) != STRING_CST)
	{
	  warning (OPT_Wattributes,
		   "%qE attribute requires a string constant argument",
		   name);
	  *no_add_attrs = true;
	}
      else if (strcmp (TREE_STRING_POINTER (cst), "keep") != 0
	       && strcmp (TREE_STRING_POINTER (cst), "thunk") != 0
	       && strcmp (TREE_STRING_POINTER (cst), "thunk-inline") != 0
	       && strcmp (TREE_STRING_POINTER (cst), "thunk-extern") != 0)
	{
	  warning (OPT_Wattributes,
		   "argument to %qE attribute is not "
		   "(keep|thunk|thunk-inline|thunk-extern)", name);
	  *no_add_attrs = true;
	}
    }

  if (is_attribute_p ("function_return", name))
    {
      tree cst = TREE_VALUE (args);
      if (TREE_CODE (cst) != STRING_CST)
	{
	  warning (OPT_Wattributes,
		   "%qE attribute requires a string constant argument",
		   name);
	  *no_add_attrs = true;
	}
      else if (strcmp (TREE_STRING_POINTER (cst), "keep") != 0
	       && strcmp (TREE_STRING_POINTER (cst), "thunk") != 0
	       && strcmp (TREE_STRING_POINTER (cst), "thunk-inline") != 0
	       && strcmp (TREE_STRING_POINTER (cst), "thunk-extern") != 0)
	{
	  warning (OPT_Wattributes,
		   "argument to %qE attribute is not "
		   "(keep|thunk|thunk-inline|thunk-extern)", name);
	  *no_add_attrs = true;
	}
    }

  return NULL_TREE;
}

static tree
ix86_handle_call_saved_registers_attribute (tree *, tree, tree,
					    int, bool *)
{
  return NULL_TREE;
}

static tree
ix86_handle_interrupt_attribute (tree *node, tree, tree, int, bool *)
{
  /* DECL_RESULT and DECL_ARGUMENTS do not exist there yet,
     but the function type contains args and return type data.  */
  tree func_type = *node;
  tree return_type = TREE_TYPE (func_type);

  int nargs = 0;
  tree current_arg_type = TYPE_ARG_TYPES (func_type);
  while (current_arg_type
	 && ! VOID_TYPE_P (TREE_VALUE (current_arg_type)))
    {
      if (nargs == 0)
	{
	  if (! POINTER_TYPE_P (TREE_VALUE (current_arg_type)))
	    error ("interrupt service routine should have a pointer "
		   "as the first argument");
	}
      else if (nargs == 1)
	{
	  if (TREE_CODE (TREE_VALUE (current_arg_type)) != INTEGER_TYPE
	      || TYPE_MODE (TREE_VALUE (current_arg_type)) != word_mode)
	    error ("interrupt service routine should have %qs "
		   "as the second argument",
		   TARGET_64BIT
		   ? (TARGET_X32 ? "unsigned long long int"
				 : "unsigned long int")
		   : "unsigned int");
	}
      nargs++;
      current_arg_type = TREE_CHAIN (current_arg_type);
    }
  if (!nargs || nargs > 2)
    error ("interrupt service routine can only have a pointer argument "
	   "and an optional integer argument");
  if (! VOID_TYPE_P (return_type))
    error ("interrupt service routine must return %<void%>");

  return NULL_TREE;
}

/* Handle fentry_name / fentry_section attribute.  */

static tree
ix86_handle_fentry_name (tree *node, tree name, tree args,
			 int, bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
    /* Do nothing else, just set the attribute.  We'll get at
       it later with lookup_attribute.  */
    ;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "nodirect_extern_access" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nodirect_extern_access_attribute (tree *pnode, tree name,
					 tree ARG_UNUSED (args),
					 int ARG_UNUSED (flags),
					 bool *no_add_attrs)
{
  tree node = *pnode;

  if (VAR_OR_FUNCTION_DECL_P (node))
    {
      if ((!TREE_STATIC (node) && TREE_CODE (node) != FUNCTION_DECL
	   && !DECL_EXTERNAL (node)) || !TREE_PUBLIC (node))
	{
	  warning (OPT_Wattributes,
		   "%qE attribute have effect only on public objects", name);
	  *no_add_attrs = true;
	}
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Table of valid machine attributes.  */
static const attribute_spec ix86_gnu_attributes[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  /* Stdcall attribute says callee is responsible for popping arguments
     if they are not variable.  */
  { "stdcall",   0, 0, false, true,  true,  true, ix86_handle_cconv_attribute,
    NULL },
  /* Fastcall attribute says callee is responsible for popping arguments
     if they are not variable.  */
  { "fastcall",  0, 0, false, true,  true,  true, ix86_handle_cconv_attribute,
    NULL },
  /* Thiscall attribute says callee is responsible for popping arguments
     if they are not variable.  */
  { "thiscall",  0, 0, false, true,  true,  true, ix86_handle_cconv_attribute,
    NULL },
  /* Cdecl attribute says the callee is a normal C declaration */
  { "cdecl",     0, 0, false, true,  true,  true, ix86_handle_cconv_attribute,
    NULL },
  /* Regparm attribute specifies how many integer arguments are to be
     passed in registers.  */
  { "regparm",   1, 1, false, true,  true,  true, ix86_handle_cconv_attribute,
    NULL },
  /* Sseregparm attribute says we are using x86_64 calling conventions
     for FP arguments.  */
  { "sseregparm", 0, 0, false, true, true,  true, ix86_handle_cconv_attribute,
    NULL },
  /* The transactional memory builtins are implicitly regparm or fastcall
     depending on the ABI.  Override the generic do-nothing attribute that
     these builtins were declared with.  */
  { "*tm regparm", 0, 0, false, true, true, true,
    ix86_handle_tm_regparm_attribute, NULL },
  /* force_align_arg_pointer says this function realigns the stack at entry.  */
  { "force_align_arg_pointer", 0, 0,
    false, true,  true, false, ix86_handle_force_align_arg_pointer_attribute,
    NULL },
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
  { "dllimport", 0, 0, false, false, false, false, handle_dll_attribute,
    NULL },
  { "dllexport", 0, 0, false, false, false, false, handle_dll_attribute,
    NULL },
  { "shared",    0, 0, true,  false, false, false,
    ix86_handle_shared_attribute, NULL },
#endif
  { "ms_struct", 0, 0, false, false,  false, false,
    ix86_handle_struct_attribute, NULL },
  { "gcc_struct", 0, 0, false, false,  false, false,
    ix86_handle_struct_attribute, NULL },
#ifdef SUBTARGET_ATTRIBUTE_TABLE
  SUBTARGET_ATTRIBUTE_TABLE,
#endif
  /* ms_abi and sysv_abi calling convention function attributes.  */
  { "ms_abi", 0, 0, false, true, true, true, ix86_handle_abi_attribute, NULL },
  { "sysv_abi", 0, 0, false, true, true, true, ix86_handle_abi_attribute,
    NULL },
  { "ms_abi va_list", 0, 0, false, false, false, false, NULL, NULL },
  { "sysv_abi va_list", 0, 0, false, false, false, false, NULL, NULL },
  { "ms_hook_prologue", 0, 0, true, false, false, false,
    ix86_handle_fndecl_attribute, NULL },
  { "callee_pop_aggregate_return", 1, 1, false, true, true, true,
    ix86_handle_callee_pop_aggregate_return, NULL },
  { "interrupt", 0, 0, false, true, true, false,
    ix86_handle_interrupt_attribute, NULL },
  { "no_caller_saved_registers", 0, 0, false, true, true, false,
    ix86_handle_call_saved_registers_attribute, NULL },
  { "no_callee_saved_registers", 0, 0, false, true, true, true,
    ix86_handle_call_saved_registers_attribute, NULL },
  { "naked", 0, 0, true, false, false, false,
    ix86_handle_fndecl_attribute, NULL },
  { "indirect_branch", 1, 1, true, false, false, false,
    ix86_handle_fndecl_attribute, NULL },
  { "function_return", 1, 1, true, false, false, false,
    ix86_handle_fndecl_attribute, NULL },
  { "indirect_return", 0, 0, false, true, true, false,
    NULL, NULL },
  { "fentry_name", 1, 1, true, false, false, false,
    ix86_handle_fentry_name, NULL },
  { "fentry_section", 1, 1, true, false, false, false,
    ix86_handle_fentry_name, NULL },
  { "cf_check", 0, 0, true, false, false, false,
    ix86_handle_fndecl_attribute, NULL },
  { "nodirect_extern_access", 0, 0, true, false, false, false,
    handle_nodirect_extern_access_attribute, NULL }
};

const scoped_attribute_specs ix86_gnu_attribute_table =
{
  "gnu", { ix86_gnu_attributes }
};

#include "gt-i386-options.h"
