/* Get CPU type and Features for x86 processors.
   Copyright (C) 2012-2021 Free Software Foundation, Inc.
   Contributed by Sriraman Tallam (tmsriram@google.com)

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

struct __processor_model
{
  unsigned int __cpu_vendor;
  unsigned int __cpu_type;
  unsigned int __cpu_subtype;
  /* The first 32 features are stored as bitmasks in __cpu_features.
     The rest of features are stored as bitmasks in a separate array
     of unsigned int.  */
  unsigned int __cpu_features[1];
};

struct __processor_model2
{
  unsigned int __cpu_family;
  unsigned int __cpu_model;
  unsigned int __cpu_max_level;
  unsigned int __cpu_ext_level;
};

#ifndef CHECK___builtin_cpu_is
# define CHECK___builtin_cpu_is(cpu)
#endif

/* Return non-zero if the processor has feature F.  */

static inline int
has_cpu_feature (struct __processor_model *cpu_model,
		 unsigned int *cpu_features2,
		 enum processor_features f)
{
  unsigned int i;
  if (f < 32)
    {
      /* The first 32 features.  */
      return cpu_model->__cpu_features[0] & (1U << (f & 31));
    }
  /* The rest of features.  cpu_features2[i] contains features from
     (32 + i * 32) to (31 + 32 + i * 32), inclusively.  */
  for (i = 0; i < SIZE_OF_CPU_FEATURES; i++)
    if (f < (32 + 32 + i * 32))
    return cpu_features2[i] & (1U << ((f - (32 + i * 32)) & 31));
  gcc_unreachable ();
}

static inline void
set_cpu_feature (struct __processor_model *cpu_model,
		 unsigned int *cpu_features2,
		 enum processor_features f)
{
  unsigned int i;
  if (f < 32)
    {
      /* The first 32 features.  */
      cpu_model->__cpu_features[0] |= (1U << (f & 31));
      return;
    }
  /* The rest of features.  cpu_features2[i] contains features from
     (32 + i * 32) to (31 + 32 + i * 32), inclusively.  */
  for (i = 0; i < SIZE_OF_CPU_FEATURES; i++)
    if (f < (32 + 32 + i * 32))
      {
	cpu_features2[i] |= (1U << ((f - (32 + i * 32)) & 31));
	return;
      }
  gcc_unreachable ();
}

/* Get the specific type of AMD CPU and return AMD CPU name.  Return
   NULL for unknown AMD CPU.  */

static inline const char *
get_amd_cpu (struct __processor_model *cpu_model,
	     struct __processor_model2 *cpu_model2,
	     unsigned int *cpu_features2)
{
  const char *cpu = NULL;
  unsigned int family = cpu_model2->__cpu_family;
  unsigned int model = cpu_model2->__cpu_model;

  switch (family)
    {
    case 0x10:
      /* AMD Family 10h.  */
      cpu = "amdfam10";
      cpu_model->__cpu_type = AMDFAM10H;
      switch (model)
	{
	case 0x2:
	  /* Barcelona.  */
	  CHECK___builtin_cpu_is ("amdfam10h");
	  CHECK___builtin_cpu_is ("barcelona");
	  cpu_model->__cpu_subtype = AMDFAM10H_BARCELONA;
	  break;
	case 0x4:
	  /* Shanghai.  */
	  CHECK___builtin_cpu_is ("amdfam10h");
	  CHECK___builtin_cpu_is ("shanghai");
	  cpu_model->__cpu_subtype = AMDFAM10H_SHANGHAI;
	  break;
	case 0x8:
	  /* Istanbul.  */
	  CHECK___builtin_cpu_is ("amdfam10h");
	  CHECK___builtin_cpu_is ("istanbul");
	  cpu_model->__cpu_subtype = AMDFAM10H_ISTANBUL;
	  break;
	default:
	  break;
	}
      break;
    case 0x14:
      /* AMD Family 14h "btver1". */
      cpu = "btver1";
      CHECK___builtin_cpu_is ("btver1");
      cpu_model->__cpu_type = AMD_BTVER1;
      break;
    case 0x15:
      /* AMD Family 15h "Bulldozer".  */
      cpu_model->__cpu_type = AMDFAM15H;
      if (model == 0x2)
	{
	  /* Bulldozer version 2 "Piledriver" */
	  cpu = "bdver2";
	  CHECK___builtin_cpu_is ("bdver2");
	  cpu_model->__cpu_subtype = AMDFAM15H_BDVER2;
	}
      else if (model <= 0xf)
	{
	  /* Bulldozer version 1.  */
	  cpu = "bdver1";
	  CHECK___builtin_cpu_is ("bdver1");
	  cpu_model->__cpu_subtype = AMDFAM15H_BDVER1;
	}
      else if (model <= 0x2f)
	{
	  /* Bulldozer version 2 "Piledriver" */
	  cpu = "bdver2";
	  CHECK___builtin_cpu_is ("bdver2");
	  cpu_model->__cpu_subtype = AMDFAM15H_BDVER2;
	}
      else if (model <= 0x4f)
	{
	  /* Bulldozer version 3 "Steamroller"  */
	  cpu = "bdver3";
	  CHECK___builtin_cpu_is ("bdver3");
	  cpu_model->__cpu_subtype = AMDFAM15H_BDVER3;
	}
      else if (model <= 0x7f)
	{
	  /* Bulldozer version 4 "Excavator"   */
	  cpu = "bdver4";
	  CHECK___builtin_cpu_is ("bdver4");
	  cpu_model->__cpu_subtype = AMDFAM15H_BDVER4;
	}
      else if (has_cpu_feature (cpu_model, cpu_features2,
				FEATURE_AVX2))
	{
	  cpu = "bdver4";
	  CHECK___builtin_cpu_is ("bdver4");
	  cpu_model->__cpu_subtype = AMDFAM15H_BDVER4;
	}
      else if (has_cpu_feature (cpu_model, cpu_features2,
				FEATURE_XSAVEOPT))
	{
	  cpu = "bdver3";
	  CHECK___builtin_cpu_is ("bdver3");
	  cpu_model->__cpu_subtype = AMDFAM15H_BDVER3;
	}
      else if (has_cpu_feature (cpu_model, cpu_features2,
				FEATURE_BMI))
	{
	  cpu = "bdver2";
	  CHECK___builtin_cpu_is ("bdver2");
	  cpu_model->__cpu_subtype = AMDFAM15H_BDVER2;
	}
      else if (has_cpu_feature (cpu_model, cpu_features2,
				FEATURE_XOP))
	{
	  cpu = "bdver1";
	  CHECK___builtin_cpu_is ("bdver1");
	  cpu_model->__cpu_subtype = AMDFAM15H_BDVER1;
	}
      break;
    case 0x16:
      /* AMD Family 16h "btver2" */
      cpu = "btver2";
      CHECK___builtin_cpu_is ("btver2");
      cpu_model->__cpu_type = AMD_BTVER2;
      break;
    case 0x17:
      cpu_model->__cpu_type = AMDFAM17H;
      if (model <= 0x1f)
	{
	  /* AMD family 17h version 1.  */
	  cpu = "znver1";
	  CHECK___builtin_cpu_is ("znver1");
	  cpu_model->__cpu_subtype = AMDFAM17H_ZNVER1;
	}
      else if (model >= 0x30)
	{
	  cpu = "znver2";
	  CHECK___builtin_cpu_is ("znver2");
	  cpu_model->__cpu_subtype = AMDFAM17H_ZNVER2;
	}
      else if (has_cpu_feature (cpu_model, cpu_features2,
				FEATURE_CLWB))
	{
	  cpu = "znver2";
	  CHECK___builtin_cpu_is ("znver2");
	  cpu_model->__cpu_subtype = AMDFAM17H_ZNVER2;
	}
      else if (has_cpu_feature (cpu_model, cpu_features2,
				FEATURE_CLZERO))
	{
	  cpu = "znver1";
	  CHECK___builtin_cpu_is ("znver1");
	  cpu_model->__cpu_subtype = AMDFAM17H_ZNVER1;
	}
      break;
    case 0x19:
      cpu_model->__cpu_type = AMDFAM19H;
      /* AMD family 19h version 1.  */
      if (model <= 0x0f)
	{
	  cpu = "znver3";
	  CHECK___builtin_cpu_is ("znver3");
	  cpu_model->__cpu_subtype = AMDFAM19H_ZNVER3;
	}
      else if (has_cpu_feature (cpu_model, cpu_features2,
				FEATURE_VAES))
	{
	  cpu = "znver3";
	  CHECK___builtin_cpu_is ("znver3");
	  cpu_model->__cpu_subtype = AMDFAM19H_ZNVER3;
	}
      break;
    default:
      break;
    }

  return cpu;
}

/* Get the specific type of Intel CPU and return Intel CPU name.  Return
   NULL for unknown Intel CPU.  */

static inline const char *
get_intel_cpu (struct __processor_model *cpu_model,
	       struct __processor_model2 *cpu_model2,
	       unsigned int *cpu_features2)
{
  const char *cpu = NULL;

  /* Parse family and model only for model 6. */
  if (cpu_model2->__cpu_family != 0x6)
    return cpu;

  switch (cpu_model2->__cpu_model)
    {
    case 0x1c:
    case 0x26:
      /* Bonnell.  */
      cpu = "bonnell";
      CHECK___builtin_cpu_is ("atom");
      cpu_model->__cpu_type = INTEL_BONNELL;
      break;
    case 0x37:
    case 0x4a:
    case 0x4d:
    case 0x5d:
      /* Silvermont.  */
    case 0x4c:
    case 0x5a:
    case 0x75:
      /* Airmont.  */
      cpu = "silvermont";
      CHECK___builtin_cpu_is ("silvermont");
      cpu_model->__cpu_type = INTEL_SILVERMONT;
      break;
    case 0x5c:
    case 0x5f:
      /* Goldmont.  */
      cpu = "goldmont";
      CHECK___builtin_cpu_is ("goldmont");
      cpu_model->__cpu_type = INTEL_GOLDMONT;
      break;
    case 0x7a:
      /* Goldmont Plus.  */
      cpu = "goldmont-plus";
      CHECK___builtin_cpu_is ("goldmont-plus");
      cpu_model->__cpu_type = INTEL_GOLDMONT_PLUS;
      break;
    case 0x86:
    case 0x96:
    case 0x9c:
      /* Tremont.  */
      cpu = "tremont";
      CHECK___builtin_cpu_is ("tremont");
      cpu_model->__cpu_type = INTEL_TREMONT;
      break;
    case 0x57:
      /* Knights Landing.  */
      cpu = "knl";
      CHECK___builtin_cpu_is ("knl");
      cpu_model->__cpu_type = INTEL_KNL;
      break;
    case 0x85:
      /* Knights Mill. */
      cpu = "knm";
      CHECK___builtin_cpu_is ("knm");
      cpu_model->__cpu_type = INTEL_KNM;
      break;
    case 0x1a:
    case 0x1e:
    case 0x1f:
    case 0x2e:
      /* Nehalem.  */
      cpu = "nehalem";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("nehalem");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_NEHALEM;
      break;
    case 0x25:
    case 0x2c:
    case 0x2f:
      /* Westmere.  */
      cpu = "westmere";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("westmere");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_WESTMERE;
      break;
    case 0x2a:
    case 0x2d:
      /* Sandy Bridge.  */
      cpu = "sandybridge";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("sandybridge");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_SANDYBRIDGE;
      break;
    case 0x3a:
    case 0x3e:
      /* Ivy Bridge.  */
      cpu = "ivybridge";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("ivybridge");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_IVYBRIDGE;
      break;
    case 0x3c:
    case 0x3f:
    case 0x45:
    case 0x46:
      /* Haswell.  */
      cpu = "haswell";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("haswell");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_HASWELL;
      break;
    case 0x3d:
    case 0x47:
    case 0x4f:
    case 0x56:
      /* Broadwell.  */
      cpu = "broadwell";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("broadwell");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_BROADWELL;
      break;
    case 0x4e:
    case 0x5e:
      /* Skylake.  */
    case 0x8e:
    case 0x9e:
      /* Kaby Lake.  */
    case 0xa5:
    case 0xa6:
      /* Comet Lake.  */
      cpu = "skylake";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("skylake");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_SKYLAKE;
      break;
    case 0xa7:
      /* Rocket Lake.  */
      cpu = "rocketlake";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("rocketlake");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_ROCKETLAKE;
      break;
    case 0x55:
      CHECK___builtin_cpu_is ("corei7");
      cpu_model->__cpu_type = INTEL_COREI7;
      if (has_cpu_feature (cpu_model, cpu_features2,
			   FEATURE_AVX512BF16))
	{
	  /* Cooper Lake.  */
	  cpu = "cooperlake";
	  CHECK___builtin_cpu_is ("cooperlake");
	  cpu_model->__cpu_subtype = INTEL_COREI7_COOPERLAKE;
	}
      else if (has_cpu_feature (cpu_model, cpu_features2,
				FEATURE_AVX512VNNI))
	{
	  /* Cascade Lake.  */
	  cpu = "cascadelake";
	  CHECK___builtin_cpu_is ("cascadelake");
	  cpu_model->__cpu_subtype = INTEL_COREI7_CASCADELAKE;
	}
      else
	{
	  /* Skylake with AVX-512 support.  */
	  cpu = "skylake-avx512";
	  CHECK___builtin_cpu_is ("skylake-avx512");
	  cpu_model->__cpu_subtype = INTEL_COREI7_SKYLAKE_AVX512;
	}
      break;
    case 0x66:
      /* Cannon Lake.  */
      cpu = "cannonlake";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("cannonlake");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_CANNONLAKE;
      break;
    case 0x6a:
    case 0x6c:
      /* Ice Lake server.  */
      cpu = "icelake-server";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("icelake-server");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_ICELAKE_SERVER;
      break;
    case 0x7e:
    case 0x7d:
    case 0x9d:
       /* Ice Lake client.  */
      cpu = "icelake-client";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("icelake-client");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_ICELAKE_CLIENT;
      break;
    case 0x8c:
    case 0x8d:
      /* Tiger Lake.  */
      cpu = "tigerlake";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("tigerlake");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_TIGERLAKE;
      break;
    case 0x97:
    case 0x9a:
      /* Alder Lake.  */
      cpu = "alderlake";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("alderlake");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_ALDERLAKE;
      break;
    case 0x8f:
      /* Sapphire Rapids.  */
      cpu = "sapphirerapids";
      CHECK___builtin_cpu_is ("corei7");
      CHECK___builtin_cpu_is ("sapphirerapids");
      cpu_model->__cpu_type = INTEL_COREI7;
      cpu_model->__cpu_subtype = INTEL_COREI7_SAPPHIRERAPIDS;
      break;
    case 0x17:
    case 0x1d:
      /* Penryn.  */
    case 0x0f:
      /* Merom.  */
      cpu = "core2";
      CHECK___builtin_cpu_is ("core2");
      cpu_model->__cpu_type = INTEL_CORE2;
      break;
    default:
      break;
    }

  return cpu;
}

/* ECX and EDX are output of CPUID at level one.  */
static inline void
get_available_features (struct __processor_model *cpu_model,
			struct __processor_model2 *cpu_model2,
			unsigned int *cpu_features2,
			unsigned int ecx, unsigned int edx)
{
  unsigned int max_cpuid_level = cpu_model2->__cpu_max_level;
  unsigned int eax, ebx;
  unsigned int ext_level;

  /* Get XCR_XFEATURE_ENABLED_MASK register with xgetbv.  */
#define XCR_XFEATURE_ENABLED_MASK	0x0
#define XSTATE_FP			0x1
#define XSTATE_SSE			0x2
#define XSTATE_YMM			0x4
#define XSTATE_OPMASK			0x20
#define XSTATE_ZMM			0x40
#define XSTATE_HI_ZMM			0x80
#define XSTATE_TILECFG			0x20000
#define XSTATE_TILEDATA		0x40000

#define XCR_AVX_ENABLED_MASK \
  (XSTATE_SSE | XSTATE_YMM)
#define XCR_AVX512F_ENABLED_MASK \
  (XSTATE_SSE | XSTATE_YMM | XSTATE_OPMASK | XSTATE_ZMM | XSTATE_HI_ZMM)
#define XCR_AMX_ENABLED_MASK \
  (XSTATE_TILECFG | XSTATE_TILEDATA)

  /* Check if AVX and AVX512 are usable.  */
  int avx_usable = 0;
  int avx512_usable = 0;
  int amx_usable = 0;
  /* Check if KL is usable.  */
  int has_kl = 0;
  if ((ecx & bit_OSXSAVE))
    {
      /* Check if XMM, YMM, OPMASK, upper 256 bits of ZMM0-ZMM15 and
	 ZMM16-ZMM31 states are supported by OSXSAVE.  */
      unsigned int xcrlow;
      unsigned int xcrhigh;
      __asm__ (".byte 0x0f, 0x01, 0xd0"
	       : "=a" (xcrlow), "=d" (xcrhigh)
	       : "c" (XCR_XFEATURE_ENABLED_MASK));
      if ((xcrlow & XCR_AVX_ENABLED_MASK) == XCR_AVX_ENABLED_MASK)
	{
	  avx_usable = 1;
	  avx512_usable = ((xcrlow & XCR_AVX512F_ENABLED_MASK)
			   == XCR_AVX512F_ENABLED_MASK);
	}
      amx_usable = ((xcrlow & XCR_AMX_ENABLED_MASK)
		    == XCR_AMX_ENABLED_MASK);
    }

#define set_feature(f) \
  set_cpu_feature (cpu_model, cpu_features2, f)

  if (edx & bit_CMOV)
    set_feature (FEATURE_CMOV);
  if (edx & bit_MMX)
    set_feature (FEATURE_MMX);
  if (edx & bit_SSE)
    set_feature (FEATURE_SSE);
  if (edx & bit_SSE2)
    set_feature (FEATURE_SSE2);
  if (edx & bit_CMPXCHG8B)
    set_feature (FEATURE_CMPXCHG8B);
  if (edx & bit_FXSAVE)
    set_feature (FEATURE_FXSAVE);

  if (ecx & bit_POPCNT)
    set_feature (FEATURE_POPCNT);
  if (ecx & bit_AES)
    set_feature (FEATURE_AES);
  if (ecx & bit_PCLMUL)
    set_feature (FEATURE_PCLMUL);
  if (ecx & bit_SSE3)
    set_feature (FEATURE_SSE3);
  if (ecx & bit_SSSE3)
    set_feature (FEATURE_SSSE3);
  if (ecx & bit_SSE4_1)
    set_feature (FEATURE_SSE4_1);
  if (ecx & bit_SSE4_2)
    set_feature (FEATURE_SSE4_2);
  if (ecx & bit_OSXSAVE)
    set_feature (FEATURE_OSXSAVE);
  if (ecx & bit_CMPXCHG16B)
    set_feature (FEATURE_CMPXCHG16B);
  if (ecx & bit_MOVBE)
    set_feature (FEATURE_MOVBE);
  if (ecx & bit_AES)
    set_feature (FEATURE_AES);
  if (ecx & bit_F16C)
    set_feature (FEATURE_F16C);
  if (ecx & bit_RDRND)
    set_feature (FEATURE_RDRND);
  if (ecx & bit_XSAVE)
    set_feature (FEATURE_XSAVE);
  if (avx_usable)
    {
      if (ecx & bit_AVX)
	set_feature (FEATURE_AVX);
      if (ecx & bit_FMA)
	set_feature (FEATURE_FMA);
    }

  /* Get Advanced Features at level 7 (eax = 7, ecx = 0/1). */
  if (max_cpuid_level >= 7)
    {
      __cpuid_count (7, 0, eax, ebx, ecx, edx);
      if (ebx & bit_BMI)
	set_feature (FEATURE_BMI);
      if (ebx & bit_SGX)
	set_feature (FEATURE_SGX);
      if (ebx & bit_HLE)
	set_feature (FEATURE_HLE);
      if (ebx & bit_RTM)
	set_feature (FEATURE_RTM);
      if (avx_usable)
	{
	  if (ebx & bit_AVX2)
	    set_feature (FEATURE_AVX2);
	  if (ecx & bit_VPCLMULQDQ)
	    set_feature (FEATURE_VPCLMULQDQ);
	}
      if (ebx & bit_BMI2)
	set_feature (FEATURE_BMI2);
      if (ebx & bit_FSGSBASE)
	set_feature (FEATURE_FSGSBASE);
      if (ebx & bit_RDSEED)
	set_feature (FEATURE_RDSEED);
      if (ebx & bit_ADX)
	set_feature (FEATURE_ADX);
      if (ebx & bit_SHA)
	set_feature (FEATURE_SHA);
      if (ebx & bit_CLFLUSHOPT)
	set_feature (FEATURE_CLFLUSHOPT);
      if (ebx & bit_CLWB)
	set_feature (FEATURE_CLWB);
      if (ecx & bit_PREFETCHWT1)
	set_feature (FEATURE_PREFETCHWT1);
      /* NB: bit_OSPKE indicates that OS supports PKU.  */
      if (ecx & bit_OSPKE)
	set_feature (FEATURE_PKU);
      if (ecx & bit_RDPID)
	set_feature (FEATURE_RDPID);
      if (ecx & bit_VAES)
	set_feature (FEATURE_VAES);
      if (ecx & bit_GFNI)
	set_feature (FEATURE_GFNI);
      if (ecx & bit_MOVDIRI)
	set_feature (FEATURE_MOVDIRI);
      if (ecx & bit_MOVDIR64B)
	set_feature (FEATURE_MOVDIR64B);
      if (ecx & bit_ENQCMD)
	set_feature (FEATURE_ENQCMD);
      if (ecx & bit_CLDEMOTE)
	set_feature (FEATURE_CLDEMOTE);
      if (ecx & bit_WAITPKG)
	set_feature (FEATURE_WAITPKG);
      if (ecx & bit_SHSTK)
	set_feature (FEATURE_SHSTK);
      if (ecx & bit_KL)
	has_kl = 1;
      if (edx & bit_SERIALIZE)
	set_feature (FEATURE_SERIALIZE);
      if (edx & bit_TSXLDTRK)
	set_feature (FEATURE_TSXLDTRK);
      if (edx & bit_PCONFIG)
	set_feature (FEATURE_PCONFIG);
      if (edx & bit_IBT)
	set_feature (FEATURE_IBT);
      if (edx & bit_UINTR)
	set_feature (FEATURE_UINTR);
      if (amx_usable)
	{
	  if (edx & bit_AMX_TILE)
	    set_feature (FEATURE_AMX_TILE);
	  if (edx & bit_AMX_INT8)
	    set_feature (FEATURE_AMX_INT8);
	  if (edx & bit_AMX_BF16)
	    set_feature (FEATURE_AMX_BF16);
	}
      if (avx512_usable)
	{
	  if (ebx & bit_AVX512F)
	    set_feature (FEATURE_AVX512F);
	  if (ebx & bit_AVX512VL)
	    set_feature (FEATURE_AVX512VL);
	  if (ebx & bit_AVX512BW)
	    set_feature (FEATURE_AVX512BW);
	  if (ebx & bit_AVX512DQ)
	    set_feature (FEATURE_AVX512DQ);
	  if (ebx & bit_AVX512CD)
	    set_feature (FEATURE_AVX512CD);
	  if (ebx & bit_AVX512PF)
	    set_feature (FEATURE_AVX512PF);
	  if (ebx & bit_AVX512ER)
	    set_feature (FEATURE_AVX512ER);
	  if (ebx & bit_AVX512IFMA)
	    set_feature (FEATURE_AVX512IFMA);
	  if (ecx & bit_AVX512VBMI)
	    set_feature (FEATURE_AVX512VBMI);
	  if (ecx & bit_AVX512VBMI2)
	    set_feature (FEATURE_AVX512VBMI2);
	  if (ecx & bit_AVX512VNNI)
	    set_feature (FEATURE_AVX512VNNI);
	  if (ecx & bit_AVX512BITALG)
	    set_feature (FEATURE_AVX512BITALG);
	  if (ecx & bit_AVX512VPOPCNTDQ)
	    set_feature (FEATURE_AVX512VPOPCNTDQ);
	  if (edx & bit_AVX5124VNNIW)
	    set_feature (FEATURE_AVX5124VNNIW);
	  if (edx & bit_AVX5124FMAPS)
	    set_feature (FEATURE_AVX5124FMAPS);
	  if (edx & bit_AVX512VP2INTERSECT)
	    set_feature (FEATURE_AVX512VP2INTERSECT);
	}

      __cpuid_count (7, 1, eax, ebx, ecx, edx);
      if (eax & bit_HRESET)
	set_feature (FEATURE_HRESET);
      if (avx_usable)
	{
	  if (eax & bit_AVXVNNI)
	    set_feature (FEATURE_AVXVNNI);
	}
      if (avx512_usable)
	{
	  if (eax & bit_AVX512BF16)
	    set_feature (FEATURE_AVX512BF16);
	}
    }

  /* Get Advanced Features at level 0xd (eax = 0xd, ecx = 1). */
  if (max_cpuid_level >= 0xd)
    {
      __cpuid_count (0xd, 1, eax, ebx, ecx, edx);
      if (eax & bit_XSAVEOPT)
	set_feature (FEATURE_XSAVEOPT);
      if (eax & bit_XSAVEC)
	set_feature (FEATURE_XSAVEC);
      if (eax & bit_XSAVES)
	set_feature (FEATURE_XSAVES);
    }

  /* Get Advanced Features at level 0x14 (eax = 0x14, ecx = 0). */
  if (max_cpuid_level >= 0x14)
    {
      __cpuid_count (0x14, 0, eax, ebx, ecx, edx);
      if (ebx & bit_PTWRITE)
	set_feature (FEATURE_PTWRITE);
    }

  /* Get Advanced Features at level 0x19 (eax = 0x19).  */
  if (max_cpuid_level >= 0x19)
    {
      set_feature (FEATURE_AESKLE);
      __cpuid (19, eax, ebx, ecx, edx);
      /* Check if OS support keylocker.  */
      if (ebx & bit_AESKLE)
	{
	  if (ebx & bit_WIDEKL)
	    set_feature (FEATURE_WIDEKL);
	  if (has_kl)
	    set_feature (FEATURE_KL);
	}
    }

  /* Check cpuid level of extended features.  */
  __cpuid (0x80000000, ext_level, ebx, ecx, edx);

  cpu_model2->__cpu_ext_level = ext_level;

  if (ext_level >= 0x80000001)
    {
      __cpuid (0x80000001, eax, ebx, ecx, edx);

      if (ecx & bit_SSE4a)
	set_feature (FEATURE_SSE4_A);
      if (ecx & bit_LAHF_LM)
	set_feature (FEATURE_LAHF_LM);
      if (ecx & bit_ABM)
	set_feature (FEATURE_ABM);
      if (ecx & bit_LWP)
	set_feature (FEATURE_LWP);
      if (ecx & bit_TBM)
	set_feature (FEATURE_TBM);
      if (ecx & bit_LZCNT)
	set_feature (FEATURE_LZCNT);
      if (ecx & bit_PRFCHW)
	set_feature (FEATURE_PRFCHW);
      if (ecx & bit_MWAITX)
	set_feature (FEATURE_MWAITX);

      if (edx & bit_LM)
	set_feature (FEATURE_LM);
      if (edx & bit_3DNOWP)
	set_feature (FEATURE_3DNOWP);
      if (edx & bit_3DNOW)
	set_feature (FEATURE_3DNOW);

      if (avx_usable)
	{
	  if (ecx & bit_FMA4)
	    set_feature (FEATURE_FMA4);
	  if (ecx & bit_XOP)
	    set_feature (FEATURE_XOP);
	}
    }

  if (ext_level >= 0x80000008)
    {
      __cpuid (0x80000008, eax, ebx, ecx, edx);
      if (ebx & bit_CLZERO)
	set_feature (FEATURE_CLZERO);
      if (ebx & bit_WBNOINVD)
	set_feature (FEATURE_WBNOINVD);
    }

#undef set_feature
}

static inline int
cpu_indicator_init (struct __processor_model *cpu_model,
		    struct __processor_model2 *cpu_model2,
		    unsigned int *cpu_features2)
{
  unsigned int eax, ebx, ecx, edx;

  int max_level;
  unsigned int vendor;
  unsigned int model, family;
  unsigned int extended_model, extended_family;

  /* This function needs to run just once.  */
  if (cpu_model->__cpu_vendor)
    return 0;

  /* Assume cpuid insn present. Run in level 0 to get vendor id. */
  if (!__get_cpuid (0, &eax, &ebx, &ecx, &edx))
    {
      cpu_model->__cpu_vendor = VENDOR_OTHER;
      return -1;
    }

  vendor = ebx;
  max_level = eax;

  if (max_level < 1)
    {
      cpu_model->__cpu_vendor = VENDOR_OTHER;
      return -1;
    }

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    {
      cpu_model->__cpu_vendor = VENDOR_OTHER;
      return -1;
    }

  cpu_model2->__cpu_max_level = max_level;

  model = (eax >> 4) & 0x0f;
  family = (eax >> 8) & 0x0f;
  extended_model = (eax >> 12) & 0xf0;
  extended_family = (eax >> 20) & 0xff;

  if (vendor == signature_INTEL_ebx)
    {
      /* Adjust model and family for Intel CPUS. */
      if (family == 0x0f)
	{
	  family += extended_family;
	  model += extended_model;
	}
      else if (family == 0x06)
	model += extended_model;

      cpu_model2->__cpu_family = family;
      cpu_model2->__cpu_model = model;

      /* Find available features. */
      get_available_features (cpu_model, cpu_model2, cpu_features2,
			      ecx, edx);
      /* Get CPU type.  */
      get_intel_cpu (cpu_model, cpu_model2, cpu_features2);
      cpu_model->__cpu_vendor = VENDOR_INTEL;
    }
  else if (vendor == signature_AMD_ebx)
    {
      /* Adjust model and family for AMD CPUS. */
      if (family == 0x0f)
	{
	  family += extended_family;
	  model += extended_model;
	}

      cpu_model2->__cpu_family = family;
      cpu_model2->__cpu_model = model;

      /* Find available features. */
      get_available_features (cpu_model, cpu_model2, cpu_features2,
			      ecx, edx);
      /* Get CPU type.  */
      get_amd_cpu (cpu_model, cpu_model2, cpu_features2);
      cpu_model->__cpu_vendor = VENDOR_AMD;
    }
  else if (vendor == signature_CENTAUR_ebx)
    cpu_model->__cpu_vendor = VENDOR_CENTAUR;
  else if (vendor == signature_CYRIX_ebx)
    cpu_model->__cpu_vendor = VENDOR_CYRIX;
  else if (vendor == signature_NSC_ebx)
    cpu_model->__cpu_vendor = VENDOR_NSC;
  else
    cpu_model->__cpu_vendor = VENDOR_OTHER;

  gcc_assert (cpu_model->__cpu_vendor < VENDOR_MAX);
  gcc_assert (cpu_model->__cpu_type < CPU_TYPE_MAX);
  gcc_assert (cpu_model->__cpu_subtype < CPU_SUBTYPE_MAX);

  return 0;
}
