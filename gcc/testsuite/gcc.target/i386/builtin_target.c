/* This test checks if the __builtin_cpu_is and __builtin_cpu_supports calls
   are recognized.  It also independently uses CPUID to get cpu type and
   features supported and checks if the builtins correctly identify the
   platform.  The code to do the identification is adapted from
   libgcc/config/i386/cpuinfo.c.  */

/* { dg-do run } */

#include <assert.h>
#include "cpuid.h"

/* Check if the Intel CPU model and sub-model are identified.  */
static void
check_intel_cpu_model (unsigned int family, unsigned int model,
		       unsigned int brand_id)
{
  /* Parse family and model only if brand ID is 0. */
  if (brand_id == 0)
    {
      switch (family)
	{
	case 0x5:
	  /* Pentium.  */
	  break;
	case 0x6:
	  switch (model)
	    {
	    case 0x1c:
	    case 0x26:
	      /* Atom.  */
	      assert (__builtin_cpu_is ("atom"));
	      break;
	    case 0x37:
	    case 0x4a:
	    case 0x4d:
	    case 0x5a:
	    case 0x5d:
	      /* Silvermont.  */
	      assert (__builtin_cpu_is ("silvermont"));
	      break;
	    case 0x5c:
	    case 0x5f:
	      /* Goldmont.  */
	      assert (__builtin_cpu_is ("goldmont"));
	      break;
	    case 0x7a:
	      /* Goldmont Plus.  */
	      assert (__builtin_cpu_is ("goldmont-plus"));
	      break;
	    case 0x57:
	      /* Knights Landing.  */
	      assert (__builtin_cpu_is ("knl"));
	      break;
	    case 0x85:
	      /* Knights Mill */
	      assert (__builtin_cpu_is ("knm"));
	      break;
	    case 0x1a:
	    case 0x1e:
	    case 0x1f:
	    case 0x2e:
	      /* Nehalem.  */
	      assert (__builtin_cpu_is ("corei7"));
	      assert (__builtin_cpu_is ("nehalem"));
	      break;
	    case 0x25:
	    case 0x2c:
	    case 0x2f:
	      /* Westmere.  */
	      assert (__builtin_cpu_is ("corei7"));
	      assert (__builtin_cpu_is ("westmere"));
	      break;
	    case 0x2a:
	    case 0x2d:
	      /* Sandy Bridge.  */
	      assert (__builtin_cpu_is ("corei7"));
	      assert (__builtin_cpu_is ("sandybridge"));
	      break;
	    case 0x3a:
	    case 0x3e:
	      /* Ivy Bridge.  */
	      assert (__builtin_cpu_is ("corei7"));
	      assert (__builtin_cpu_is ("ivybridge"));
	      break;
	    case 0x3c:
	    case 0x3f:
	    case 0x45:
	    case 0x46:
	      /* Haswell.  */
	      assert (__builtin_cpu_is ("corei7"));
	      assert (__builtin_cpu_is ("haswell"));
	      break;
	    case 0x3d:
	    case 0x47:
	    case 0x4f:
	    case 0x56:
	      /* Broadwell.  */
	      assert (__builtin_cpu_is ("corei7"));
	      assert (__builtin_cpu_is ("broadwell"));
	      break;
	    case 0x4e:
	    case 0x5e:
	      /* Skylake.  */
	    case 0x8e:
	    case 0x9e:
	      /* Kaby Lake.  */
	      assert (__builtin_cpu_is ("corei7"));
	      assert (__builtin_cpu_is ("skylake"));
	      break;
	    case 0x55:
	      {
	        unsigned int eax, ebx, ecx, edx;
	        __cpuid_count (7, 0, eax, ebx, ecx, edx);
	        assert (__builtin_cpu_is ("corei7"));
	        if (ecx & bit_AVX512VNNI)
	          /* Cascade Lake.  */
	          assert (__builtin_cpu_is ("cascadelake"));
	        else
	          /* Skylake with AVX-512 support.  */
	          assert (__builtin_cpu_is ("skylake-avx512"));
	        break;
	      }
	    case 0x66:
	      /* Cannon Lake.  */
	      assert (__builtin_cpu_is ("cannonlake"));
	      break;
	    case 0x17:
	    case 0x1d:
	      /* Penryn.  */
	    case 0x0f:
	      /* Merom.  */
	      assert (__builtin_cpu_is ("core2"));
	      break;
	    default:
	      break;
	    }
	  break;
	default:
	  /* We have no idea.  */
	  break;
	}
    }
}

/* Check if the AMD CPU model and sub-model are identified.  */
static void
check_amd_cpu_model (unsigned int family, unsigned int model)
{
  switch (family)
    {
    /* AMD Family 10h.  */
    case 0x10:
      switch (model)
	{
	case 0x2:
	  /* Barcelona.  */
	  assert (__builtin_cpu_is ("amdfam10h"));
	  assert (__builtin_cpu_is ("barcelona"));
	  break;
	case 0x4:
	  /* Shanghai.  */
	  assert (__builtin_cpu_is ("amdfam10h"));
	  assert (__builtin_cpu_is ("shanghai"));
	  break;
	case 0x8:
	  /* Istanbul.  */
	  assert (__builtin_cpu_is ("amdfam10h"));
	  assert (__builtin_cpu_is ("istanbul"));
	  break;
	default:
	  break;
	}
      break;
    /* AMD Family 15h.  */
    case 0x15:
      assert (__builtin_cpu_is ("amdfam15h"));
      /* Bulldozer version 1.  */
      if ( model <= 0xf)
	assert (__builtin_cpu_is ("bdver1"));
      /* Bulldozer version 2.  */
      if (model >= 0x10 && model <= 0x1f)
	assert (__builtin_cpu_is ("bdver2"));
      break;
    default:
      break;
    }
}

/* Check if the ISA features are identified.  */
static void
check_features (unsigned int ecx, unsigned int edx,
		int max_cpuid_level)
{
  unsigned int eax, ebx;
  unsigned int ext_level;

  if (edx & bit_CMOV)
    assert (__builtin_cpu_supports ("cmov"));
  if (edx & bit_MMX)
    assert (__builtin_cpu_supports ("mmx"));
  if (edx & bit_SSE)
    assert (__builtin_cpu_supports ("sse"));
  if (edx & bit_SSE2)
    assert (__builtin_cpu_supports ("sse2"));
  if (ecx & bit_POPCNT)
    assert (__builtin_cpu_supports ("popcnt"));
  if (ecx & bit_AES)
    assert (__builtin_cpu_supports ("aes"));
  if (ecx & bit_PCLMUL)
    assert (__builtin_cpu_supports ("pclmul"));
  if (ecx & bit_SSE3)
    assert (__builtin_cpu_supports ("sse3"));
  if (ecx & bit_SSSE3)
    assert (__builtin_cpu_supports ("ssse3"));
  if (ecx & bit_SSE4_1)
    assert (__builtin_cpu_supports ("sse4.1"));
  if (ecx & bit_SSE4_2)
    assert (__builtin_cpu_supports ("sse4.2"));
  if (ecx & bit_AVX)
    assert (__builtin_cpu_supports ("avx"));
  if (ecx & bit_FMA)
    assert (__builtin_cpu_supports ("fma"));

  /* Get advanced features at level 7 (eax = 7, ecx = 0).  */
  if (max_cpuid_level >= 7)
    {
      __cpuid_count (7, 0, eax, ebx, ecx, edx);
      if (ebx & bit_BMI)
	assert (__builtin_cpu_supports ("bmi"));
      if (ebx & bit_AVX2)
	assert (__builtin_cpu_supports ("avx2"));
      if (ebx & bit_BMI2)
	assert (__builtin_cpu_supports ("bmi2"));
      if (ebx & bit_AVX512F)
	assert (__builtin_cpu_supports ("avx512f"));
      if (ebx & bit_AVX512VL)
	assert (__builtin_cpu_supports ("avx512vl"));
      if (ebx & bit_AVX512BW)
	assert (__builtin_cpu_supports ("avx512bw"));
      if (ebx & bit_AVX512DQ)
	assert (__builtin_cpu_supports ("avx512dq"));
      if (ebx & bit_AVX512CD)
	assert (__builtin_cpu_supports ("avx512cd"));
      if (ebx & bit_AVX512PF)
	assert (__builtin_cpu_supports ("avx512pf"));
      if (ebx & bit_AVX512ER)
	assert (__builtin_cpu_supports ("avx512er"));
      if (ebx & bit_AVX512IFMA)
	assert (__builtin_cpu_supports ("avx512ifma"));
      if (ecx & bit_AVX512VBMI)
	assert (__builtin_cpu_supports ("avx512vbmi"));
      if (ecx & bit_AVX512VBMI2)
	assert (__builtin_cpu_supports ("avx512vbmi2"));
      if (ecx & bit_GFNI)
	assert (__builtin_cpu_supports ("gfni"));
      if (ecx & bit_VPCLMULQDQ)
	assert (__builtin_cpu_supports ("vpclmulqdq"));
      if (ecx & bit_AVX512VNNI)
	assert (__builtin_cpu_supports ("avx512vnni"));
      if (ecx & bit_AVX512BITALG)
	assert (__builtin_cpu_supports ("avx512bitalg"));
      if (ecx & bit_AVX512VPOPCNTDQ)
	assert (__builtin_cpu_supports ("avx512vpopcntdq"));
      if (edx & bit_AVX5124VNNIW)
	assert (__builtin_cpu_supports ("avx5124vnniw"));
      if (edx & bit_AVX5124FMAPS)
	assert (__builtin_cpu_supports ("avx5124fmaps"));
    }

  /* Check cpuid level of extended features.  */
  __cpuid (0x80000000, ext_level, ebx, ecx, edx);

  if (ext_level >= 0x80000001)
    {
      __cpuid (0x80000001, eax, ebx, ecx, edx);

      if (ecx & bit_SSE4a)
	assert (__builtin_cpu_supports ("sse4a"));
      if (ecx & bit_FMA4)
	assert (__builtin_cpu_supports ("fma4"));
      if (ecx & bit_XOP)
	assert (__builtin_cpu_supports ("xop"));
    }
}

static int __attribute__ ((noinline))
__get_cpuid_output (unsigned int __level,
		    unsigned int *__eax, unsigned int *__ebx,
		    unsigned int *__ecx, unsigned int *__edx)
{
  return __get_cpuid (__level, __eax, __ebx, __ecx, __edx);
}

static int
check_detailed ()
{
  unsigned int eax, ebx, ecx, edx;

  int max_level; 
  unsigned int vendor;
  unsigned int model, family, brand_id;
  unsigned int extended_model, extended_family;

  /* Assume cpuid insn present. Run in level 0 to get vendor id. */
  if (!__get_cpuid_output (0, &eax, &ebx, &ecx, &edx))
    return 0;

  vendor = ebx;
  max_level = eax;

  if (max_level < 1)
    return 0;

  if (!__get_cpuid_output (1, &eax, &ebx, &ecx, &edx))
    return 0;

  model = (eax >> 4) & 0x0f;
  family = (eax >> 8) & 0x0f;
  brand_id = ebx & 0xff;
  extended_model = (eax >> 12) & 0xf0;
  extended_family = (eax >> 20) & 0xff;

  if (vendor == signature_INTEL_ebx)
    {
      assert (__builtin_cpu_is ("intel"));
      /* Adjust family and model for Intel CPUs.  */
      if (family == 0x0f)
	{
	  family += extended_family;
	  model += extended_model;
	}
      else if (family == 0x06)
	model += extended_model;
      check_intel_cpu_model (family, model, brand_id);
      check_features (ecx, edx, max_level);
    }
  else if (vendor == signature_AMD_ebx)
    {
      assert (__builtin_cpu_is ("amd"));
      /* Adjust model and family for AMD CPUS. */
      if (family == 0x0f)
	{
	  family += extended_family;
	  model += (extended_model << 4);
	}
      check_amd_cpu_model (family, model);
      check_features (ecx, edx, max_level);
    }

  return 0;
}

static int
quick_check ()
{
  /* Check CPU Features.  */
  assert (__builtin_cpu_supports ("cmov") >= 0);

  assert (__builtin_cpu_supports ("mmx") >= 0);

  assert (__builtin_cpu_supports ("popcnt") >= 0);

  assert (__builtin_cpu_supports ("sse") >= 0);

  assert (__builtin_cpu_supports ("sse2") >= 0);

  assert (__builtin_cpu_supports ("sse3") >= 0);

  assert (__builtin_cpu_supports ("ssse3") >= 0);

  assert (__builtin_cpu_supports ("sse4.1") >= 0);

  assert (__builtin_cpu_supports ("sse4.2") >= 0);

  assert (__builtin_cpu_supports ("avx") >= 0);

  assert (__builtin_cpu_supports ("avx2") >= 0);

  assert (__builtin_cpu_supports ("avx512f") >= 0);

  assert (__builtin_cpu_supports ("avx5124vnniw") >= 0);

  assert (__builtin_cpu_supports ("avx5124fmaps") >= 0);

  assert (__builtin_cpu_supports ("avx512vpopcntdq") >= 0);

  /* Check CPU type.  */
  assert (__builtin_cpu_is ("amd") >= 0);

  assert (__builtin_cpu_is ("intel") >= 0);

  assert (__builtin_cpu_is ("atom") >= 0);

  assert (__builtin_cpu_is ("core2") >= 0);

  assert (__builtin_cpu_is ("corei7") >= 0);

  assert (__builtin_cpu_is ("nehalem") >= 0);

  assert (__builtin_cpu_is ("westmere") >= 0);

  assert (__builtin_cpu_is ("sandybridge") >= 0);

  assert (__builtin_cpu_is ("amdfam10h") >= 0);

  assert (__builtin_cpu_is ("barcelona") >= 0);

  assert (__builtin_cpu_is ("shanghai") >= 0);

  assert (__builtin_cpu_is ("istanbul") >= 0);

  assert (__builtin_cpu_is ("amdfam15h") >= 0);

  assert (__builtin_cpu_is ("bdver1") >= 0);

  assert (__builtin_cpu_is ("bdver2") >= 0);

  return 0;
}

int main ()
{
  __builtin_cpu_init ();
  quick_check ();
  check_detailed ();
  return 0;
}
