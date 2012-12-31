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
	      /* Sandy Bridge.  */
	      assert (__builtin_cpu_is ("corei7"));
	      assert (__builtin_cpu_is ("sandybridge"));
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

  /* Get advanced features at level 7 (eax = 7, ecx = 0).  */
  if (max_cpuid_level >= 7)
    {
      unsigned int eax, ebx, ecx, edx;
      __cpuid_count (7, 0, eax, ebx, ecx, edx);
      if (ebx & bit_AVX2)
	assert (__builtin_cpu_supports ("avx2"));
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
