/* This test checks if the __builtin_cpu_is and __builtin_cpu_supports calls
   are recognized.  It also independently uses CPUID to get cpu type and
   features supported and checks if the builtins correctly identify the
   platform.  The code to do the identification is adapted from
   libgcc/config/i386/cpuinfo.c.  */

/* { dg-do run } */

#include <assert.h>
#include <stdlib.h>
#include "cpuid.h"
#define CHECK___builtin_cpu_is(cpu) assert (__builtin_cpu_is (cpu))
#define gcc_assert(a) assert (a)
#define gcc_unreachable() abort ()
#define inline
#include "../../../common/config/i386/i386-cpuinfo.h"
#include "../../../common/config/i386/cpuinfo.h"

/* Check if the ISA features are identified.  */
static void
check_features (struct __processor_model *cpu_model,
		unsigned int *cpu_features2)
{
#define has_feature(f) \
  has_cpu_feature (cpu_model, cpu_features2, f)
#define ISA_NAMES_TABLE_START
#define ISA_NAMES_TABLE_END
#define ISA_NAMES_TABLE_ENTRY(name, feature, priority, option)  \
  assert (!!has_feature (feature) == !!__builtin_cpu_supports (name));
#include "../../../common/config/i386/i386-isas.h"
}

static int
check_detailed ()
{
  struct __processor_model cpu_model = { 0 };
  struct __processor_model2 cpu_model2 = { 0 };
  unsigned int cpu_features2[SIZE_OF_CPU_FEATURES] = { 0 };

  if (cpu_indicator_init (&cpu_model, &cpu_model2, cpu_features2) != 0)
    return 0;

  check_features (&cpu_model, cpu_features2);

  switch (cpu_model.__cpu_vendor)
    {
    case VENDOR_INTEL:
      assert (__builtin_cpu_is ("intel"));
      get_intel_cpu (&cpu_model, &cpu_model2, cpu_features2);
      break;
    case VENDOR_AMD:
      assert (__builtin_cpu_is ("amd"));
      get_amd_cpu (&cpu_model, &cpu_model2, cpu_features2);
      break;
    default:
      break;
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
