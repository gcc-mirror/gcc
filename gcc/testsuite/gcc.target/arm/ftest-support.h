#if 0
#define INTERNAL_DEBUG 1
#endif

#ifdef INTERNAL_DEBUG
#include <stdio.h>
#endif

extern void abort (void);

enum architecture {
  ARCH_V4 = 0,
  ARCH_V4T,
  ARCH_V5T,
  ARCH_V5TE,
  ARCH_V6,
  ARCH_V6K,
  ARCH_V6T2,
  ARCH_V6Z,
  ARCH_V6M,
  ARCH_V7A,
  ARCH_V7R,
  ARCH_V7M,
  ARCH_V7EM,
  ARCH_COUNT
};

#define NUM_FEATURES 11
int feature_matrix[ARCH_COUNT][NUM_FEATURES];

int
ftest (int arch)
{
  int features[NUM_FEATURES] = {0};
  int y;

  for (y = 0; y < NUM_FEATURES; ++y)
    features[y] = 0;

#ifdef __ARM_ARCH
  features[0] = __ARM_ARCH;
#endif
#ifdef __ARM_ARCH_ISA_ARM
  features[1] = __ARM_ARCH_ISA_ARM;
#endif
#ifdef __ARM_ARCH_ISA_THUMB
  features[2] = __ARM_ARCH_ISA_THUMB;
#endif
#ifdef __ARM_ARCH_PROFILE
  features[3] = __ARM_ARCH_PROFILE;
#endif
#ifdef __ARM_FEATURE_UNALIGNED
  features[4] = __ARM_FEATURE_UNALIGNED;
#endif
#ifdef __ARM_FEATURE_LDREX
  features[5] = __ARM_FEATURE_LDREX;
#endif
#ifdef __ARM_FEATURE_CLZ
  features[6] = __ARM_FEATURE_CLZ;
#endif
#ifdef __ARM_FEATURE_DSP
  features[7] = __ARM_FEATURE_DSP;
#endif
#ifdef __ARM_FEATURE_SIMD32
  features[8] = __ARM_FEATURE_SIMD32;
#endif
#ifdef __ARM_FEATURE_QBIT
  features[9] = __ARM_FEATURE_QBIT;
#endif
#ifdef __ARM_FEATURE_SAT
  features[10] = __ARM_FEATURE_SAT;
#endif
  for (y = 0; y < NUM_FEATURES; ++y)
    if (feature_matrix[arch][y] != features[y])
      {
#ifdef INTERNAL_DEBUG
	printf ("%d, %d, %d, %d\n", arch, y, feature_matrix[arch][y], features[y]);
#endif
        abort ();
      }
  return 0;
}

