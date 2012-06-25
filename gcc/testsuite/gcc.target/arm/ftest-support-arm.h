#include "ftest-support.h"

  /*Feature matrix layout:
    __ARM_ARCH
    __ARM_ARCH_ISA_ARM
    __ARM_ARCH_ISA_THUMB
    __ARM_ARCH_PROFILE
    __ARM_FEATURE_UNALIGNED
    __ARM_FEATURE_LDREX
    __ARM_FEATURE_CLZ
    __ARM_FEATURE_DSP
    __ARM_FEATURE_SIMD32
    __ARM_FEATURE_QBIT
    __ARM_FEATURE_SAT
  */
int feature_matrix[ARCH_COUNT][NUM_FEATURES] =
  {{4, 1, 0, 0,   0, 0,  0, 0, 0, 0, 0},  /* ARCH_V4.  */
   {4, 1, 1, 0,   0, 0,  0, 0, 0, 0, 0},  /* ARCH_V4T.  */
   {5, 1, 1, 0,   0, 0,  1, 0, 0, 0, 0},  /* ARCH_V5T.  */
   {5, 1, 1, 0,   0, 0,  1, 1, 0, 1, 0},  /* ARCH_V5TE.  */
   {6, 1, 1, 0,   1, 4,  1, 1, 1, 1, 1},  /* ARCH_V6.  */
   {6, 1, 1, 0,   1, 15, 1, 1, 1, 1, 1},  /* ARCH_V6K.  */
   {6, 1, 2, 0,   1, 4,  1, 1, 1, 1, 1},  /* ARCH_V6T2.  */
   {6, 1, 1, 0,   1, 4,  1, 1, 1, 1, 1},  /* ARCH_V6Z.  */
   {6, 0, 1, 'M', 0, 0,  1, 0, 0, 0, 0},  /* ARCH_V6M.  */
   {7, 1, 2, 'A', 1, 15, 1, 1, 1, 1, 1},  /* ARCH_V7A.  */
   {7, 1, 2, 'R', 1, 15, 1, 1, 1, 1, 1},  /* ARCH_V7R.  */
   {7, 0, 2, 'M', 1, 7,  1, 0, 0, 1, 1},  /* ARCH_V7M.  */
   {7, 0, 2, 'M', 1, 7,  1, 1, 0, 1, 1}}; /* ARCH_V7EM.  */
