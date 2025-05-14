/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6z_arm_ok } */
/* { dg-options "-marm" } */
/* { dg-add-options arm_arch_v6z } */

#define NEED_ARM_ARCH
#define VALUE_ARM_ARCH 6

#define NEED_ARM_ARCH_ISA_ARM
#define VALUE_ARM_ARCH_ISA_ARM 1

#define NEED_ARM_ARCH_ISA_THUMB
#define VALUE_ARM_ARCH_ISA_THUMB 1

#define NEED_ARM_FEATURE_UNALIGNED
#define VALUE_ARM_FEATURE_UNALIGNED 1

#define NEED_ARM_FEATURE_LDREX
#define VALUE_ARM_FEATURE_LDREX 4

#define NEED_ARM_FEATURE_CLZ
#define VALUE_ARM_FEATURE_CLZ 1

#define NEED_ARM_FEATURE_DSP
#define VALUE_ARM_FEATURE_DSP 1

#define NEED_ARM_FEATURE_SIMD32
#define VALUE_ARM_FEATURE_SIMD32 1

#define NEED_ARM_FEATURE_QBIT
#define VALUE_ARM_FEATURE_QBIT 1

#define NEED_ARM_FEATURE_SAT
#define VALUE_ARM_FEATURE_SAT 1

#include "ftest-support.h"
