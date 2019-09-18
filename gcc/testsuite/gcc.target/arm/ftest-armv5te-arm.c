/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv5te" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-mthumb" } { "" } } */
/* { dg-require-effective-target arm_arch_v5te_arm_ok } */
/* { dg-options "-marm" } */
/* { dg-add-options arm_arch_v5te } */

#define NEED_ARM_ARCH
#define VALUE_ARM_ARCH 5

#define NEED_ARM_ARCH_ISA_ARM
#define VALUE_ARM_ARCH_ISA_ARM 1

#define NEED_ARM_ARCH_ISA_THUMB
#define VALUE_ARM_ARCH_ISA_THUMB 1

#define NEED_ARM_FEATURE_CLZ
#define VALUE_ARM_FEATURE_CLZ 1

#define NEED_ARM_FEATURE_DSP
#define VALUE_ARM_FEATURE_DSP 1

#define NEED_ARM_FEATURE_QBIT
#define VALUE_ARM_FEATURE_QBIT 1

#include "ftest-support.h"
