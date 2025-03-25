/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v5t_arm_ok } */
/* { dg-options "-marm" } */
/* { dg-add-options arm_arch_v5t } */

#define NEED_ARM_ARCH
#define VALUE_ARM_ARCH 5

#define NEED_ARM_ARCH_ISA_ARM
#define VALUE_ARM_ARCH_ISA_ARM 1

#define NEED_ARM_ARCH_ISA_THUMB
#define VALUE_ARM_ARCH_ISA_THUMB 1

#define NEED_ARM_FEATURE_CLZ
#define VALUE_ARM_FEATURE_CLZ 1

#include "ftest-support.h"
