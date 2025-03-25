/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v4t_thumb_ok } */
/* { dg-options "-mthumb" } */
/* { dg-add-options arm_arch_v4t } */

#define NEED_ARM_ARCH
#define VALUE_ARM_ARCH 4

#define NEED_ARM_ARCH_ISA_ARM
#define VALUE_ARM_ARCH_ISA_ARM 1

#define NEED_ARM_ARCH_ISA_THUMB
#define VALUE_ARM_ARCH_ISA_THUMB 1

#include "ftest-support.h"
