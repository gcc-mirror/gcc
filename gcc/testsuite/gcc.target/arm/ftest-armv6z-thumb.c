/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6z_thumb_ok } */
/* { dg-options "-mthumb" } */
/* { dg-add-options arm_arch_v6z } */

#define NEED_ARM_ARCH
#define VALUE_ARM_ARCH 6

#define NEED_ARM_ARCH_ISA_ARM
#define VALUE_ARM_ARCH_ISA_ARM 1

#define NEED_ARM_ARCH_ISA_THUMB
#define VALUE_ARM_ARCH_ISA_THUMB 1

/* Not in the Thumb ISA, but does exist in Arm state.  A call to the library
   function should result in using that instruction in Arm state.  */
#define NEED_ARM_FEATURE_CLZ
#define VALUE_ARM_FEATURE_CLZ 1

#include "ftest-support.h"
