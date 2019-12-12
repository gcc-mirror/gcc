/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv6" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" } { "" } } */
/* { dg-require-effective-target arm_arch_v6_thumb_ok } */
/* { dg-options "-mthumb" } */
/* { dg-add-options arm_arch_v6 } */

#define NEED_ARM_ARCH
#define VALUE_ARM_ARCH 6

#define NEED_ARM_ARCH_ISA_ARM
#define VALUE_ARM_ARCH_ISA_ARM 1

#define NEED_ARM_ARCH_ISA_THUMB
#define VALUE_ARM_ARCH_ISA_THUMB 1

#include "ftest-support.h"
