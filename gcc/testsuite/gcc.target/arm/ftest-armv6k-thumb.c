/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv6k" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" } { "" } } */
/* { dg-options "-mthumb" } */
/* { dg-add-options arm_arch_v6k } */

#define NEED_ARM_ARCH
#define VALUE_ARM_ARCH 6

#define NEED_ARM_ARCH_ISA_ARM
#define VALUE_ARM_ARCH_ISA_ARM 1

#define NEED_ARM_ARCH_ISA_THUMB
#define VALUE_ARM_ARCH_ISA_THUMB 1

#define NEED_ARM_FEATURE_UNALIGNED
#define VALUE_ARM_FEATURE_UNALIGNED 1

#include "ftest-support.h"
