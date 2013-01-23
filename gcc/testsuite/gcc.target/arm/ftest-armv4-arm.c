/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv4" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-mthumb" } { "" } } */
/* { dg-options "-marm" } */
/* { dg-add-options arm_arch_v4 } */

#define NEED_ARM_ARCH
#define VALUE_ARM_ARCH 4

#define NEED_ARM_ARCH_ISA_ARM
#define VALUE_ARM_ARCH_ISA_ARM 1

#include "ftest-support.h"


