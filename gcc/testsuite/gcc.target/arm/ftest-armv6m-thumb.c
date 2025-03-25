/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6m_ok } */
/* { dg-options "-mthumb" } */
/* { dg-add-options arm_arch_v6m } */

#define NEED_ARM_ARCH
#define VALUE_ARM_ARCH 6

#define NEED_ARM_ARCH_ISA_THUMB
#define VALUE_ARM_ARCH_ISA_THUMB 1

#define NEED_ARM_ARCH_PROFILE
#define VALUE_ARM_ARCH_PROFILE 'M'

#include "ftest-support.h"
