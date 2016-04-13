/* { dg-do compile } */
/* { dg-options "-std=c11 -O" } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-add-options arm_arch_v8a } */

#include "atomic_loaddi_relaxed.x"

/* { dg-final { scan-assembler-times "ldrd\tr\[0-9\]+, r\[0-9\]+, \\\[r\[0-9\]+\\\]" 1 } } */
/* { dg-final { scan-assembler-not "dmb\tish" } } */