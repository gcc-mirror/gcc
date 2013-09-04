/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8a } */

#include "../aarch64/atomic-comp-swap-release-acquire.x"

/* { dg-final { scan-assembler-times "ldaex\tr\[0-9\]+, \\\[r\[0-9\]+\\\]" 4 } } */
/* { dg-final { scan-assembler-times "stlex" 4 } } */
/* { dg-final { scan-assembler-not "dmb" } } */
