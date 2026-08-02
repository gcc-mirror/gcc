/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-options "-O2 -fno-ipa-icf" } */
/* { dg-add-options arm_arch_v7a } */

#include "../aarch64/atomic-comp-swap-release-acquire.x"

/* { dg-final { scan-assembler-not "ldaex" } } */
/* { dg-final { scan-assembler-not "stlex" } } */
/* { dg-final { scan-assembler-times "dmb\tish" 8 } } */
