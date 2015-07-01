/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8a } */

#include "../aarch64/sync-comp-swap.x"

/* { dg-final { scan-assembler-times "ldrex" 2 } } */
/* { dg-final { scan-assembler-times "stlex" 2 } } */
/* { dg-final { scan-assembler-times "dmb" 2 } } */
