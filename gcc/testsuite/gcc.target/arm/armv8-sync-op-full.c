/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8a } */

#include "../aarch64/sync-op-full.x"

/* { dg-final { scan-assembler-times "ldrex" 12 } } */
/* { dg-final { scan-assembler-times "stlex" 12 } } */
/* { dg-final { scan-assembler-times "dmb" 12 } } */
