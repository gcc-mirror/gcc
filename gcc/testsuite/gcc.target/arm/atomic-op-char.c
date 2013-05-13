/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8a } */

#include "../aarch64/atomic-op-char.x"

/* { dg-final { scan-assembler-times "ldrexb\tr\[0-9\]+, \\\[r\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-times "strexb\t...?, r\[0-9\]+, \\\[r\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-not "dmb" } } */
