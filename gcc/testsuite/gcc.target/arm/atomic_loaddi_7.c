/* { dg-do compile } */
/* { dg-options "-std=c11 -O" } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-add-options arm_arch_v7a } */

#include "atomic_loaddi_seq_cst.x"

/* { dg-final { scan-assembler-times "ldrexd\tr\[0-9\]+, r\[0-9\]+, \\\[r\[0-9\]+\\\]" 1 } } */
/* { dg-final { scan-assembler-times "dmb\tish" 2 } } */
