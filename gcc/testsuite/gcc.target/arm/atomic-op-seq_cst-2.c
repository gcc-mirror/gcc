/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8m_main_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8m_main } */

#include "../aarch64/atomic-op-seq_cst.x"

/* { dg-final { scan-assembler-times "ldaex\tr\[0-9\]+, \\\[r\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-times "stlex\t...?, r\[0-9\]+, \\\[r\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-not "dmb" } } */
