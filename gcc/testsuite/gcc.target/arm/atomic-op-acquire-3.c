/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8m_base_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8m_base } */

#include "../aarch64/atomic-op-acquire.x"

/* { dg-final { scan-assembler-times "ldaex\tr\[0-9\]+, \\\[r\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-times "strex\t...?, r\[0-9\]+, \\\[r\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-not "dmb" } } */
