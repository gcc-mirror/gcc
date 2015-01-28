/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8a } */

#include "../aarch64/atomic-op-consume.x"

/* To workaround Bugzilla 59448 issue, a request for __ATOMIC_CONSUME is always
   promoted to __ATOMIC_ACQUIRE, implemented as MEMMODEL_ACQUIRE.  This causes
   "LDAEX" to be generated instead of "LDREX".  */
/* { dg-final { scan-assembler-times "ldaex\tr\[0-9\]+, \\\[r\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-times "strex\t...?, r\[0-9\]+, \\\[r\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-not "dmb" } } */
