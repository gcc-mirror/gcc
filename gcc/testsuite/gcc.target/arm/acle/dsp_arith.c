/* { dg-do compile } */
/* { dg-require-effective-target arm_qbit_ok } */
/* { dg-add-options arm_qbit  } */

#include <arm_acle.h>

int32_t
test_qadd (int32_t a, int32_t b)
{
  return __qadd (a, b);
}

int32_t
test_qdbl (int32_t a)
{
  return __qdbl(a);
}

/* { dg-final { scan-assembler-times "qadd\t...?, ...?, ...?" 2 } } */

int32_t
test_qsub (int32_t a, int32_t b)
{
  return __qsub (a, b);
}

/* { dg-final { scan-assembler-times "qsub\t...?, ...?, ...?" 1 } } */
