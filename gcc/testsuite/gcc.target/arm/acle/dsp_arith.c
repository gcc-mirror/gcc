/* { dg-do compile } */
/* { dg-require-effective-target arm_dsp_ok } */
/* { dg-add-options arm_dsp } */

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

int32_t
test_smlabb (int32_t a, int32_t b, int32_t c)
{
  return __smlabb (a, b, c);
}

/* { dg-final { scan-assembler-times "smlabb\t...?, ...?, ...?, ...?" 1 } } */

int32_t
test_smlabt (int32_t a, int32_t b, int32_t c)
{
  return __smlabt (a, b, c);
}

int32_t
test_smlatb (int32_t a, int32_t b, int32_t c)
{
  return __smlatb (a, b, c);
}

/* { dg-final { scan-assembler-times "smlatb\t...?, ...?, ...?, ...?" 2 } } */

int32_t
test_smlatt (int32_t a, int32_t b, int32_t c)
{
  return __smlatt (a, b, c);
}

/* { dg-final { scan-assembler-times "smlatt\t...?, ...?, ...?, ...?" 1 } } */

int32_t
test_smlawb (int32_t a, int32_t b, int32_t c)
{
  return __smlawb (a, b, c);
}

/* { dg-final { scan-assembler-times "smlawb\t...?, ...?, ...?, ...?" 1 } } */

int32_t
test_smlawt (int32_t a, int32_t b, int32_t c)
{
  return __smlawt (a, b, c);
}

/* { dg-final { scan-assembler-times "smlawt\t...?, ...?, ...?, ...?" 1 } } */
