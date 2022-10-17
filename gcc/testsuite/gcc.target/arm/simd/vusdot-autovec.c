/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_2a_i8mm_ok } */
/* { dg-options "-O3" } */
/* { dg-add-options arm_v8_2a_i8mm } */

#define N 480
#define SIGNEDNESS_1 unsigned
#define SIGNEDNESS_2 signed
#define SIGNEDNESS_3 signed
#define SIGNEDNESS_4 unsigned

SIGNEDNESS_1 int __attribute__ ((noipa))
f (SIGNEDNESS_1 int res, SIGNEDNESS_3 char *restrict a,
   SIGNEDNESS_4 char *restrict b)
{
  for (__INTPTR_TYPE__ i = 0; i < N; ++i)
    {
      int av = a[i];
      int bv = b[i];
      SIGNEDNESS_2 short mult = av * bv;
      res += mult;
    }
  return res;
}

SIGNEDNESS_1 int __attribute__ ((noipa))
g (SIGNEDNESS_1 int res, SIGNEDNESS_3 char *restrict b,
   SIGNEDNESS_4 char *restrict a)
{
  for (__INTPTR_TYPE__ i = 0; i < N; ++i)
    {
      int av = a[i];
      int bv = b[i];
      SIGNEDNESS_2 short mult = av * bv;
      res += mult;
    }
  return res;
}

/* { dg-final { scan-assembler-times {vusdot.s8} 2 { target { arm-*-*-gnueabihf } } } } */
