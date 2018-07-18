/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */
/* { dg-final { scan-assembler "adc" } } */
/* { dg-final { scan-assembler-not "jmp" } } */

typedef unsigned long u64;

typedef struct {
  u64 hi, lo;
} u128;

static inline u128 add_u128 (u128 a, u128 b)
{
  a.hi += b.hi;
  a.lo += b.lo;
  if (a.lo < b.lo)
    a.hi++;

  return a;
}

extern u128 t1, t2, t3;

void foo (void)
{
  t1 = add_u128 (t1, t2);
  t1 = add_u128 (t1, t3);
}
