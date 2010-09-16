/* { dg-do compile } */
/* { dg-options "-O2 -m8bit-idiv" } */
/* { dg-require-effective-target lp64 } */

extern void abort (void);

void
test (unsigned long long x, unsigned long long y,
      unsigned long long q, unsigned long long r)
{
  if ((x / y) != q || (x % y) != r)
    abort ();
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-times "divq" 1 } } */
