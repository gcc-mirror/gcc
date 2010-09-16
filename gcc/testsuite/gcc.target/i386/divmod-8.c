/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -m8bit-idiv" } */

extern void foo (long long, long long, long long, long long,
		 long long, long long);

void
bar (long long x, long long y)
{
  foo (0, 0, 0, 0, x / y, x % y);
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-times "idivq" 1 } } */
