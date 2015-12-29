/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -m8bit-idiv" } */

extern void foo (unsigned long long, unsigned long long,
		 unsigned long long, unsigned long long,
		 unsigned long long, unsigned long long);

void
bar (unsigned long long x, unsigned long long y)
{
  foo (0, 0, 0, 0, x / y, x % y);
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-times "divq" 1 } } */
