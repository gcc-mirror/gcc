/* { dg-do compile } */
/* { dg-options "-O2 -dp" } */
/* { dg-require-effective-target lp64 } */

void
f (int x0, int *x1, long x2)
{
  asm volatile ("// foo %0 %1"
		:: "r,w" (x0), "Q,m" (x1[x2]));
}

/* { dg-final { scan-assembler-times "add_lsl_di" 1 } } */
