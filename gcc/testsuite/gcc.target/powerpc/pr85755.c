/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O1" } */

void
preinc (long *q, long n)
{
  long i;
  for (i = 0; i < n; i++)
    q[i] = i;
}

void
predec (long *q, long n)
{
  long i;
  for (i = n; i >= 0; i--)
    q[i] = i;
}

/* { dg-final { scan-assembler-times {\mstwu\M} 2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstdu\M} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-not {\mstfdu\M} } } */
