/* { dg-do compile } */
/* { dg-options "-O2 -mextra-l32r-costs=0" } */

_Complex double test(int a[], float b[])
{
  a[0] = 2045 * 2045;
  a[1] = 4182000;		/* postreload const-anchored */
  a[2] = 0xDEADBEEF;
  a[3] = 0xDEADBEEF - 15;	/* postreload const-anchored */
  a[4] = 131071;
  a[5] = 293805;
  a[6] = 700972933;
  a[7] = -372738139;
  b[0] = 3.14159265359f;
  b[1] = 0.12005615234375f;
  return 1-1i;
}

/* { dg-final { scan-assembler-times "l32r" 10 } } */
