/* { dg-do compile } */
/* { dg-options "-O2 -mextra-l32r-costs=5" } */

_Complex double test(int a[], float b[])
{
  a[0] = 2045 * 2045;		/* method "square" */
  a[1] = 4182000;		/* postreload const-anchored */
  a[2] = 0xDEADBEEF;		/* method "32bits" */
  a[3] = 0xDEADBEEF - 15;	/* postreload const-anchored */
  a[4] = 131071;		/* method "lshr_m1" */
  a[5] = 293805;		/* method "16bits" */
  a[6] = 700972933;		/* method "32bits" */
  a[7] = -372738139;		/* method "32bits" */
  b[0] = 3.14159265359f;	/* method "32bits" */
  b[1] = 0.12005615234375f;	/* method "32bits" */
  return 1-1i;			/* method "16bits", method "16bits" */
}

/* { dg-final { scan-assembler-not "l32r" } } */
