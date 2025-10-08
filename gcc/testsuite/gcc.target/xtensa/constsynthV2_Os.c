/* { dg-do compile } */
/* { dg-options "-Os -mabi=windowed" } */

_Complex double test(int a[], float b[])
{
  a[0] = 2045 * 2045;		/* method "square", but not unique */
  a[1] = 4182000;		/* postreload const-anchored */
  a[2] = 0xDEADBEEF;
  a[3] = 0xDEADBEEF - 15;	/* postreload const-anchored */
  a[4] = 131071;		/* method "lshr_m1", but not unique */
  a[5] = 293805;
  a[6] = 700972933;
  a[7] = -372738139;
  asm volatile ("# clobbers":::"a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15");
  a[8] = 2045 * 2045;		/* method "square", but not unique */
  a[9] = 131071;		/* method "lshr_m1", but not unique */
  b[0] = 3.14159265359f;
  b[1] = 0.12005615234375f;
  return 1-1i;			/* method "16bits", method "16bits" */
}

/* { dg-final { scan-assembler-times "l32r" 10 } } */
/* { dg-final { scan-assembler-times ".literal " 8 } } */
