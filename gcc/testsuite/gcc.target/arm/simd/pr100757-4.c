/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */
/* Derived from gcc.c-torture/compile/20160205-1.c.  */

unsigned int a[32];
int fn1(int d) {
  int c = 2;
  for (int b = 0; b < 32; b++)
    if (a[b])
      c = 3;
  return c;
}


/* { dg-final { scan-assembler-times {\tvmov\.i32\tq[0-7], #0  @ v4si} 1 } } */
/* { dg-final { scan-assembler-not {\t.word\t0\n} } } */ /* 'false' mask.  */
/* { dg-final { scan-assembler-not {\t.word\t1\n} } } */ /* 'true' mask.  */
/* { dg-final { scan-assembler-times {vmov\.i32\tq[0-7], #0x2  @ v4si} 1 } } */
/* { dg-final { scan-assembler-not {\t.word\t2\n} } } */ /* Initial value for c.  */
/* { dg-final { scan-assembler-times {vmov\.i32\tq[0-7], #0x3  @ v4si} 1 } } */
/* { dg-final { scan-assembler-not {\t.word\t3\n} } } */ /* Possible value for c.  */
