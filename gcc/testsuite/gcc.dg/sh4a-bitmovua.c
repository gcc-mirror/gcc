/* Verify that we generate movua to load unaligned 32-bit values.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-times "\tmovua\\.l\t" 6 } } */

#ifdef __SH4A__
/* Aligned.  */
struct s0 { long long d : 32; } x0;
long long f0() {
  return x0.d;
}

/* Unaligned load.  */
struct s1 { long long c : 8; long long d : 32; } x1;
long long f1() {
  return x1.d;
}

/* Unaligned load.  */
struct s2 { long long c : 16; long long d : 32; } x2;
long long f2() {
  return x2.d;
}

/* Unaligned load.  */
struct s3 { long long c : 24; long long d : 32; } x3;
long long f3() {
  return x3.d;
}

/* Aligned.  */
struct s4 { long long c : 32; long long d : 32; } x4;
long long f4() {
  return x4.d;
}

/* Aligned.  */
struct u0 { unsigned long long d : 32; } y0;
unsigned long long g0() {
  return y0.d;
}

/* Unaligned load.  */
struct u1 { long long c : 8; unsigned long long d : 32; } y1;
unsigned long long g1() {
  return y1.d;
}

/* Unaligned load.  */
struct u2 { long long c : 16; unsigned long long d : 32; } y2;
unsigned long long g2() {
  return y2.d;
}

/* Unaligned load.  */
struct u3 { long long c : 24; unsigned long long d : 32; } y3;
unsigned long long g3() {
  return y3.d;
}

/* Aligned.  */
struct u4 { long long c : 32; unsigned long long d : 32; } y4;
unsigned long long g4() {
  return y4.d;
}
#else
asm ("movua.l\t");
asm ("movua.l\t");
asm ("movua.l\t");
asm ("movua.l\t");
asm ("movua.l\t");
asm ("movua.l\t");
#endif
