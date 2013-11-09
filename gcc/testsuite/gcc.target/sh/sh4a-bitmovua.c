/* Verify that we generate movua to load unaligned 32-bit values on SH4A.  */
/* { dg-do run }  */
/* { dg-options "-O1 -save-temps -fno-inline" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m4a*" } }  */
/* { dg-final { scan-assembler-times "movua.l" 6 } } */

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
struct u0 { unsigned long long d : 32; } y_0;
unsigned long long g0() {
  return y_0.d;
}

/* Unaligned load.  */
struct u1 { long long c : 8; unsigned long long d : 32; } y_1;
unsigned long long g1() {
  return y_1.d;
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

#include <assert.h>

int
main (void)
{
  x1.d = 0x12345678;
  assert (f1 () == 0x12345678);

  x2.d = 0x12345678;
  assert (f2 () == 0x12345678);

  x3.d = 0x12345678;
  assert (f3 () == 0x12345678);

  y_1.d = 0x12345678;
  assert (g1 () == 0x12345678);

  y2.d = 0x12345678;
  assert (g2 () == 0x12345678);

  y3.d = 0x12345678;
  assert (g3 () == 0x12345678);

  return 0;
}
