/* { dg-do run } */
/* { dg-options "-mips3d forbid_cpu=octeon.* (REQUIRES_STDLIB)" } */

/* Test MIPS-3D builtin functions */
#include <stdlib.h>
#include <stdio.h>

typedef float v2sf __attribute__ ((vector_size(8)));

NOMIPS16 int main ()
{
  int little_endian;
  v2sf a, b, c, d;
  float f1, f2, f3, f4, f5, f6;
  double d1, d2, d3, d4, d5, d6, d7, d8, d9;
  v2sf ps1, ps2, ps3, ps4, ps5, ps6;

  union { long long ll; int i[2]; } endianness_test;
  endianness_test.ll = 1;
  little_endian = endianness_test.i[0];

  /* addr.ps */
  a = (v2sf) {12, 34};
  b = (v2sf) {45, 67};
  c = __builtin_mips_addr_ps (a, b);
  if (little_endian)
    d = (v2sf) {112, 46};
  else
    d = (v2sf) {46, 112};

  if (!__builtin_mips_all_c_eq_ps(c, d))
     abort ();

  /* mulr.ps */
  a = (v2sf) {12, 34};
  b = (v2sf) {45, 67};
  c = __builtin_mips_mulr_ps (a, b);
  if (little_endian)
    d = (v2sf) {3015, 408};
  else
    d = (v2sf) {408, 3015};

  if (!__builtin_mips_all_c_eq_ps(c, d))
     abort ();

  /* cvt.pw.ps */
  a = (v2sf) {12345.34, 67890.45};
  b = __builtin_mips_cvt_pw_ps (a);

  /* cvt.ps.pw */
  c = __builtin_mips_cvt_ps_pw (b);
  d = (v2sf) {12345.0, 67890.0};

  if (!__builtin_mips_all_c_eq_ps(c, d))
     abort ();

  /* recip1.s recip2.s */
  f1 = 40;
  f2 = __builtin_mips_recip1_s (f1);
  f3 = __builtin_mips_recip2_s (f2, f1);
  f4 = f2 + f2 * f3;
  f5 = 0.025;

  if (f4 != f5)
    abort ();

  /* recip1.d recip2.d */
  d1 = 80;
  d2 = __builtin_mips_recip1_d (d1);
  d3 = __builtin_mips_recip2_d (d2, d1);
  d4 = d2 + d2 * d3;
  d5 = __builtin_mips_recip2_d (d4, d1);
  d6 = d4 + d4 * d5;
  d7 = 0.0125;

  if (d6 != d7)
    abort ();

  /* recip1.ps recip2.ps */
  ps1 = (v2sf) {100, 200};
  ps2 = __builtin_mips_recip1_ps (ps1);
  ps3 = __builtin_mips_recip2_ps (ps2, ps1);
  ps4 = ps2 + ps2 * ps3;
  ps5 = (v2sf) {0.01, 0.005};

  if (!__builtin_mips_all_c_eq_ps(ps4, ps5))
    abort ();

  /* rsqrt1.s rsqrt2.s */
  f1 = 400;
  f2 = __builtin_mips_rsqrt1_s (f1);
  f3 = f2 * f1;
  f4 = __builtin_mips_rsqrt2_s (f3, f2);
  f5 = f2 + f2 * f4;
  f6 = 0.05;

  if (f5 != f6)
    abort ();

  /* rsqrt1.d rsqrt2.d */
  d1 = 1600;
  d2 = __builtin_mips_rsqrt1_d (d1);
  d3 = d2 * d1;
  d4 = __builtin_mips_rsqrt2_d (d3, d2);
  d5 = d2 + d2 * d4;
  d6 = d1 * d5;
  d7 = __builtin_mips_rsqrt2_d (d6, d5);
  d8 = d5 + d5 * d7;
  d9 = 0.025;

  if (d8 != d9)
    abort ();

  /* rsqrt1.ps rsqrt2.ps */
  ps1 = (v2sf) {400, 100};
  ps2 = __builtin_mips_rsqrt1_ps (ps1);
  ps3 = ps2 * ps1;
  ps4 = __builtin_mips_rsqrt2_ps (ps3, ps2);
  ps5 = ps2 + ps2 * ps4;
  ps6 = (v2sf) {0.05, 0.1};

  if (!__builtin_mips_all_c_eq_ps(ps5, ps6))
     abort ();

  printf ("Test Passes\n");
  exit (0);
}
