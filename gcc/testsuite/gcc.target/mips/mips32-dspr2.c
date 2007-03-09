/* Test MIPS32 DSP REV 2 instructions */
/* { dg-do run { target mipsisa32r2*-*-* } } */
/* { dg-mips-options "-march=mips32r2 -mdspr2 -O2" } */

typedef signed char v4q7 __attribute__ ((vector_size(4)));
typedef signed char v4i8 __attribute__ ((vector_size(4)));
typedef short v2q15 __attribute__ ((vector_size(4)));
typedef short v2i16 __attribute__ ((vector_size(4)));
typedef int q31;
typedef int i32;
typedef unsigned int ui32;
typedef long long a64;

void abort (void);

void test_MIPS_DSPR2 (void);

int little_endian;

int main ()
{
  union { long long ll; int i[2]; } endianness_test;
  endianness_test.ll = 1;
  little_endian = endianness_test.i[0];

  test_MIPS_DSPR2 ();

  return 0;
}

void test_MIPS_DSPR2 ()
{
  v4q7 v4q7_a,v4q7_b,v4q7_c,v4q7_r,v4q7_s;
  v4i8 v4i8_a,v4i8_b,v4i8_c,v4i8_r,v4i8_s;
  v2q15 v2q15_a,v2q15_b,v2q15_c,v2q15_r,v2q15_s;
  v2i16 v2i16_a,v2i16_b,v2i16_c,v2i16_r,v2i16_s;
  q31 q31_a,q31_b,q31_c,q31_r,q31_s;
  i32 i32_a,i32_b,i32_c,i32_r,i32_s;
  ui32 ui32_a,ui32_b,ui32_c,ui32_r,ui32_s;
  a64 a64_a,a64_b,a64_c,a64_r,a64_s;

  int r,s;

  v4q7_a = (v4i8) {0x81, 0xff, 0x80, 0x23};
  v4q7_s = (v4i8) {0x7f, 0x01, 0x7f, 0x23};
  v4q7_r = __builtin_mips_absq_s_qb (v4q7_a);
  r = (int) v4q7_r;
  s = (int) v4q7_s;
  if (r != s)
    abort ();

  v2i16_a = (v2i16) {0xffff, 0x2468};
  v2i16_b = (v2i16) {0x1234, 0x1111};
  v2i16_s = (v2i16) {0x1233, 0x3579};
  v2i16_r = __builtin_mips_addu_ph (v2i16_a, v2i16_b);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  v2i16_a = (v2i16) {0xffff, 0x2468};
  v2i16_b = (v2i16) {0x1234, 0x1111};
  v2i16_s = (v2i16) {0xffff, 0x3579};
  v2i16_r = __builtin_mips_addu_s_ph (v2i16_a, v2i16_b);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x11, 0x22, 0x33, 0xff};
  v4i8_b = (v4i8) {0x11, 0x33, 0x99, 0xff};
  v4i8_s = (v4i8) {0x11, 0x2a, 0x66, 0xff};
  v4i8_r = __builtin_mips_adduh_qb (v4i8_a, v4i8_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x11, 0x22, 0x33, 0xff};
  v4i8_b = (v4i8) {0x11, 0x33, 0x99, 0xff};
  v4i8_s = (v4i8) {0x11, 0x2b, 0x66, 0xff};
  v4i8_r = __builtin_mips_adduh_r_qb (v4i8_a, v4i8_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  i32_a = 0x12345678;
  i32_b = 0x87654321;
  i32_s = 0x56784321;
  i32_r = __builtin_mips_append (i32_a, i32_b, 16);
  if (i32_r != i32_s)
    abort ();

  i32_a = 0x12345678;
  i32_b = 0x87654321;
  i32_s = 0x78876543;
  i32_r = __builtin_mips_balign (i32_a, i32_b, 3);
  if (i32_r != i32_s)
    abort ();

  __builtin_mips_wrdsp (0, 63);
  v4i8_a = (v4i8) {0x11, 0x22, 0x33, 0x44};
  v4i8_b = (v4i8) {0x11, 0x33, 0x33, 0x44};
  if (little_endian)
    i32_s = 0xd;
  else
    i32_s = 0xb;
  i32_r = __builtin_mips_cmpgdu_eq_qb (v4i8_a, v4i8_b);
  if (i32_r != i32_s)
    abort ();
  i32_r = __builtin_mips_rddsp (16);
  if (little_endian)
    i32_s = 0x0d000000;
  else
    i32_s = 0x0b000000;
  if (i32_r != i32_s)
    abort ();

  __builtin_mips_wrdsp (0, 63);
  v4i8_a = (v4i8) {0x11, 0x22, 0x33, 0x44};
  v4i8_b = (v4i8) {0x11, 0x33, 0x33, 0x44};
  if (little_endian)
    i32_s = 0x2;
  else
    i32_s = 0x4;
  i32_r = __builtin_mips_cmpgdu_lt_qb (v4i8_a, v4i8_b);
  if (i32_r != i32_s)
    abort ();
  i32_r = __builtin_mips_rddsp (16);
  if (little_endian)
    i32_s = 0x02000000;
  else
    i32_s = 0x04000000;
  if (i32_r != i32_s)
    abort ();

  __builtin_mips_wrdsp (0, 63);
  v4i8_a = (v4i8) {0x11, 0x22, 0x33, 0x54};
  v4i8_b = (v4i8) {0x11, 0x33, 0x33, 0x44};
  if (little_endian)
    i32_s = 0x7;
  else
    i32_s = 0xe;
  i32_r = __builtin_mips_cmpgdu_le_qb (v4i8_a, v4i8_b);
  if (i32_r != i32_s)
    abort ();
  i32_r = __builtin_mips_rddsp (16);
  if (little_endian)
    i32_s = 0x07000000;
  else
    i32_s = 0x0e000000;
  if (i32_r != i32_s)
    abort ();

#ifndef __mips64
  a64_a = 0x12345678;
  v2i16_b = (v2i16) {0xffff, 0x1555};
  v2i16_c = (v2i16) {0x1234, 0x3322};
  a64_s = 0x1677088e;
  a64_r = __builtin_mips_dpa_w_ph (a64_a, v2i16_b, v2i16_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0x12345678;
  v2i16_b = (v2i16) {0xffff, 0x1555};
  v2i16_c = (v2i16) {0x1234, 0x3322};
  a64_s = 0x0df1a462;
  a64_r = __builtin_mips_dps_w_ph (a64_a, v2i16_b, v2i16_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0x12345678;
  i32_b = 0x80000000;
  i32_c = 0x11112222;
  a64_s = 0xF7776EEF12345678LL;
  a64_r = __builtin_mips_madd (a64_a, i32_b, i32_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0x12345678;
  ui32_b = 0x80000000;
  ui32_c = 0x11112222;
  a64_s = 0x0888911112345678LL;
  a64_r = __builtin_mips_maddu (a64_a, ui32_b, ui32_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0x12345678;
  i32_b = 0x80000000;
  i32_c = 0x11112222;
  a64_s = 0x0888911112345678LL;
  a64_r = __builtin_mips_msub (a64_a, i32_b, i32_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0x12345678;
  ui32_b = 0x80000000;
  ui32_c = 0x11112222;
  a64_s = 0xF7776EEF12345678LL;
  a64_r = __builtin_mips_msubu (a64_a, ui32_b, ui32_c);
  if (a64_r != a64_s)
    abort ();
#endif

  v2i16_a = (v2i16) {0xffff, 0x2468};
  v2i16_b = (v2i16) {0x1234, 0x1111};
  v2i16_s = (v2i16) {0xedcc, 0x52e8};
  v2i16_r = __builtin_mips_mul_ph (v2i16_a, v2i16_b);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  v2i16_a = (v2i16) {0x8000, 0x7fff};
  v2i16_b = (v2i16) {0x1234, 0x1111};
  v2i16_s = (v2i16) {0x8000, 0x7fff};
  v2i16_r = __builtin_mips_mul_s_ph (v2i16_a, v2i16_b);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  q31_a = 0x80000000;
  q31_b = 0x80000000; 
  q31_s = 0x7fffffff;
  q31_r = __builtin_mips_mulq_rs_w (q31_a, q31_b);
  if (q31_r != q31_s)
    abort ();

  v2q15_a = (v2q15) {0xffff, 0x8000};
  v2q15_b = (v2q15) {0x1111, 0x8000};
  v2q15_s = (v2q15) {0xffff, 0x7fff};
  v2q15_r = __builtin_mips_mulq_s_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  q31_a = 0x00000002;
  q31_b = 0x80000000; 
  q31_s = 0xfffffffe;
  q31_r = __builtin_mips_mulq_s_w (q31_a, q31_b);
  if (q31_r != q31_s)
    abort ();

#ifndef __mips64
  a64_a = 0x19848419;
  v2i16_b = (v2i16) {0xffff, 0x8000};
  v2i16_c = (v2i16) {0x1111, 0x8000};
  if (little_endian)
    a64_s = 0x5984952a;
  else
    a64_s = 0xffffffffd9847308LL;
  a64_r = __builtin_mips_mulsa_w_ph (a64_a, v2i16_b, v2i16_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  i32_a = 0x80000000;
  i32_b = 0x11112222;
  a64_s = 0xF7776EEF00000000LL;
  a64_r = __builtin_mips_mult (i32_a, i32_b);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  ui32_a = 0x80000000;
  ui32_b = 0x11112222;
  a64_s = 0x888911100000000LL;
  a64_r = __builtin_mips_multu (ui32_a, ui32_b);
  if (a64_r != a64_s)
    abort ();
#endif

  v2i16_a = (v2i16) {0x1234, 0x5678};
  v2i16_b = (v2i16) {0x2233, 0x5566};
  if (little_endian)
    v4i8_s = (v4i8) {0x33, 0x66, 0x34, 0x78};
  else
    v4i8_s = (v4i8) {0x34, 0x78, 0x33, 0x66};
  v4i8_r = __builtin_mips_precr_qb_ph (v2i16_a, v2i16_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  i32_a = 0x12345678;
  i32_b = 0x33334444;
  if (little_endian)
    v2i16_s = (v2i16) {0x3444, 0x4567};
  else
    v2i16_s = (v2i16) {0x4567, 0x3444};
  v2i16_r = __builtin_mips_precr_sra_ph_w (i32_a, i32_b, 4);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  i32_a = 0x12345678;
  i32_b = 0x33334444;
  if (little_endian)
    v2i16_s = (v2i16) {0x3444, 0x4568};
  else
    v2i16_s = (v2i16) {0x4568, 0x3444};
  v2i16_r = __builtin_mips_precr_sra_r_ph_w (i32_a, i32_b, 4);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  i32_a = 0x12345678;
  i32_b = 0x87654321;
  i32_s = 0x43211234;
  i32_r = __builtin_mips_prepend (i32_a, i32_b, 16);
  if (i32_r != i32_s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x45, 0x77, 0x99};
  v4i8_s = (v4i8) {0x9, 0x22, 0x3b, 0xcc};
  v4i8_r = __builtin_mips_shra_qb (v4i8_a, 1);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x45, 0x77, 0x99};
  v4i8_s = (v4i8) {0x9, 0x23, 0x3c, 0xcd};
  v4i8_r = __builtin_mips_shra_r_qb (v4i8_a, 1);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  i32_b = 1;  
  v4i8_a = (v4i8) {0x12, 0x45, 0x77, 0x99};
  v4i8_s = (v4i8) {0x9, 0x22, 0x3b, 0xcc};
  v4i8_r = __builtin_mips_shra_qb (v4i8_a, i32_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  i32_b = 1;  
  v4i8_a = (v4i8) {0x12, 0x45, 0x77, 0x99};
  v4i8_s = (v4i8) {0x9, 0x23, 0x3c, 0xcd};
  v4i8_r = __builtin_mips_shra_r_qb (v4i8_a, i32_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v2i16_a = (v2i16) {0x1357, 0x2468};
  v2i16_s = (v2i16) {0x0135, 0x0246};
  v2i16_r = __builtin_mips_shrl_ph (v2i16_a, 4);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  i32_b = 8;
  v2i16_a = (v2i16) {0x1357, 0x2468};
  v2i16_s = (v2i16) {0x0013, 0x0024};
  v2i16_r = __builtin_mips_shrl_ph (v2i16_a, i32_b);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  v2i16_a = (v2i16) {0x1357, 0x4455};
  v2i16_b = (v2i16) {0x3333, 0x4444};
  v2i16_s = (v2i16) {0xe024, 0x0011};
  v2i16_r = __builtin_mips_subu_ph (v2i16_a, v2i16_b);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  v2i16_a = (v2i16) {0x1357, 0x4455};
  v2i16_b = (v2i16) {0x3333, 0x4444};
  v2i16_s = (v2i16) {0x0000, 0x0011};
  v2i16_r = __builtin_mips_subu_s_ph (v2i16_a, v2i16_b);
  r = (int) v2i16_r;
  s = (int) v2i16_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x33 ,0x44, 0x55, 0x66};
  v4i8_b = (v4i8) {0x99 ,0x15, 0x85, 0xff};
  v4i8_s = (v4i8) {0xcd ,0x17, 0xe8, 0xb3};
  v4i8_r = __builtin_mips_subuh_qb (v4i8_a, v4i8_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x33 ,0x44, 0x55, 0x66};
  v4i8_b = (v4i8) {0x99 ,0x15, 0x85, 0xff};
  v4i8_s = (v4i8) {0xcd ,0x18, 0xe8, 0xb4};
  v4i8_r = __builtin_mips_subuh_r_qb (v4i8_a, v4i8_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x3334, 0x4444};
  v2q15_b = (v2q15) {0x1111, 0x2222};
  v2q15_s = (v2q15) {0x2222, 0x3333};
  v2q15_r = __builtin_mips_addqh_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x3334, 0x4444};
  v2q15_b = (v2q15) {0x1111, 0x2222};
  v2q15_s = (v2q15) {0x2223, 0x3333};
  v2q15_r = __builtin_mips_addqh_r_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  q31_a = 0x11111112;
  q31_b = 0x99999999;
  q31_s = 0xd5555555;
  q31_r = __builtin_mips_addqh_w (q31_a, q31_b);
  if (q31_r != q31_s)
    abort ();

  q31_a = 0x11111112;
  q31_b = 0x99999999;
  q31_s = 0xd5555556;
  q31_r = __builtin_mips_addqh_r_w (q31_a, q31_b);
  if (q31_r != q31_s)
    abort ();

  v2q15_a = (v2q15) {0x3334, 0x4444};
  v2q15_b = (v2q15) {0x1111, 0x2222};
  v2q15_s = (v2q15) {0x1111, 0x1111};
  v2q15_r = __builtin_mips_subqh_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x3334, 0x4444};
  v2q15_b = (v2q15) {0x1111, 0x2222};
  v2q15_s = (v2q15) {0x1112, 0x1111};
  v2q15_r = __builtin_mips_subqh_r_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  q31_a = 0x11111112;
  q31_b = 0x99999999;
  q31_s = 0x3bbbbbbc;
  q31_r = __builtin_mips_subqh_w (q31_a, q31_b);
  if (q31_r != q31_s)
    abort ();

  q31_a = 0x11111112;
  q31_b = 0x99999999;
  q31_s = 0x3bbbbbbd;
  q31_r = __builtin_mips_subqh_r_w (q31_a, q31_b);
  if (q31_r != q31_s)
    abort ();

#ifndef __mips64
  a64_a = 0x1111222212345678LL;
  v2i16_b = (v2i16) {0x1, 0x2};
  v2i16_c = (v2i16) {0x3, 0x4};
  a64_s = 0x1111222212345682LL;
  a64_r = __builtin_mips_dpax_w_ph (a64_a, v2i16_b, v2i16_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0x9999111112345678LL;
  v2i16_b = (v2i16) {0x1, 0x2};
  v2i16_c = (v2i16) {0x3, 0x4};
  a64_s = 0x999911111234566eLL;
  a64_r = __builtin_mips_dpsx_w_ph (a64_a, v2i16_b, v2i16_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0x70000000;
  v2q15_b = (v2q15) {0x4000, 0x2000};
  v2q15_c = (v2q15) {0x2000, 0x4000};
  a64_s = 0x98000000;
  a64_r = __builtin_mips_dpaqx_s_w_ph (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0x70000000;
  v2q15_b = (v2q15) {0x4000, 0x2000};
  v2q15_c = (v2q15) {0x2000, 0x4000};
  a64_s = 0x7fffffff;
  a64_r = __builtin_mips_dpaqx_sa_w_ph (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0x70000000;
  v2q15_b = (v2q15) {0x4000, 0x2000};
  v2q15_c = (v2q15) {0x2000, 0x4000};
  a64_s = 0x48000000;
  a64_r = __builtin_mips_dpsqx_s_w_ph (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();
#endif

#ifndef __mips64
  a64_a = 0xFFFFFFFF80000000LL;
  v2q15_b = (v2q15) {0x4000, 0x2000};
  v2q15_c = (v2q15) {0x2000, 0x4000};
  a64_s = 0xFFFFFFFF80000000LL;
  a64_r = __builtin_mips_dpsqx_sa_w_ph (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();
#endif
}
