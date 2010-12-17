/* Test MIPS32 DSP instructions */
/* { dg-do run } */
/* { dg-options "-mdsp -O2" } */

#include <stdlib.h>
#include <stdio.h>

typedef signed char v4i8 __attribute__ ((vector_size(4)));
typedef short v2q15 __attribute__ ((vector_size(4)));

typedef int q31;
typedef int i32;
typedef unsigned int ui32;
typedef long long a64;

NOMIPS16 void test_MIPS_DSP (void);

char array[100];
int little_endian;

int main ()
{
  int i;

  union { long long ll; int i[2]; } endianness_test;
  endianness_test.ll = 1;
  little_endian = endianness_test.i[0];

  for (i = 0; i < 100; i++)
    array[i] = i;

  test_MIPS_DSP ();

  exit (0);
}

NOMIPS16 v2q15 add_v2q15 (v2q15 a, v2q15 b)
{
  return __builtin_mips_addq_ph (a, b);
}

NOMIPS16 v4i8 add_v4i8 (v4i8 a, v4i8 b)
{
  return __builtin_mips_addu_qb (a, b);
}

NOMIPS16 v2q15 sub_v2q15 (v2q15 a, v2q15 b)
{
  return __builtin_mips_subq_ph (a, b);
}

NOMIPS16 v4i8 sub_v4i8 (v4i8 a, v4i8 b)
{
  return __builtin_mips_subu_qb (a, b);
}

NOMIPS16 void test_MIPS_DSP ()
{
  v4i8 v4i8_a,v4i8_b,v4i8_c,v4i8_r,v4i8_s;
  v2q15 v2q15_a,v2q15_b,v2q15_c,v2q15_r,v2q15_s;
  q31 q31_a,q31_b,q31_c,q31_r,q31_s;
  i32 i32_a,i32_b,i32_c,i32_r,i32_s;
  ui32 ui32_a,ui32_b,ui32_c;
  a64 a64_a,a64_b,a64_c,a64_r,a64_s;

  void *ptr_a;
  int r,s;
  long long lr,ls;

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_b = (v2q15) {0x6f89, 0x1111};
  v2q15_s = (v2q15) {0x81bd, 0x6789};
  v2q15_r = add_v2q15 (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_b = (v2q15) {0x6f89, 0x1111};
  v2q15_s = (v2q15) {0x7fff, 0x6789};
  v2q15_r = __builtin_mips_addq_s_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  q31_a = 0x70000000;
  q31_b = 0x71234567;
  q31_s = 0x7fffffff;
  q31_r = __builtin_mips_addq_s_w (q31_a, q31_b);
  if (q31_r != q31_s)
    abort ();

  v4i8_a = (v4i8) {0xf2, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0xff, 0x89, 0x11, 0x11};
  v4i8_s = (v4i8) {0xf1, 0xbd, 0x67, 0x89};
  v4i8_r = add_v4i8 (v4i8_a, v4i8_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0xf2, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0xff, 0x89, 0x11, 0x11};
  v4i8_s = (v4i8) {0xff, 0xbd, 0x67, 0x89};
  v4i8_r = __builtin_mips_addu_s_qb (v4i8_a, v4i8_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_b = (v2q15) {0x6f89, 0x1111};
  v2q15_s = (v2q15) {0xa2ab, 0x4567};
  v2q15_r = sub_v2q15 (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x8000, 0x5678};
  v2q15_b = (v2q15) {0x6f89, 0x1111};
  v2q15_s = (v2q15) {0x8000, 0x4567};
  v2q15_r = __builtin_mips_subq_s_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  q31_a = 0x70000000;
  q31_b = 0x71234567;
  q31_s = 0xfedcba99;
  q31_r = __builtin_mips_subq_s_w (q31_a, q31_b);
  if (q31_r != q31_s)
    abort ();

  v4i8_a = (v4i8) {0xf2, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0xff, 0x89, 0x11, 0x11};
  v4i8_s = (v4i8) {0xf3, 0xab, 0x45, 0x67};
  v4i8_r = sub_v4i8 (v4i8_a, v4i8_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0xf2, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0xff, 0x89, 0x11, 0x11};
  v4i8_s = (v4i8) {0x0, 0x0, 0x45, 0x67};
  v4i8_r = __builtin_mips_subu_s_qb (v4i8_a, v4i8_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  i32_a = 0xf5678900;
  i32_b = 0x7abcdef0;
  i32_s = 0x702467f0;
  i32_r = __builtin_mips_addsc (i32_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  i32_a = 0x75678900;
  i32_b = 0x7abcdef0;
  i32_s = 0xf02467f1;
  i32_r = __builtin_mips_addwc (i32_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  i32_a = 0;
  i32_b = 0x00000901;
  i32_s = 9;
  i32_r = __builtin_mips_modsub (i32_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  v4i8_a = (v4i8) {0xf2, 0x34, 0x56, 0x78};
  i32_s = 0x1f4;
  i32_r = __builtin_mips_raddu_w_qb (v4i8_a);
  if (i32_r != i32_s)
    abort ();

  v2q15_a = (v2q15) {0x8000, 0x8134};
  v2q15_s = (v2q15) {0x7fff, 0x7ecc};
  v2q15_r = __builtin_mips_absq_s_ph (v2q15_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  q31_a = (q31) 0x80000000;
  q31_s = (q31) 0x7fffffff;
  q31_r = __builtin_mips_absq_s_w (q31_a);
  if (q31_r != q31_s)
    abort ();

  v2q15_a = (v2q15) {0x9999, 0x5612};
  v2q15_b = (v2q15) {0x5612, 0x3333};
  if (little_endian)
    v4i8_s = (v4i8) {0x56, 0x33, 0x99, 0x56};
  else
    v4i8_s = (v4i8) {0x99, 0x56, 0x56, 0x33};
  v4i8_r = __builtin_mips_precrq_qb_ph (v2q15_a, v2q15_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  q31_a = 0x12348678;
  q31_b = 0x44445555;
  if (little_endian)
    v2q15_s = (v2q15) {0x4444, 0x1234};
  else
    v2q15_s = (v2q15) {0x1234, 0x4444};
  v2q15_r = __builtin_mips_precrq_ph_w (q31_a, q31_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  q31_a = 0x12348678;
  q31_b = 0x44445555;
  if (little_endian)
    v2q15_s = (v2q15) {0x4444, 0x1235};
  else
    v2q15_s = (v2q15) {0x1235, 0x4444};
  v2q15_r = __builtin_mips_precrq_rs_ph_w (q31_a, q31_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x9999, 0x5612};
  v2q15_b = (v2q15) {0x5612, 0x3333};
  if (little_endian)
    v4i8_s = (v4i8) {0xac, 0x66, 0x00, 0xac};
  else
    v4i8_s = (v4i8) {0x00, 0xac, 0xac, 0x66};
  v4i8_r = __builtin_mips_precrqu_s_qb_ph (v2q15_a, v2q15_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x3589, 0x4444};
  if (little_endian)
    q31_s = 0x44440000;
  else
    q31_s = 0x35890000;
  q31_r = __builtin_mips_preceq_w_phl (v2q15_a);
  if (q31_r != q31_s)
    abort ();

  v2q15_a = (v2q15) {0x3589, 0x4444};
  if (little_endian)
    q31_s = 0x35890000;
  else
    q31_s = 0x44440000;
  q31_r = __builtin_mips_preceq_w_phr (v2q15_a);
  if (q31_r != q31_s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x56, 0x56, 0x33};
  if (little_endian)
    v2q15_s = (v2q15) {0x2b00, 0x1980};
  else
    v2q15_s = (v2q15) {0x0900, 0x2b00};
  v2q15_r = __builtin_mips_precequ_ph_qbl (v4i8_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x56, 0x56, 0x33};
  if (little_endian)
    v2q15_s = (v2q15) {0x0900, 0x2b00};
  else
    v2q15_s = (v2q15) {0x2b00, 0x1980};
  v2q15_r = __builtin_mips_precequ_ph_qbr (v4i8_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x56, 0x56, 0x33};
  if (little_endian)
    v2q15_s = (v2q15) {0x2b00, 0x1980};
  else
    v2q15_s = (v2q15) {0x0900, 0x2b00};
  v2q15_r = __builtin_mips_precequ_ph_qbla (v4i8_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x56, 0x56, 0x33};
  if (little_endian)
    v2q15_s = (v2q15) {0x0900, 0x2b00};
  else
    v2q15_s = (v2q15) {0x2b00, 0x1980};
  v2q15_r = __builtin_mips_precequ_ph_qbra (v4i8_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x56, 0x56, 0x33};
  if (little_endian)
    v2q15_s = (v2q15) {0x56, 0x33};
  else
    v2q15_s = (v2q15) {0x12, 0x56};
  v2q15_r = __builtin_mips_preceu_ph_qbl (v4i8_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x56, 0x56, 0x33};
  if (little_endian)
    v2q15_s = (v2q15) {0x12, 0x56};
  else
    v2q15_s = (v2q15) {0x56, 0x33};
  v2q15_r = __builtin_mips_preceu_ph_qbr (v4i8_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x99, 0x56, 0x33};
  if (little_endian)
    v2q15_s = (v2q15) {0x99, 0x33};
  else
    v2q15_s = (v2q15) {0x12, 0x56};
  v2q15_r = __builtin_mips_preceu_ph_qbla (v4i8_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x99, 0x56, 0x33};
  if (little_endian)
    v2q15_s = (v2q15) {0x12, 0x56};
  else
    v2q15_s = (v2q15) {0x99, 0x33};
  v2q15_r = __builtin_mips_preceu_ph_qbra (v4i8_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0xf2, 0x34, 0x56, 0x78};
  v4i8_s = (v4i8) {0xc8, 0xd0, 0x58, 0xe0};
  v4i8_r = __builtin_mips_shll_qb (v4i8_a, 2);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0xf2, 0x34, 0x56, 0x78};
  i32_b = 1;
  v4i8_s = (v4i8) {0xe4, 0x68, 0xac, 0xf0};
  v4i8_r = __builtin_mips_shll_qb (v4i8_a, i32_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_s = (v2q15) {0x48d0, 0x59e0};
  v2q15_r = __builtin_mips_shll_ph (v2q15_a, 2);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  i32_b = 1;
  v2q15_s = (v2q15) {0x2468, 0xacf0};
  v2q15_r = __builtin_mips_shll_ph (v2q15_a, i32_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_s = (v2q15) {0x48d0, 0x7fff};
  v2q15_r = __builtin_mips_shll_s_ph (v2q15_a, 2);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  i32_b = 1;
  v2q15_s = (v2q15) {0x2468, 0x7fff};
  v2q15_r = __builtin_mips_shll_s_ph (v2q15_a, i32_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  q31_a = 0x70000000;
  q31_s = 0x7fffffff;
  q31_r = __builtin_mips_shll_s_w (q31_a, 2);
  if (q31_r != q31_s)
    abort ();

  q31_a = 0x70000000;
  i32_b = 1;
  q31_s = 0x7fffffff;
  q31_r = __builtin_mips_shll_s_w (q31_a, i32_b);
  if (q31_r != q31_s)
    abort ();

  v4i8_a = (v4i8) {0xf2, 0x34, 0x56, 0x78};
  v4i8_s = (v4i8) {0x3c, 0xd, 0x15, 0x1e};
  v4i8_r = __builtin_mips_shrl_qb (v4i8_a, 2);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0xf2, 0x34, 0x56, 0x78};
  i32_b = 1;
  v4i8_s = (v4i8) {0x79, 0x1a, 0x2b, 0x3c};
  v4i8_r = __builtin_mips_shrl_qb (v4i8_a, i32_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_s = (v2q15) {0x48d, 0x159e};
  v2q15_r = __builtin_mips_shra_ph (v2q15_a, 2);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  i32_b = 1;
  v2q15_s = (v2q15) {0x91a, 0x2b3c};
  v2q15_r = __builtin_mips_shra_ph (v2q15_a, i32_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_s = (v2q15) {0x48d, 0x159e};
  v2q15_r = __builtin_mips_shra_r_ph (v2q15_a, 2);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  i32_b = 3;
  v2q15_s = (v2q15) {0x247, 0xacf};
  v2q15_r = __builtin_mips_shra_r_ph (v2q15_a, i32_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  q31_a = 0x70000000;
  q31_s = 0x1c000000;
  q31_r = __builtin_mips_shra_r_w (q31_a, 2);
  if (q31_r != q31_s)
    abort ();

  q31_a = 0x70000004;
  i32_b = 3;
  q31_s = 0x0e000001;
  q31_r = __builtin_mips_shra_r_w (q31_a, i32_b);
  if (q31_r != q31_s)
    abort ();

  v4i8_a = (v4i8) {0x1, 0x2, 0x3, 0x4};
  v2q15_b = (v2q15) {0x6f89, 0x1111};
  if (little_endian)
    v2q15_s = (v2q15) {0xffff, 0x4444};
  else
    v2q15_s = (v2q15) {0x6f89, 0x2222};
  v2q15_r = __builtin_mips_muleu_s_ph_qbl (v4i8_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x1, 0x2, 0x3, 0x4};
  v2q15_b = (v2q15) {0x6f89, 0x1111};
  if (little_endian)
    v2q15_s = (v2q15) {0x6f89, 0x2222};
  else
    v2q15_s = (v2q15) {0xffff, 0x4444};
  v2q15_r = __builtin_mips_muleu_s_ph_qbr (v4i8_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_b = (v2q15) {0x6f89, 0x1111};
  v2q15_s = (v2q15) {0x0fdd, 0x0b87};
  v2q15_r = __builtin_mips_mulq_rs_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x8000, 0x8000};
  v2q15_b = (v2q15) {0x8000, 0x8000};
  q31_s = 0x7fffffff;
  q31_r = __builtin_mips_muleq_s_w_phl (v2q15_a, v2q15_b);
  if (q31_r != q31_s)
    abort ();

  v2q15_a = (v2q15) {0x8000, 0x8000};
  v2q15_b = (v2q15) {0x8000, 0x8000};
  q31_s = 0x7fffffff;
  q31_r = __builtin_mips_muleq_s_w_phr (v2q15_a, v2q15_b);
  if (q31_r != q31_s)
    abort ();

#ifndef __mips64
  a64_a = 0x22221111;
  v4i8_b = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_c = (v4i8) {0xaa, 0x89, 0x11, 0x34};
  if (little_endian)
    a64_s = 0x22222f27;
  else
    a64_s = 0x222238d9;
  a64_r = __builtin_mips_dpau_h_qbl (a64_a, v4i8_b, v4i8_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x22221111;
  v4i8_b = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_c = (v4i8) {0xaa, 0x89, 0x11, 0x34};
  if (little_endian)
    a64_s = 0x222238d9;
  else
    a64_s = 0x22222f27;
  a64_r = __builtin_mips_dpau_h_qbr (a64_a, v4i8_b, v4i8_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x22221111;
  v4i8_b = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_c = (v4i8) {0xaa, 0x89, 0x11, 0x34};
  if (little_endian)
    a64_s = 0x2221f2fb;
  else
    a64_s = 0x2221e949;
  a64_r = __builtin_mips_dpsu_h_qbl (a64_a, v4i8_b, v4i8_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x22221111;
  v4i8_b = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_c = (v4i8) {0xaa, 0x89, 0x11, 0x34};
  if (little_endian)
    a64_s = 0x2221e949;
  else
    a64_s = 0x2221f2fb;
  a64_r = __builtin_mips_dpsu_h_qbr (a64_a, v4i8_b, v4i8_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x00001111;
  v2q15_b = (v2q15) {0x8000, 0x5678};
  v2q15_c = (v2q15) {0x8000, 0x1111};
  a64_s = 0x8b877d00;
  a64_r = __builtin_mips_dpaq_s_w_ph (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x00001111;
  v2q15_b = (v2q15) {0x8000, 0x5678};
  v2q15_c = (v2q15) {0x8000, 0x1111};
  a64_s = 0xffffffff7478a522LL;
  a64_r = __builtin_mips_dpsq_s_w_ph (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x00001111;
  v2q15_b = (v2q15) {0x8000, 0x5678};
  v2q15_c = (v2q15) {0x8000, 0x1111};
  if (little_endian)
    a64_s = 0xffffffff8b877d02LL;
  else
    a64_s = 0x7478a520;
  a64_r = __builtin_mips_mulsaq_s_w_ph (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x00001111;
  q31_b = 0x80000000;
  q31_c = 0x80000000;
  a64_s = 0x7fffffffffffffffLL;
  a64_r = __builtin_mips_dpaq_sa_l_w (a64_a, q31_b, q31_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x00001111;
  q31_b = 0x80000000;
  q31_c = 0x80000000;
  a64_s = 0x8000000000001112LL;
  a64_r = __builtin_mips_dpsq_sa_l_w (a64_a, q31_b, q31_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x00001111;
  v2q15_b = (v2q15) {0x8000, 0x1};
  v2q15_c = (v2q15) {0x8000, 0x2};
  if (little_endian)
    a64_s = 0x1115;
  else
    a64_s = 0x80001110;
  a64_r = __builtin_mips_maq_s_w_phl (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x00001111;
  v2q15_b = (v2q15) {0x8000, 0x1};
  v2q15_c = (v2q15) {0x8000, 0x2};
  if (little_endian)
    a64_s = 0x80001110;
  else
    a64_s = 0x1115;
  a64_r = __builtin_mips_maq_s_w_phr (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x00001111;
  v2q15_b = (v2q15) {0x8000, 0x1};
  v2q15_c = (v2q15) {0x8000, 0x2};
  if (little_endian)
    a64_s = 0x1115;
  else
    a64_s = 0x7fffffff;
  a64_r = __builtin_mips_maq_sa_w_phl (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x00001111;
  v2q15_b = (v2q15) {0x8000, 0x1};
  v2q15_c = (v2q15) {0x8000, 0x2};
  if (little_endian)
    a64_s = 0x7fffffff;
  else
    a64_s = 0x1115;
  a64_r = __builtin_mips_maq_sa_w_phr (a64_a, v2q15_b, v2q15_c);
  if (a64_r != a64_s)
    abort ();
#endif

  i32_a = 0x12345678;
  i32_s = 0x00001e6a;
  i32_r = __builtin_mips_bitrev (i32_a);
  if (i32_r != i32_s)
    abort ();

  i32_a = 0x00000208; // pos is 8, size is 4
  __builtin_mips_wrdsp (i32_a, 31);
  i32_a = 0x12345678;
  i32_b = 0x87654321;
  i32_s = 0x12345178;
  i32_r = __builtin_mips_insv (i32_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  v4i8_s = (v4i8) {1, 1, 1, 1};
  v4i8_r = __builtin_mips_repl_qb (1);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  i32_a = 99;
  v4i8_s = (v4i8) {99, 99, 99, 99};
  v4i8_r = __builtin_mips_repl_qb (i32_a);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  v2q15_s = (v2q15) {30, 30};
  v2q15_r = __builtin_mips_repl_ph (30);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  i32_a = 0x5612;
  v2q15_s = (v2q15) {0x5612, 0x5612};
  v2q15_r = __builtin_mips_repl_ph (i32_a);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0x12, 0x34, 0x78, 0x56};
  if (little_endian)
    i32_s = 0x03000000;
  else
    i32_s = 0x0c000000;
  __builtin_mips_cmpu_eq_qb (v4i8_a, v4i8_b);
  i32_r = __builtin_mips_rddsp (16);
  if (i32_r != i32_s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0x12, 0x34, 0x78, 0x56};
  if (little_endian)
    i32_s = 0x04000000;
  else
    i32_s = 0x02000000;
  __builtin_mips_cmpu_lt_qb (v4i8_a, v4i8_b);
  i32_r = __builtin_mips_rddsp (16);
  if (i32_r != i32_s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0x12, 0x34, 0x78, 0x56};
  if (little_endian)
    i32_s = 0x07000000;
  else
    i32_s = 0x0e000000;
  __builtin_mips_cmpu_le_qb (v4i8_a, v4i8_b);
  i32_r = __builtin_mips_rddsp (16);
  if (i32_r != i32_s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0x12, 0x34, 0x78, 0x56};
  if (little_endian)
    i32_s = 0x3;
  else
    i32_s = 0xc;
  i32_r=__builtin_mips_cmpgu_eq_qb (v4i8_a, v4i8_b);
  if (i32_r != i32_s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0x12, 0x34, 0x78, 0x56};
  if (little_endian)
    i32_s = 0x4;
  else
    i32_s = 0x2;
  i32_r = __builtin_mips_cmpgu_lt_qb (v4i8_a, v4i8_b);
  if (i32_r != i32_s)
    abort ();

  v4i8_a = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0x12, 0x34, 0x78, 0x56};
  if (little_endian)
    i32_s = 0x7;
  else
    i32_s = 0xe;
  i32_r = __builtin_mips_cmpgu_le_qb (v4i8_a, v4i8_b);
  if (i32_r != i32_s)
    abort ();

  __builtin_mips_wrdsp (0,31); // Clear all condition code bits.
  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_b = (v2q15) {0x1234, 0x7856};
  if (little_endian)
    i32_s = 0x01000000;
  else
    i32_s = 0x02000000;
  __builtin_mips_cmp_eq_ph (v2q15_a, v2q15_b);
  i32_r = __builtin_mips_rddsp (16);
  if (i32_r != i32_s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_b = (v2q15) {0x1234, 0x7856};
  if (little_endian)
    i32_s = 0x02000000;
  else
    i32_s = 0x01000000;
  __builtin_mips_cmp_lt_ph (v2q15_a, v2q15_b);
  i32_r = __builtin_mips_rddsp (16);
  if (i32_r != i32_s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_b = (v2q15) {0x1234, 0x7856};
  i32_s = 0x03000000;
  __builtin_mips_cmp_le_ph (v2q15_a, v2q15_b);
  i32_r = __builtin_mips_rddsp (16);
  if (i32_r != i32_s)
    abort ();

  i32_a = 0x0a000000; // cc: 0000 1010
  __builtin_mips_wrdsp (i32_a, 31);
  v4i8_a = (v4i8) {0x12, 0x34, 0x56, 0x78};
  v4i8_b = (v4i8) {0x21, 0x43, 0x65, 0x87};
  if (little_endian)
    v4i8_s = (v4i8) {0x21, 0x34, 0x65, 0x78};
  else
    v4i8_s = (v4i8) {0x12, 0x43, 0x56, 0x87};
  v4i8_r = __builtin_mips_pick_qb (v4i8_a, v4i8_b);
  r = (int) v4i8_r;
  s = (int) v4i8_s;
  if (r != s)
    abort ();

  i32_a = 0x02000000; // cc: 0000 0010
  __builtin_mips_wrdsp (i32_a, 31);
  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_b = (v2q15) {0x2143, 0x6587};
  if (little_endian)
    v2q15_s = (v2q15) {0x2143, 0x5678};
  else
    v2q15_s = (v2q15) {0x1234, 0x6587};
  v2q15_r = __builtin_mips_pick_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

  v2q15_a = (v2q15) {0x1234, 0x5678};
  v2q15_b = (v2q15) {0x1234, 0x7856};
  if (little_endian)
    v2q15_s = (v2q15) {0x7856, 0x1234};
  else
    v2q15_s = (v2q15) {0x5678, 0x1234};
  v2q15_r = __builtin_mips_packrl_ph (v2q15_a, v2q15_b);
  r = (int) v2q15_r;
  s = (int) v2q15_s;
  if (r != s)
    abort ();

#ifndef __mips64
  a64_a = 0x1234567887654321LL;
  i32_s = 0x88765432;
  i32_r = __builtin_mips_extr_w (a64_a, 4);
  if (i32_r != i32_s)
    abort ();

  a64_a = 0x1234567887658321LL;
  i32_s = 0x56788766;
  i32_r = __builtin_mips_extr_r_w (a64_a, 16);
  if (i32_r != i32_s)
    abort ();

  a64_a = 0x12345677fffffff8LL;
  i32_s = 0x7fffffff;
  i32_r = __builtin_mips_extr_rs_w (a64_a, 4);
  if (i32_r != i32_s)
    abort ();

  a64_a = 0x1234567887658321LL;
  i32_s = 0x7fff;
  i32_r = __builtin_mips_extr_s_h (a64_a, 16);
  if (i32_r != i32_s)
    abort ();

  a64_a = 0x0000007887658321LL;
  i32_b = 24;
  i32_s = 0x7887;
  i32_r = __builtin_mips_extr_s_h (a64_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  a64_a = 0x1234567887654321LL;
  i32_b = 4;
  i32_s = 0x88765432;
  i32_r = __builtin_mips_extr_w (a64_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  a64_a = 0x1234567887658321LL;
  i32_b = 16;
  i32_s = 0x56788766;
  i32_r = __builtin_mips_extr_r_w (a64_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  a64_a = 0x12345677fffffff8LL;
  i32_b = 4;
  i32_s = 0x7fffffff;
  i32_r = __builtin_mips_extr_rs_w (a64_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  i32_a = 0x0000021f; // pos is 31
  __builtin_mips_wrdsp (i32_a, 31);
  a64_a = 0x1234567887654321LL;
  i32_s = 8;
  i32_r = __builtin_mips_extp (a64_a, 3); // extract 4 bits
  if (i32_r != i32_s)
    abort ();

  i32_a = 0x0000021f; // pos is 31
  __builtin_mips_wrdsp (i32_a, 31);
  a64_a = 0x1234567887654321LL;
  i32_b = 7; // size is 8. NOTE!! we should use 7
  i32_s = 0x87;
  i32_r = __builtin_mips_extp (a64_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  i32_a = 0x0000021f; // pos is 31
  __builtin_mips_wrdsp (i32_a, 31);
  a64_a = 0x1234567887654321LL;
  i32_s = 8;
  i32_r = __builtin_mips_extpdp (a64_a, 3); // extract 4 bits
  if (i32_r != i32_s)
    abort ();

  i32_s = 0x0000021b; // pos is 27
  i32_r = __builtin_mips_rddsp (31);
  if (i32_r != i32_s)
    abort ();

  i32_a = 0x0000021f; // pos is 31
  __builtin_mips_wrdsp (i32_a, 31);
  a64_a = 0x1234567887654321LL;
  i32_b = 11; // size is 12. NOTE!!! We should use 11
  i32_s = 0x876;
  i32_r = __builtin_mips_extpdp (a64_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  i32_s = 0x00000213; // pos is 19
  i32_r = __builtin_mips_rddsp (31);
  if (i32_r != i32_s)
    abort ();

  a64_a = 0x1234567887654321LL;
  a64_s = 0x0012345678876543LL;
  a64_r = __builtin_mips_shilo (a64_a, 8);
  if (a64_r != a64_s)
    abort ();

  a64_a = 0x1234567887654321LL;
  i32_b = -16;
  a64_s = 0x5678876543210000LL;
  a64_r = __builtin_mips_shilo (a64_a, i32_b);
  if (a64_r != a64_s)
    abort ();

  i32_a = 0x0;
  __builtin_mips_wrdsp (i32_a, 31);
  a64_a = 0x1234567887654321LL;
  i32_b = 0x11112222;
  a64_s = 0x8765432111112222LL;
  a64_r = __builtin_mips_mthlip (a64_a, i32_b);
  if (a64_r != a64_s)
    abort ();
  i32_s = 32;
  i32_r = __builtin_mips_rddsp (31);
  if (i32_r != i32_s)
    abort ();
#endif

  i32_a = 0x1357a468;
  __builtin_mips_wrdsp (i32_a, 63);
  i32_s = 0x03572428;
  i32_r = __builtin_mips_rddsp (63);
  if (i32_r != i32_s)
    abort ();

  ptr_a = &array;
  i32_b = 37;
  i32_s = 37;
  i32_r = __builtin_mips_lbux (ptr_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  ptr_a = &array;
  i32_b = 38;
  if (little_endian)
    i32_s = 0x2726;
  else
    i32_s = 0x2627;
  i32_r = __builtin_mips_lhx (ptr_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  ptr_a = &array;
  i32_b = 40;
  if (little_endian)
    i32_s = 0x2b2a2928;
  else
    i32_s = 0x28292a2b;
  i32_r = __builtin_mips_lwx (ptr_a, i32_b);
  if (i32_r != i32_s)
    abort ();

  i32_a = 0x00000220; // pos is 32, size is 4
  __builtin_mips_wrdsp (i32_a, 63);
  i32_s = 1;
  i32_r = __builtin_mips_bposge32 ();
  if (i32_r != i32_s)
    abort ();

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
}

