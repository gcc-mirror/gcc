/* { dg-do run } */
/* { dg-options "-mpaired-single" } */

/* Test MIPS paired-single builtin functions */
#include <stdlib.h>
#include <stdio.h>

typedef float v2sf __attribute__ ((vector_size(8)));

NOMIPS16 int main ()
{
  int little_endian;
  v2sf a, b, c, d;
  float e,f;
  int i;

  union { long long ll; int i[2]; } endianness_test;
  endianness_test.ll = 1;
  little_endian = endianness_test.i[0];

  /* pll.ps */
  a = (v2sf) {1, 2};
  b = (v2sf) {3, 4};
  c = __builtin_mips_pll_ps (a, b);
  if (little_endian) // little endian
    d = (v2sf) {3, 1};
  else // big endian
    d = (v2sf) {2, 4};

  if (!__builtin_mips_upper_c_eq_ps (c, d) ||
      !__builtin_mips_lower_c_eq_ps (c, d))
    abort ();

  /* pul.ps */
  a = (v2sf) {1, 2};
  b = (v2sf) {3, 4};
  c = __builtin_mips_pul_ps (a, b);
  if (little_endian) // little endian
    d = (v2sf) {3, 2};
  else // big endian
    d = (v2sf) {1, 4};
  if (!__builtin_mips_upper_c_eq_ps (c, d) ||
      !__builtin_mips_lower_c_eq_ps (c, d))
    abort ();

  /* plu.ps */
  a = (v2sf) {1, 2};
  b = (v2sf) {3, 4};
  c = __builtin_mips_plu_ps (a, b);
  if (little_endian) // little endian
    d = (v2sf) {4, 1};
  else // big endian
    d = (v2sf) {2, 3};
  if (!__builtin_mips_upper_c_eq_ps (c, d) ||
      !__builtin_mips_lower_c_eq_ps (c, d))
    abort ();

  /* puu.ps */
  a = (v2sf) {1, 2};
  b = (v2sf) {3, 4};
  c = __builtin_mips_puu_ps (a, b);
  if (little_endian) // little endian
    d = (v2sf) {4, 2};
  else // big endian
    d = (v2sf) {1, 3};
  if (!__builtin_mips_upper_c_eq_ps (c, d) ||
      !__builtin_mips_lower_c_eq_ps (c, d))
    abort ();

  /* cvt.ps.s */
  e = 3.4;
  f = 4.5; 
  a = __builtin_mips_cvt_ps_s (e, f);
  if (little_endian) // little endian
    b = (v2sf) {4.5, 3.4};
  else // big endian
    b = (v2sf) {3.4, 4.5};
  if (!__builtin_mips_upper_c_eq_ps (a, b) ||
      !__builtin_mips_lower_c_eq_ps (a, b))
    abort ();

  /* cvt.s.pl */
  a = (v2sf) {35.1, 120.2};
  e = __builtin_mips_cvt_s_pl (a);
  if (little_endian) // little endian
    f = 35.1; 
  else // big endian
    f = 120.2;
  if (e != f)
    abort ();

  /* cvt.s.pu */
  a = (v2sf) {30.0, 100.0};
  e = __builtin_mips_cvt_s_pu (a);
  if (little_endian) // little endian
    f = 100.0;
  else // big endian
    f = 30.0; 
  if (e != f)
    abort ();

  /* abs.ps */
  a = (v2sf) {-3.4, -5.8};
  b = __builtin_mips_abs_ps (a);
  c = (v2sf) {3.4, 5.8};
  if (!__builtin_mips_upper_c_eq_ps (b, c) ||
      !__builtin_mips_lower_c_eq_ps (b, c))
    abort ();

  /* alnv.ps with rs = 4*/
  a = (v2sf) {1, 2};
  b = (v2sf) {3, 4};
  i = 4;
  c = __builtin_mips_alnv_ps (a, b, i);
  d = (v2sf) {2, 3};

  if (!__builtin_mips_upper_c_eq_ps (c, d) ||
      !__builtin_mips_lower_c_eq_ps (c, d))
    abort ();

  /* alnv.ps with rs = 0 */
  a = (v2sf) {5, 6};
  b = (v2sf) {7, 8};
  i = 0;
  c = __builtin_mips_alnv_ps (a, b, i);
  d = (v2sf) {5, 6};

  if (!__builtin_mips_upper_c_eq_ps (c, d) ||
      !__builtin_mips_lower_c_eq_ps (c, d))
    abort ();

  printf ("Test Passes\n");
  exit (0);
}
