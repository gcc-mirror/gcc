/* { dg-require-effective-target vect_complex_add_double } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-add-options arm_v8_3a_complex_neon } */

#include <stdio.h>
#include <complex.h>
#include <string.h>
#include <float.h>
#include <math.h>

#define PREF old
#pragma GCC push_options
#pragma GCC optimize ("no-tree-vectorize")
# include "complex-operations.c"
#pragma GCC pop_options
#undef PREF

#define PREF new
# include "complex-operations.c"
#undef PREF

#define TYPE double
#define TYPE2 double
#define EP pow(2, -45)

#define xstr(s) str(s)
#define str(s) #s

#define FCMP(A, B) \
  ((fabs (creal (A) - creal (B)) <= EP) && (fabs (cimag (A) - cimag (B)) <= EP))

#define CMP(A, B) \
  (FCMP(A,B) ? "PASS" : "FAIL")

#define COMPARE(A,B) \
  memset (&c1, 0, sizeof (c1)); \
  memset (&c2, 0, sizeof (c2)); \
  A; B; \
  if (!FCMP(c1[0],c2[0]) || !FCMP(c1[1], c2[1])) \
  { \
    printf ("=> %s vs %s\n", xstr (A), xstr (B)); \
    printf ("%a\n", creal (c1[0]) - creal (c2[0])); \
    printf ("%a\n", cimag (c1[1]) - cimag (c2[1])); \
    printf ("%.2f+%.2fI == %.2f+%.2fI (%s)\n", creal (c1[0]), cimag (c1[0]), creal (c2[0]), cimag (c2[0]), CMP (c1[0], c2[0])); \
    printf ("%.2f+%.2fI == %.2f+%.2fI (%s)\n", creal (c1[1]), cimag (c1[1]), creal (c2[1]), cimag (c2[1]), CMP (c1[1], c2[1])); \
    printf ("\n"); \
    __builtin_abort (); \
  }

int main ()
{
  TYPE2 complex a[] = { 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I, 1.0 + 3.0 * I, 2.0 + 3.5 * I };
  TYPE  complex b[] = { 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I, 1.1 + 3.1 * I, 2.1 + 3.6 * I };
  TYPE  complex c2[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  TYPE  complex c1[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  TYPE  diff1, diff2;

  COMPARE(fma0_old(a, b, c1), fma0_new(a, b, c2));
  COMPARE(fma90_old(a, b, c1), fma90_new(a, b, c2));
  COMPARE(fma180_old(a, b, c1), fma180_new(a, b, c2));
  COMPARE(fma270_old(a, b, c1), fma270_new(a, b, c2));
  COMPARE(fma0_snd_old(a, b, c1), fma0_snd_new(a, b, c2));
  COMPARE(fma90_snd_old(a, b, c1), fma90_snd_new(a, b, c2));
  COMPARE(fma180_snd_old(a, b, c1), fma180_snd_new(a, b, c2));
  COMPARE(fma270_snd_old(a, b, c1), fma270_snd_new(a, b, c2));
  COMPARE(fma_conj_first_old(a, b, c1), fma_conj_first_new(a, b, c2));
  COMPARE(fma_conj_second_old(a, b, c1), fma_conj_second_new(a, b, c2));
  COMPARE(fma_conj_both_old(a, b, c1), fma_conj_both_new(a, b, c2));
  COMPARE(fms0_old(a, b, c1), fms0_new(a, b, c2));
  COMPARE(fms90_old(a, b, c1), fms90_new(a, b, c2));
  COMPARE(fms180_old(a, b, c1), fms180_new(a, b, c2));
  COMPARE(fms270_old(a, b, c1), fms270_new(a, b, c2));
  COMPARE(fms0_snd_old(a, b, c1), fms0_snd_new(a, b, c2));
  COMPARE(fms90_snd_old(a, b, c1), fms90_snd_new(a, b, c2));
  COMPARE(fms180_snd_old(a, b, c1), fms180_snd_new(a, b, c2));
  COMPARE(fms270_snd_old(a, b, c1), fms270_snd_new(a, b, c2));
  COMPARE(fms_conj_first_old(a, b, c1), fms_conj_first_new(a, b, c2));
  COMPARE(fms_conj_second_old(a, b, c1), fms_conj_second_new(a, b, c2));
  COMPARE(fms_conj_both_old(a, b, c1), fms_conj_both_new(a, b, c2));
  COMPARE(mul0_old(a, b, c1), mul0_new(a, b, c2));
  COMPARE(mul90_old(a, b, c1), mul90_new(a, b, c2));
  COMPARE(mul180_old(a, b, c1), mul180_new(a, b, c2));
  COMPARE(mul270_old(a, b, c1), mul270_new(a, b, c2));
  COMPARE(mul0_snd_old(a, b, c1), mul0_snd_new(a, b, c2));
  COMPARE(mul90_snd_old(a, b, c1), mul90_snd_new(a, b, c2));
  COMPARE(mul180_snd_old(a, b, c1), mul180_snd_new(a, b, c2));
  COMPARE(mul270_snd_old(a, b, c1), mul270_snd_new(a, b, c2));
  COMPARE(mul_conj_first_old(a, b, c1), mul_conj_first_new(a, b, c2));
  COMPARE(mul_conj_second_old(a, b, c1), mul_conj_second_new(a, b, c2));
  COMPARE(mul_conj_both_old(a, b, c1), mul_conj_both_new(a, b, c2));
  COMPARE(add0_old(a, b, c1), add0_new(a, b, c2));
  COMPARE(add90_old(a, b, c1), add90_new(a, b, c2));
  COMPARE(add180_old(a, b, c1), add180_new(a, b, c2));
  COMPARE(add270_old(a, b, c1), add270_new(a, b, c2));
  COMPARE(add0_snd_old(a, b, c1), add0_snd_new(a, b, c2));
  COMPARE(add90_snd_old(a, b, c1), add90_snd_new(a, b, c2));
  COMPARE(add180_snd_old(a, b, c1), add180_snd_new(a, b, c2));
  COMPARE(add270_snd_old(a, b, c1), add270_snd_new(a, b, c2));
  COMPARE(add_conj_first_old(a, b, c1), add_conj_first_new(a, b, c2));
  COMPARE(add_conj_second_old(a, b, c1), add_conj_second_new(a, b, c2));
  COMPARE(add_conj_both_old(a, b, c1), add_conj_both_new(a, b, c2));
}
