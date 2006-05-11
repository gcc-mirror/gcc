/* { dg-do run { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include "altivec_check.h"

typedef int int4 __attribute__ ((vector_size (16)));
typedef float float4 __attribute__ ((vector_size (16)));

int4 a1 = (int4) { 100, 200, 300, 400 };
int4 a2 = (int4) { 500, 600, 700, 800 };

float4 f1 = (float4) { 1.0, 2.0, 3.0, 4.0 };  
float4 f2 = (float4) { 5.0, 6.0, 7.0, 8.0 };

int i3[4] __attribute__((aligned(16)));
int j3[4] __attribute__((aligned(16)));
float h3[4] __attribute__((aligned(16)));
float g3[4] __attribute__((aligned(16)));

#define vec_store(dst, src) \
  __builtin_vec_st (src, 0, (__typeof__ (src) *) dst)

#define vec_add_int4(x, y) \
  __builtin_altivec_vaddsws (x, y)

#define vec_add_float4(x, y) \
  __builtin_altivec_vaddfp (x, y)

#define my_abs(x) (x > 0.0F ? x : -x)

void
compare_int4 (int *a, int *b)
{
  int i;

  for (i = 0; i < 4; ++i)
    if (a[i] != b[i])
      abort ();
}

void
compare_float4 (float *a, float *b)
{
  int i;

  for (i = 0; i < 4; ++i)
    if (my_abs(a[i] - b[i]) >= 1.0e-6)
      abort ();
}

void
main1 ()
{
  int loc1 = 600, loc2 = 800;
  int4 a3 = (int4) { loc1, loc2, 1000, 1200 };
  int4 itmp;
  double locf = 12.0;
  float4 f3 = (float4) { 6.0, 8.0, 10.0, 12.0 };
  float4 ftmp;

  vec_store (i3, a3);
  itmp = vec_add_int4 (a1, a2);
  vec_store (j3, itmp);
  compare_int4 (i3, j3);

  vec_store (g3, f3);
  ftmp = vec_add_float4 (f1, f2);
  vec_store (h3, ftmp);
  compare_float4 (g3, h3);
}

int
main ()
{
  altivec_check ();
  main1 ();
  exit (0);
}
