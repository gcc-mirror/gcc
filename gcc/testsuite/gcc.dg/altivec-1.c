/* { dg-do run { target powerpc-*-*altivec powerpc-*-*-*altivec } } */
/* { dg-options "-maltivec" } */

/* Program to test PowerPC AltiVec instructions.  */

/* These macros are not analogous to the overloaded functions
   described in Motorola's AltiVec Programming Interface Manual.
   These are just here for readability.  Eventually we'll get the
   overloaded functions implemented in an <altivec.h>.  */

#define vec_load(src) \
  __builtin_altivec_ld_internal_4si ((int *) src)

#define vec_store(dst, src) \
  __builtin_altivec_st_internal_4si ((int *) dst, (int4) src)

#define vec_add_int4(x, y) \
  __builtin_altivec_vaddsws (x, y)

#define vec_add_float4(x, y) \
  __builtin_altivec_vaddfp (x, y)

#define vec_average_int4(x, y) \
  __builtin_altivec_vavgsw (x, y)

typedef int int4 __attribute__ ((mode(V4SI)));
typedef float float4 __attribute__ ((mode(V4SF)));

int a1[4] __attribute__((aligned(16))) = { 100, 200, 300, 400 };
int a2[4] __attribute__((aligned(16))) = { 500, 600, 700, 800 };
int a3[4] __attribute__((aligned(16)));
int addi[4] = { 600, 800, 1000, 1200 };
int avgi[4] = { 300, 400, 500, 600 };

float f1[4] __attribute__((aligned(16))) = { 1.0, 2.0, 3.0, 4.0 };  
float f2[4] __attribute__((aligned(16))) = { 5.0, 6.0, 7.0, 8.0 };
float f3[4] __attribute__((aligned(16)));
float addf[4] = { 6.0, 8.0, 10.0, 12.0 };

int4 i, j, k;
float4 f, g, h;

void
compare_int4 (int *a, int *b)
{
  int i;

  for (i = 0; i < 4; ++i)
    if (a[i] != b[i])
      exit (1);
}

void
compare_float4 (float *a, float *b)
{
  int i;

  for (i = 0; i < 4; ++i)
    if (a[i] != b[i])
      exit (1);
}

main ()
{
  i = vec_load (a1);
  j = vec_load (a2);
  k = vec_add_int4 (i, j);
  vec_store (a3, k);
  compare_int4 (a3, addi);

  k = vec_average_int4 (i, j);
  vec_store (a3, k);
  compare_int4 (a3, avgi);

  f = (float4) vec_load (f1);
  g = (float4) vec_load (f2);
  h = vec_add_float4 (f, g);
  vec_store (f3, h);
  compare_float4 (f3, addf);

  exit (0);
}
