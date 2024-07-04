/* { dg-do compile } */
/* { dg-options "-O3 -fno-vect-cost-model -mlasx" } */
/* { dg-final { scan-assembler-times "vld" 12 } } */


typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v32qi __attribute__ ((vector_size (32)));

typedef short v8hi __attribute__ ((vector_size (16)));
typedef short v16hi __attribute__ ((vector_size (32)));

typedef int v4si __attribute__ ((vector_size (16)));
typedef int v8si __attribute__ ((vector_size (32)));

typedef long v2di __attribute__ ((vector_size (16)));
typedef long v4di __attribute__ ((vector_size (32)));

typedef float v4sf __attribute__ ((vector_size (16)));
typedef float v8sf __attribute__ ((vector_size (32)));

typedef double v2df __attribute__ ((vector_size (16)));
typedef double v4df __attribute__ ((vector_size (32)));

v16qi a_qi, b_qi;
v8hi  a_hi, b_hi;
v4si  a_si, b_si;
v2di  a_di, b_di;
v4sf  a_sf, b_sf;
v2df  a_df, b_df;

v32qi
foo_v32qi ()
{
  return __builtin_shufflevector (a_qi, b_qi, 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23, 8, 24, 9, 25, 10, 26, 11, 27, 12, 28, 13, 29, 14, 30, 15, 31);
}

v16hi
foo_v16qi ()
{
  return __builtin_shufflevector (a_hi, b_hi, 0, 8, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15);
}

v8si
foo_v8si ()
{
  return __builtin_shufflevector (a_si, b_si, 0, 4, 1, 5, 2, 6, 3, 7);
}

v4di
foo_v4di ()
{
  return __builtin_shufflevector (a_di, b_di, 0, 2, 1, 3);
}

v8sf
foo_v8sf ()
{
  return __builtin_shufflevector (a_sf, b_sf, 0, 4, 1, 5, 2, 6, 3, 7);
}

v4df
foo_v4df ()
{
  return __builtin_shufflevector (a_df, b_df, 0, 2, 1, 3);
}
