/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -mno-avx512vl -mprefer-vector-width=512 -O2" } */
/* { dg-final { scan-assembler-times "vpternlog\[dq\]\[ \\t\]+\\\$(?:216|228|0xd8|0xe4)," 5 } } */

double cs_df (double x, double y)
{
  return __builtin_copysign (x, y);
}

float cs_sf (float x, float y)
{
  return __builtin_copysignf (x, y);
}

typedef double __attribute__ ((vector_size (16))) v2df;
typedef double __attribute__ ((vector_size (32))) v4df;
typedef double __attribute__ ((vector_size (64))) v8df;

v2df cs_v2df (v2df x, v2df y)
{
  return __builtin_ia32_copysignpd (x, y);
}

v4df cs_v4df (v4df x, v4df y)
{
  return __builtin_ia32_copysignpd256 (x, y);
}

v8df cs_v8df (v8df x, v8df y)
{
  return __builtin_ia32_copysignpd512 (x, y);
}
