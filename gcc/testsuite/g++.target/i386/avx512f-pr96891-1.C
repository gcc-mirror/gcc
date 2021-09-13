/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-not "%k\[0-7\]" } } */

typedef float v4sf __attribute__ ((vector_size (16)));
typedef float v8sf __attribute__ ((vector_size (32)));
typedef double v2df __attribute__ ((vector_size (16)));
typedef double v4df __attribute__ ((vector_size (32)));


v4sf
foo_v4sf (v4sf x)
{
  const union U { unsigned u; float f; } u = { -1U };
  return x > 0.0f ? u.f : 0.0f;
}

v8sf
foo_v8sf (v8sf x)
{
  const union U { unsigned u; float f; } u = { -1U };
  return x > 0.0f ? u.f : 0.0f;
}

v2df
foo_v2df (v2df x)
{
  const union U { unsigned long long u; double df; } u = { -1ULL };
  return x > 0.0 ? u.df : 0.0;
}

v4df
foo_v4df (v4df x)
{
  const union U { unsigned long long u; double df; } u = { -1ULL };
  return x > 0.0 ? u.df : 0.0;
}
