/* { dg-do compile } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -ffast-math -fdump-tree-gimple" } */

float f(float x)
{
  return x * (x > 0.f ? -1.f : 1.f);
}
float f1(float x)
{
  return x * (x > 0.f ? 1.f : -1.f);
}
float g(float x)
{
  return x * (x >= 0.f ? -1.f : 1.f);
}
float g1(float x)
{
  return x * (x >= 0.f ? 1.f : -1.f);
}
float h(float x)
{
  return x * (x < 0.f ? -1.f : 1.f);
}
float h1(float x)
{
  return x * (x < 0.f ? 1.f : -1.f);
}
float i(float x)
{
  return x * (x <= 0.f ? -1.f : 1.f);
}
float i1(float x)
{
  return x * (x <= 0.f ? 1.f : -1.f);
}

/* { dg-final { scan-tree-dump-times "ABS" 8 "gimple" } } */
