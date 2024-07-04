/* PR tree-optimization/114075 */
/* { dg-do run } */
/* { dg-options "-O2 -fopenmp-simd -Wno-psabi" } */

typedef float V __attribute__((__vector_size__ (16)));

__attribute__((__always_inline__)) inline static float
foo (V a)
{
  float r = 0;
#pragma omp simd reduction(+:r)
  for (unsigned long i = 0; i < (sizeof (a) / sizeof (float)); i++)
    r += a[i];
  return r;
}

int
main ()
{
  static const struct { V a; float r; } t[] = {
    { (V) { -17.0f, -18.0f, -19.0f, 20.0f }, -34.0f },
    { (V) { 18.5f, 19.125f, 20.5f, -38.25f }, 19.875f },
    { (V) { -8.25f, 16.75f, -42.5f, -18.75f }, -52.75f }
  };
  for (unsigned long i = 0; i < (sizeof (t) / sizeof (t[0])); i++)
    {
      float r = foo (t[i].a);
      if (r != t[i].r)
        __builtin_abort ();
    }
}
