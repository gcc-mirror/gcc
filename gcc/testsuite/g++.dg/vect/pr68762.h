// PR middle-end/68762

#pragma omp declare simd
double baz (double x);

#pragma omp declare simd
inline double
foo (double d)
{
  return baz (d);
}
