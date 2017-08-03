/* PR middle-end/81052 */
/* { dg-do compile } */
/* { dg-options "-fopenmp-simd -O2" } */

int
foo (int x, int y)
{
  int i;
#pragma omp simd
  for (i = x; i < y; ++i)
    return 0;			/* { dg-error "invalid branch to/from OpenMP structured block" } */
  return 1;
}

#ifdef __cplusplus
template <typename T>
T
bar (T x, T y)
{
  T i;
#pragma omp simd
  for (i = x; i < y; ++i)
    return 0;			/* { dg-error "invalid branch to/from OpenMP structured block" "" { target c++ } } */
  return 1;
}

int x = bar (1, 7);
#endif
