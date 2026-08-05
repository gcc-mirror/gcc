/* PR middle-end/99928 */

void
foo (void)
{
  int a[6] = {};
  #pragma omp target simd reduction(+:a[:3])
  for (int i = 0; i < 6; i++)
    a[0]++;
  #pragma omp target simd reduction(+:a[:3]) map(always, tofrom: a)
  for (int i = 0; i < 6; i++)
    a[0]++;
  #pragma omp target simd reduction(+:a[:3]) map(always, tofrom: a[:6])
  for (int i = 0; i < 6; i++)
    a[0]++;
}
