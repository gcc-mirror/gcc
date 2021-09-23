/* PR c/100902 */

void
foo (int *ptr)
{
  #pragma omp target map (ptr, ptr[:4])
  #pragma omp parallel master
  ptr[0] = 1;
}

void
bar (int *ptr)
{
  #pragma omp target parallel map (ptr[:4], ptr)
  #pragma omp master
  ptr[0] = 1;
}
