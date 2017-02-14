/* PR middle-end/71758 */

void
foo (int *p)
{
  long long i = 0;
  #pragma omp target device (i)
  ;
  #pragma omp target update device (i) to (p[0])
}
