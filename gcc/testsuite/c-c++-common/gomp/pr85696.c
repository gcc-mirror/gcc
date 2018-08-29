/* PR c/85696 */

#ifndef __cplusplus
void
foo (int n, int a[][n])
{
  #pragma omp parallel shared(a) default(none)
  #pragma omp master
    a[23][0] = 42;
}
#endif

void
bar (int n, void *p)
{
  int (*a)[n] = (int (*)[n]) p;
  #pragma omp parallel shared(a) default(none)
  #pragma omp master
    a[23][0] = 42;
}
