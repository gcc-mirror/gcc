/* PR middle-end/85956 */
/* { dg-do compile } */
/* { dg-additional-options "-O2 -Wall" } */

void
foo (int n, void *p)
{
  int (*a)[n] = (int (*)[n]) p;
  #pragma omp parallel shared(a) default(none)
  #pragma omp master
    a[-1][-1] = 42;	/* { dg-warning "array subscript -1 is below array bounds" } */
}
