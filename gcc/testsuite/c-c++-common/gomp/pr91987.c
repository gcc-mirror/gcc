/* PR c++/91987 */

int bar (void);
void baz (int *);
#pragma omp declare target to (baz)

void
foo (int *a, int (*b)[10][10])
{
  #pragma omp target map(a[bar ()])
  baz (a);
  #pragma omp target map(a[bar ():1])
  baz (a);
  #pragma omp target map(a[10:bar ()])
  baz (a);
  #pragma omp task depend(inout:a[10:bar ()])
  baz (a);
  #pragma omp task depend(inout:a[10:bar ()])
  baz (a);
  #pragma omp parallel reduction(+:a[bar ():2])
  baz (a);
  #pragma omp parallel reduction(+:a[2:bar ()])
  baz (a);
  #pragma omp parallel reduction(+:b[bar ():2][bar ():10][bar ():10])
  baz (a);
}
