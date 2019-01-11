// PR lto/88733
// { dg-do compile }
// { dg-additional-options "-flto -ffat-lto-objects" { target lto } }

struct A { int f; } a;

__attribute__((noipa)) void
bar (A **x, int)
{
  x[0] = &a;
}

int
foo (int n)
{
  int g;
  A *j[n];
  bar (j, n);
#pragma omp parallel
#pragma omp single
  g = j[0]->f;
  return g;
}

int
main ()
{
  foo (0);
}
