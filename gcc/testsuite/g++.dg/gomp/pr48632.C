// PR c++/48632
// { dg-do compile }
// { dg-options "-fopenmp" }

template<typename T>
void
foo (T *x, T *y, unsigned z)
{
#pragma omp parallel for
  for (T *p = x; p < y; p += z)
    ;
#pragma omp parallel for
  for (T *p = y; p > x; p -= z)
    ;
}

int
main ()
{
  char buf[10];
  foo (&buf[0], &buf[9], 1);
}
