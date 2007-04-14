/* { dg-options "-O -fopenmp" } */

void foo();

inline void bar()
{
  int i;
  for ( i=0; i<1; ++i )
#pragma omp parallel
    foo();
}

void baz()
{
#pragma omp parallel
  bar();
}
