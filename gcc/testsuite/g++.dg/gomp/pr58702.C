// PR c++/58702
// { dg-do compile }
// { dg-options "-fopenmp" }

void foo()
{
  x;   // { dg-error "was not declared" }
#pragma omp parallel for reduction(+:x)
  for (int i = 0; i < 10; ++i) ;
}
