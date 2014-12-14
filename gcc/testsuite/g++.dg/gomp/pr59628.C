// PR c++/59628
// { dg-do compile }
// { dg-options "-fopenmp" }

struct A { int i; };

void foo()
{
  A a;
  #pragma omp declare reduction (+: A: omp_out.i +: omp_in.i)  // { dg-error "expected" }
  #pragma omp parallel reduction (+: a)
  ;
}
