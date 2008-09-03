// PR c++/37436
// { dg-do compile }
// { dg-options "-fopenmp" }

struct A
{
  A ();
  int i;
};

A::A ()
{
#pragma omp critical
  i++;
}
