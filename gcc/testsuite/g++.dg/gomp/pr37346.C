// PR c++/37346
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
