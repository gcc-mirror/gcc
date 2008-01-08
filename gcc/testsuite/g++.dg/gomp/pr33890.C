// PR c++/33890
// { dg-do compile }
// { dg-options "-fopenmp" }

struct A
{
  int x;
  A () : x (0) {}
  int & getX ();
};

template <int> void
foo ()
{
  A a;

#pragma omp for
  for (int i = a.getX (); i < 10; ++i)
    ;
#pragma omp for
  for (int i = 0; i < a.getX (); ++i)
    ;
  a.x = 1;
#pragma omp for
  for (int i = 0; i < 10; i += a.getX ())
    ;
}

void
bar ()
{
  foo <0> ();
  foo <1> ();
}
