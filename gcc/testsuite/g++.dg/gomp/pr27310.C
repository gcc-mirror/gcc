// PR middle-end/27310
// { dg-do compile }
// { dg-options "-O2 -fopenmp" }

struct A
{
  ~A ();
};

struct B
{
  A a, b;
};

void
foo ()
{
  A c, d;

#pragma omp parallel
  B e;
}
