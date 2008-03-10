// PR middle-end/35099
// { dg-do compile }
// { dg-options "-O2 -fopenmp" }

struct A
{
  ~A () throw ();
  void foo ();
};

struct B
{
  B () { A ().foo (); }
};

void
bar ()
{
#pragma omp parallel
  {
  #pragma omp single
    B ();
  #pragma omp for
    for (int i = 0; i < 2; ++i)
      B ();
  }
}

void
baz ()
{
#pragma omp parallel
  {
  #pragma omp single
    B ();
  #pragma omp single
    B ();
  }
}
