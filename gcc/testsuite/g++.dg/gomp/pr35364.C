// PR target/35364
// { dg-do compile }
// { dg-options "-O2 -fopenmp" }

template <typename T>
struct E
{
  E ();
  ~E ();
};

template <typename T, typename U>
struct C
{
  C (const U &y) : u (y) {}
  ~C () {}
  const U &u;
};

template <typename T, typename U = E<T> >
struct B : public C<T, U>
{
  B (int x, const T &z = T (), const U &y = U ()) : C<T, U> (y) {}
  ~B () {}
};

void
foo ()
{
#pragma omp parallel
  {
    B<double> x (1);
  }
#pragma omp for
  for (int i = 0; i < 10; i++)
    {
      B<int> x (i);
    }
#pragma omp sections
  {
#pragma omp section
    {
      B<int> x (6);
    }
  }
#pragma omp single
  {
    B<int> x (16);
  }
}
