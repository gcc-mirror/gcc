// PR middle-end/26913

struct A
{
  ~A () throw ();
};

void foo (A);

A bar () throw ();

void baz ()
{
#pragma omp parallel
  {
    A a;
    foo (bar ());
  }
}
