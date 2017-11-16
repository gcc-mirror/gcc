// PR c++/37189
// { dg-do compile }
// { dg-options "-fopenmp" }

struct S
{
  S () {}
  S (S const &) {}
};

struct T
{
  S s;
};

void
bar (T &)
{
}

void
foo ()
{
  T t;
  #pragma omp task
    bar (t);
}
