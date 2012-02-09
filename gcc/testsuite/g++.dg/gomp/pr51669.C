// PR c++/51669
// { dg-do compile }
// { dg-options "-fopenmp" }

template <typename T> const T & min (const T &, const T &);

void
f1 ()
{
#pragma omp parallel num_threads (min (4, 5))
  ;
}

struct A { A (); ~A (); };
int foo (const A &);

void
f2 ()
{
  int i;
#pragma omp parallel if (foo (A ())) num_threads (foo (A ()))
  ;
#pragma omp task if (foo (A ()))
  ;
#pragma omp for schedule (static, foo (A ()))
  for (i = 0; i < 10; i++)
    ;
#pragma omp parallel for schedule (static, foo (A ())) \
  if (foo (A ())) num_threads (foo (A ()))
  for (i = 0; i < 10; i++)
    ;
}
