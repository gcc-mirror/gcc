// { dg-do compile }
// { dg-options "-fopenmp" }

struct A { A (); ~A (); int i; };

template <typename T> void bar (T &);

const A a;

void foo (A &p)
{
  const A &q = a;
#pragma omp task	// { dg-error "has reference type" }
  bar (p);
#pragma omp task	// { dg-error "has reference type" }
  bar (q);
}
