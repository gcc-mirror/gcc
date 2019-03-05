// { dg-do compile }
// { dg-options "-fopenmp" }

struct A { int i; A (); ~A (); };
struct B { int i; };
struct C { int i; mutable int j; C (); ~C (); };

template <typename T> void bar (const T *);

const A a;
const C c;

const A foo (const A d, const C e)
{
  const A f;
  const B b = { 4 };
  A g;
  #pragma omp parallel private (a)	// { dg-error "may appear only in 'shared' or 'firstprivate' clauses" }
    bar (&a);
  #pragma omp parallel private (b)	// { dg-error "may appear only in 'shared' or 'firstprivate' clauses" }
    bar (&b);
  #pragma omp parallel private (c)
    bar (&c);
  #pragma omp parallel private (d)	// { dg-error "may appear only in 'shared' or 'firstprivate' clauses" }
    bar (&d);
  #pragma omp parallel private (e)
    bar (&e);
  #pragma omp parallel private (f)	// { dg-error "may appear only in 'shared' or 'firstprivate' clauses" }
    bar (&f);
  #pragma omp parallel private (g)
    bar (&g);
  return f;
}
