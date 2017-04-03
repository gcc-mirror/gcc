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
  #pragma omp parallel default (none)
    bar (&a);
  #pragma omp parallel default (none)
    bar (&b);
  #pragma omp parallel default (none)	// { dg-error "enclosing 'parallel'" }
    bar (&c);				// { dg-error "not specified" }
  #pragma omp parallel default (none)
    bar (&d);
  #pragma omp parallel default (none)	// { dg-error "enclosing 'parallel'" }
    bar (&e);				// { dg-error "not specified" }
  #pragma omp parallel default (none)
    bar (&f);
  #pragma omp parallel default (none)	// { dg-error "enclosing 'parallel'" }
    bar (&g);				// { dg-error "not specified" }
  return f;
}
