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
  #pragma omp parallel default (none)	// { dg-message "note: enclosing 'parallel'" }
    bar (&a);				// { dg-error "not specified" }
  #pragma omp parallel default (none)	// { dg-message "note: enclosing 'parallel'" }
    bar (&b);				// { dg-error "not specified" }
  #pragma omp parallel default (none)	// { dg-message "note: enclosing 'parallel'" }
    bar (&c);				// { dg-error "not specified" }
  #pragma omp parallel default (none)	// { dg-message "note: enclosing 'parallel'" }
    bar (&d);				// { dg-error "not specified" }
  #pragma omp parallel default (none)	// { dg-message "note: enclosing 'parallel'" }
    bar (&e);				// { dg-error "not specified" }
  #pragma omp parallel default (none)	// { dg-message "note: enclosing 'parallel'" }
    bar (&f);				// { dg-error "not specified" }
  #pragma omp parallel default (none)	// { dg-message "note: enclosing 'parallel'" }
    bar (&g);				// { dg-error "not specified" }
  return f;
}
