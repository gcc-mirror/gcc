// PR c++/35328
// { dg-do compile }
// { dg-options "-fopenmp" }

struct A
{
  ~A ()();		// { dg-error "declared as function returning a function" }
};
struct B
{
  B ()();		// { dg-error "declared as function returning a function" }
};
struct C
{
  C ();
  C (const C &)();	// { dg-error "declared as function returning a function" }
};

void
foo ()
{
  A a;
  B b;
  C c;
  #pragma omp parallel firstprivate (a)
    ;
  #pragma omp parallel private (b)
    ;
  #pragma omp parallel firstprivate (c)
    ;
}
