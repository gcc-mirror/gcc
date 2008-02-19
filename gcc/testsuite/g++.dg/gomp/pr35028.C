// PR c++/35028
// { dg-do compile }
// { dg-options "-fopenmp" }

struct A
{
  A ();
  A (const A &, ...);
  ~A ();
  A operator++ (int);
};

void
foo ()
{
  A a;
  #pragma omp parallel firstprivate (a)
    a++;
}
