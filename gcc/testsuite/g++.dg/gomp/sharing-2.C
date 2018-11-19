// { dg-do compile }

struct T
{
  int i;
  mutable int j;
};
struct U
{
  int i, j;
};
struct S
{
  const static int d = 1;
  const static T e;
  const static U f;
  void foo (int, T);
};

const int S::d;
const T S::e = { 2, 3 };
const U S::f = { 4, 5 };

void bar (const int &);

void
S::foo (const int x, const T y)
{
  #pragma omp parallel firstprivate (x)
    bar (x);
  #pragma omp parallel firstprivate (d)
    bar (d);
  #pragma omp parallel firstprivate (y)
    bar (y.i);
  #pragma omp parallel firstprivate (e)	// { dg-error "is predetermined" }
    bar (e.i);
  #pragma omp parallel firstprivate (f)
    bar (f.i);
  #pragma omp parallel shared (x)
    bar (x);
  #pragma omp parallel shared (d)	// { dg-error "is predetermined" }
    bar (d);
  #pragma omp parallel shared (e)	// { dg-error "is predetermined" }
    bar (e.i);
  #pragma omp parallel shared (f)	// { dg-error "is predetermined" }
    bar (f.i);
  #pragma omp parallel shared (y)
    bar (y.i);
  #pragma omp parallel private (x)	// { dg-error "may appear only in 'shared' or 'firstprivate' clauses" }
    bar (x);
  #pragma omp parallel private (d)	// { dg-error "is predetermined" }
    bar (d);
  #pragma omp parallel private (y)
    bar (y.i);
  #pragma omp parallel private (e)	// { dg-error "is predetermined" }
    bar (e.i);
  #pragma omp parallel private (f)	// { dg-error "is predetermined" }
    bar (f.i);
}
