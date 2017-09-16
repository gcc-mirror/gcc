// PR c++/81154
// { dg-do compile }

template <typename T>
struct C
{
  int foo (T n) const
  {
#pragma omp parallel shared (foo)	// { dg-error "is not a variable in clause" }
    ;
#pragma omp parallel private (foo)	// { dg-error "is not a variable in clause" }
    ;
#pragma omp parallel firstprivate (foo)	// { dg-error "is not a variable in clause" }
    ;
#pragma omp parallel for lastprivate (foo)	// { dg-error "is not a variable in clause" }
    for (T i = 0; i < n; i++)
      ;
#pragma omp parallel reduction (+:foo)	// { dg-error "is not a variable in clause" }
    ;
    return 0;
  }
  int foo (int x, int y) { return x; }
};

struct D
{
  typedef int T;
  int foo (T n) const
  {
#pragma omp parallel shared (foo)	// { dg-error "is not a variable in clause" }
    ;
#pragma omp parallel private (foo)	// { dg-error "is not a variable in clause" }
    ;
#pragma omp parallel firstprivate (foo)	// { dg-error "is not a variable in clause" }
    ;
#pragma omp parallel for lastprivate (foo)	// { dg-error "is not a variable in clause" }
    for (T i = 0; i < n; i++)
      ;
#pragma omp parallel reduction (+:foo)	// { dg-error "is not a variable in clause" }
    ;
    return 0;
  }
  int foo (int x, int y) { return x; }
};

int
main ()
{
  C<int> ().foo (1);
  D ().foo (1);
}
