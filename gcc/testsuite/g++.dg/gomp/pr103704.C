// PR c++/103704
// { dg-do compile }

struct S { int a; };

template <typename T>
struct U : public T {
  T a;
  U ()
  {
#pragma omp target
#pragma omp teams
#pragma omp distribute private(a)
    for (int k = 0; k < 1; ++k)
      ;
  }
};

struct V : public U<S> { V () : U<S> () {} };
