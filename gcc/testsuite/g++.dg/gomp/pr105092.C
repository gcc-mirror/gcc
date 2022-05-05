// PR c++/105092
// { dg-do compile { target analyzer } }
// { dg-options "-fanalyzer -fopenmp" }

struct S { S () {} };

template <typename T>
struct U {
  T c[10];
  U () {
#pragma omp task affinity (iterator (i = 0 : 10 : 1): c[i])
    ;
  }
};

template <typename T>
struct V {
  T c[10];
  V () {
#pragma omp task depend (iterator (i = 0 : 10 : 1), inout: c[i])
    ;
  }
};

U<S> u;
V<S> v;
