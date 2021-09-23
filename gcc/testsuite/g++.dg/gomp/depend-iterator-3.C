// PR c++/100859

struct S {
  S () {}
};

struct W {
  S c[10];
  W () {
#pragma omp task affinity (iterator (i = 0 : 10 : 1): c[i])
    ;
#pragma omp task depend (iterator (i = 0 : 10 : 1), inout: c[i])
    ;
#pragma omp task affinity (this[0])
    ;
#pragma omp task depend (inout: this[0])
    ;
#pragma omp taskwait
  }
};

template <typename T>
struct U {
  T c[10];
  U () {
#pragma omp task affinity (iterator (i = 0 : 10 : 1): c[i])
    ;
#pragma omp task depend (iterator (i = 0 : 10 : 1), inout: c[i])
    ;
#pragma omp task affinity (this[0])
    ;
#pragma omp task depend (inout: this[0])
    ;
#pragma omp taskwait
  }
};

struct V : public U<S> {
  V () : U<S> () {}
};

W w;
V v;
