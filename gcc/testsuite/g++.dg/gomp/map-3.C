// PR c++/106858
// { dg-additional-options "-fopenmp -fsanitize=undefined" }

class A {
  void f() {
    #pragma omp target map(this->f) // { dg-error "member function" }
    ;
  }
};
