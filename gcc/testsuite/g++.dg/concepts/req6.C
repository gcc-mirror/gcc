// { dg-options "-std=c++1z -fconcepts" }

struct X { };
int operator==(X, X) { return 0; }

template<typename T>
  concept bool C1() { return X(); }

template<C1 T>
  void h(T) { } // { dg-error "not|bool" }

template<typename T>
  concept bool C2() { return X() == X(); } // OK
