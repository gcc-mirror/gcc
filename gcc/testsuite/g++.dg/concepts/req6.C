// { dg-options "-std=c++1z -fconcepts" }

struct X { };
int operator==(X, X) { return 0; }

template<typename T>
  concept bool C1() { return X(); }

template<C1 T>
  void h(T) { } // OK until used.

void f()
{
  h(0); // { dg-error "does not have|cannot call" }
}

template<typename T>
  concept bool C2() { return X() == X(); } // OK
