// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

struct X { };
int operator==(X, X) { return 0; }

template<typename T>
  concept bool C1() { return X(); } // { dg-error "bool" }

template<C1 T>
  void h(T) { } // OK until used.

void f()
{
  h(0); // { dg-error "does not have|cannot call" }
}

template<typename T>
  concept bool C2() { return X() == X(); } // OK
