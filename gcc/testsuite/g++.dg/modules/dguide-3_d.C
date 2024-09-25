// { dg-additional-options "-fmodules-ts" }
// Test merging deduction guides.

template <typename T> struct S {
  template <typename U> S(U);
};

import A;
import B;
import C;

int main() {
  // declared in A and B
  S x(123);
  S<int> x2 = x;

  // declared only in A
  S y(0.5);
  S<double> y2 = y;

  // declared only in B
  S z('c');
  S<char> z2 = z;

  // declared only in C (and attached to named module)
  S w("hello");
  S<const char*> w2 = w;
}

S(char) -> S<double>;  // { dg-error "ambiguating" }
