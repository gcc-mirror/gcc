// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-ts" }

template<class T> concept bool C1() { return false; }
template<C1 T> concept bool C2() { return true; } // { dg-error "cannot be constrained" }

void f(C2 x) {
}

struct A {} a;

int main() {
  f(a);
}
