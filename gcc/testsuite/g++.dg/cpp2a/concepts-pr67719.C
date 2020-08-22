// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-ts" }

template<class X> concept bool C() {
  return __is_same_as(X, int) || __is_same_as(X, long);
}

template<C... Tx>
struct Ax {};

int main() {
  Ax<int, long> a;
  Ax<int, long, void> b; // { dg-error "template constraint failure" }
  return 0;
}
