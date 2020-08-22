// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-ts" }

template<class X>
concept bool C() {
  return requires(X x, bool b) {
    requires b; // { dg-error "not a constant expression" }
    x++;
  };
}

int main() {
  C<int>();
  return 0;
}
