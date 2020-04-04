// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

struct A {
  template <class T>
  double operator()(T x) const {
    return 0;
  }
};

template <class X> concept bool C() {
  return requires {
    &X::operator();
  };
}

int main() {
  static_assert(!C<A>());
  return 0;
}
