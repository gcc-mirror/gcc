// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

struct A {
  template <class T>
  double operator()(T x) const {
    return 0;
  }
};

template <class X> concept C =
  requires {
    &X::operator();
  };

int main() {
  static_assert(!C<A>);
  return 0;
}
