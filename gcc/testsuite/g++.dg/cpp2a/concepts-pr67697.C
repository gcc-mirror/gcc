// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template<class X>
concept C =
  requires(X x, bool b) {
    requires b; // { dg-error "not a constant expression" }
    x++;
  };

int main() {
  C<int>;
  return 0;
}
