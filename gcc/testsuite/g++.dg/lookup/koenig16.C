// Before P1787 (C++20), only hidden friends are included in ADL.
// After P1787, all friends are included.

namespace N {
  namespace NN {
    struct A;
  }
  using NN::A;
  void fn (A);
  namespace NN {
    struct A {
      friend void N::fn (A);
    };
  }
  void fn (A) { }
}

int main()
{
  N::A a;
  fn(a);			// { dg-error "not declared" "" { target c++17_down } }
}
