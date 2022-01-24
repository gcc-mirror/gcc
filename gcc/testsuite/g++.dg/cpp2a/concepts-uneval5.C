// PR c++/103714
// { dg-do compile { target c++20 } }

template<int I>
struct A {
  static const int i = I;

  template<int J>
  void f(A<J> d = {}) requires (d.i != i) {
    f<I>(); // { dg-error "no match" }
  }
};

int main() {
  A<0> a;
  a.f<1>();
}
