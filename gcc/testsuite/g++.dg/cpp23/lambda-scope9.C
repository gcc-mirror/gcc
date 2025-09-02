// P2036R3 - Change scope of lambda trailing-return-type
// PR c++/102610
// { dg-do compile { target c++23 } }
// { dg-options "" }

constexpr char f(auto a) { return 'a'; }

namespace A {
  int i = 42;
  template<char X = f([i]{})> void g() { } // { dg-warning "capture of variable .A::i. with non-automatic storage duration" }
}

namespace B {
  void call() { A::g(); }
}
