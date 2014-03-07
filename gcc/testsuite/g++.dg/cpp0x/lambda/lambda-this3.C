// PR c++/45520
// { dg-do compile { target c++11 } }

struct M {
  int i;
};

struct S {
  M m;

  void f() {
    auto lambda=[&](decltype(m.i) & i) { };
  }
};
