// PR c++/45520
// { dg-options -std=c++0x }

struct M {
  int i;
};

struct S {
  M m;

  void f() {
    auto lambda=[&](decltype(m.i) & i) { };
  }
};
