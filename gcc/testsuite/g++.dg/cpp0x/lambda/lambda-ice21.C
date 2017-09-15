// PR c++/78648
// { dg-do compile { target c++11 } }

template <typename F> void e(F) {}

template <int> void bar() {
  e([](const void) {}); // { dg-error "invalid use" }
}

void baz() { bar<1>; }
