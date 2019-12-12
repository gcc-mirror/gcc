// PR c++/89241
// { dg-do compile { target c++14 } }

template <typename al> void m(al p) {
  p(1);
}

template <typename ax> void f() {
  m([](auto) { __func__; });
}

template void f<int>();
