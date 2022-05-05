// PR c++/100030
// { dg-do compile { target c++11 } }

template <class... Ts>
void sink(Ts...);

template <class... Ts>
void f(Ts...) {
  sink([] { struct alignas(Ts) S {}; }...); // { dg-bogus "" "" { xfail *-*-* } }
}

int main() {
  f(0);
}
