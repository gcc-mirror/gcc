// PR c++/104668
// { dg-do compile { target c++11 } }
// { dg-excess-errors "" }

template <class... Ts>
void sink(Ts...);
template <class... Ts>
void f(Ts...) {
  sink([] { struct alignas:Ts) S {}; }...); }
}
int main() {
  f(0);
}
