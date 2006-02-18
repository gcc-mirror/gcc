// PR c++/26266

template <int I>
struct S;

template <int I>
void f() {
  if (const int i = 3) {
    S<i>::j; // { dg-error "incomplete" }
  }
}
