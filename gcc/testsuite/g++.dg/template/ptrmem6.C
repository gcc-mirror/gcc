struct S {};

void g(int S::**);

template <typename T>
void f (int T::* volatile *p) {
  g(p); // { dg-error "" }
}

template void f(int S::* volatile *); // { dg-error "instantiated" }
