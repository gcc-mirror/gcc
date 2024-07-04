// PR c++/113242
// { dg-do compile { target c++20 } }

struct wrapper { int n; };

template<const wrapper& X>
void f1() {
  static_assert(X.n == 42);
}

template<const wrapper* X>
void f2() {
  static_assert(X->n == 42);
}

template<wrapper X>
void g() {
  f1<X>();
  f2<&X>();
}

int main() {
  constexpr wrapper X = {42};
  g<X>();
}
