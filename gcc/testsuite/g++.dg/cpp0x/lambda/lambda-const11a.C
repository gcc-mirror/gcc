// PR c++/108975
// A version of lambda-const11.C using a generic lambda.
// { dg-do compile { target c++14 } }

template<int> void g();
template<int> struct A { };

template<class T>
void f() {
  constexpr int dim = 1;
  auto l = [&](auto) {
    int n[dim * 1];
    using ty1 = decltype(g<dim * 2>());
    using ty2 = A<dim * 3>;
  };
  l(0);
  // In f<int>, we shouldn't actually capture dim.
  static_assert (sizeof(l) == 1, "");
}

template void f<int>();
