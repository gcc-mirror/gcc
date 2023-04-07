// PR c++/108975
// { dg-do compile { target c++11 } }

template<class T>
void f() {
  constexpr int dim = 1;
  auto l = [&] {
    int n[dim * 1];
  };
  // In f<int>, we shouldn't actually capture dim.
  static_assert (sizeof(l) == 1, "");
}

template void f<int>();
