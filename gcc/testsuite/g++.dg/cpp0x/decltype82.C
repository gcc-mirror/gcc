// PR c++/105756
// { dg-do compile { target c++11 } }

template<int N>
void f() {
  using ty1 = decltype((5 % N) == 0);
  using ty2 = decltype((5 / N) == 0);
}

template void f<0>();
