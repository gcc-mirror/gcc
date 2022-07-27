// PR c++/105931
// { dg-do compile { target c++11 } }
// A version of decltype82.C where N is unsigned.

template<unsigned N>
void f() {
  using ty1 = decltype((5 % N) == 0);
  using ty2 = decltype((5 / N) == 0);
}

template void f<0>();
