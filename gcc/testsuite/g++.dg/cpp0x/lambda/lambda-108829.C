// PR c++/108829
// { dg-do compile { target c++11 } }

template <int>
void f(void) {
  constexpr int IDX_PAGE_SIZE = 4096;
  int abyPage = [=, abyPage] { return IDX_PAGE_SIZE; }(); // { dg-error "redundant" }
}
void h() {
  f<1>();
}
