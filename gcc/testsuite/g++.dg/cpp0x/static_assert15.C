// PR c++/84596
// { dg-do compile { target c++11 } }

template<int x>
struct a {
  constexpr void b() {
    int c;
    static_assert(c %= 1, "");
  }
};
