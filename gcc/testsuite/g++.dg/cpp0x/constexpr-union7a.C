// PR c++/105491
// { dg-do compile { target c++11 } }

struct V {
  int m = 0;
};
struct S : V {
  constexpr S(int) : b() { }
  bool b;
};
union W {
  constexpr W() : s(0) { }
  S s;
};
constexpr W w;
