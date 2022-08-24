// PR c++/105491
// { dg-do compile { target c++11 } }

struct V {
  int m = 0;
};
struct S : V {
  constexpr S(int) : b() { }
  bool b;
};
struct W {
  constexpr W() : s(0) { }
  union {
    S s;
  };
};
constexpr W w;
