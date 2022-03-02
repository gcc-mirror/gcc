// PR c++/104513
// { dg-do compile { target c++20 } }

struct S {
  constexpr S () : s (nullptr) {}
  constexpr ~S () { delete s; }
  int *s;
};
struct T : S {
  constexpr T () : S () {}
  constexpr ~T () { s = new int (42); return; }
};
constexpr T t;
