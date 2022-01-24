// PR c++/102786
// { dg-do compile { target c++20 } }

struct S {
  virtual constexpr int foo () const { return 42; }
};

constexpr S s;
constexpr auto a = &S::foo;
constexpr auto b = (s.*a) ();
constexpr auto c = (s.*&S::foo) ();
