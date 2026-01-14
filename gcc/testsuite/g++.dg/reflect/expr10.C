// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections in consteval blocks.

using info = decltype(^^void);

consteval info foo (info i) { return i; }

constexpr info u = ^^int;

consteval {
  auto z = ^^int;
  u;
  ^^double;
  static constexpr auto gl = ^^double;
  foo (^^int);
}

struct S {
  consteval {
    auto z = ^^int;
    ^^double;
    static constexpr auto gl = ^^double;
    foo (^^int);
  }
};

void
g ()
{
  constexpr info r = ^^int;
  consteval {
    r;
    ^^double;
    auto l = ^^double;
    static constexpr auto gl = ^^double;
    foo (^^int);
  }
}
