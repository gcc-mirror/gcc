// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

constexpr info A {};
constexpr info B = ^^::;
inline constexpr info C = ^^::;

template<typename T>
struct S {
  static constexpr info B {^^T};

  void wow () {
    constexpr info D {^^T};
  }
};

consteval void
foo ()
{
  info A = ^^::;
  constexpr info B = ^^::;
}

constexpr void
bar ()
{
  constexpr info A = ^^::;
  static constexpr info B = ^^::;
}

constexpr void
tux ()
{
  if consteval {
    constexpr info A = ^^::;
    static constexpr info B = ^^::;
  }
}

void
qux ()
{
  constexpr info A {};
  constexpr info B = ^^::;
  static constexpr info C = ^^::;
  foo ();
  consteval { bar (); }
  consteval { tux (); }
  tux ();
  bar ();
}

template struct S<int>;
