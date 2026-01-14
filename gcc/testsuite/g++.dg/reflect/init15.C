// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

consteval info identity(info i) { return i; }
consteval int something_of(info) { return 10; }
consteval void do_something(info) {}

void fox() {
  if not consteval {
    constexpr info X {};
    constexpr info A = ^^::;
    constexpr info B = identity(A);
    static constexpr info D = ^^::;

    static_assert(X == info{});
    static_assert(A == ^^::);
    static_assert(B == ^^::);
    static_assert(D == ^^::);

    consteval {
      do_something(X);
      do_something(A);
      int H = something_of(A);
      constexpr int I = something_of(A);
    }
  }
}
