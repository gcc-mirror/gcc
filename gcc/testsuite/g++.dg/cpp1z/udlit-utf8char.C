// { dg-do compile { target c++17 } }

typedef decltype(u8'c') u8_char_t;

constexpr int
operator""_foo(u8_char_t c)
{ return c * 100; }

auto cc = u8'8'_foo;
