// { dg-do compile }
// { dg-options "-std=c++17" }

constexpr int
operator""_foo(char c)
{ return c * 100; }

auto cc = u8'8'_foo;
