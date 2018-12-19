// PR c++/87934
// { dg-do compile { target c++11 } }

struct Foo
{
  enum { BAR } bar = BAR;
};

constexpr Foo foo{};
