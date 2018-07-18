// PR c++/79360
// { dg-do compile { target c++14 } }

union U
{
  enum E { e };
};

struct A
{
  U u{};
};
