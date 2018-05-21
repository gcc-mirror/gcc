// PR libstdc++/85843
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wextra }

struct A
{
  A();
  A(const A&) = default;
};

struct B : A
{
  B(): A() { }
  B(const B&) { }
};
