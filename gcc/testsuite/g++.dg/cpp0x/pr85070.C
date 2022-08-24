// { dg-do compile { target c++11 } }

struct A;

struct B
{
  constexpr A & operator= (const A &);
};

struct A : B
{
  using B::operator=;
} a { a = a };
