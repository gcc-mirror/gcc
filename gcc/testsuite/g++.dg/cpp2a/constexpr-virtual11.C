// P1064R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct A
{
  constexpr virtual int f () const { return 1; }
};

struct B : public A
{
  constexpr virtual int f () const { return 2; }
};

struct C
{
  A a;
  B b;
};

constexpr C c;
constexpr const A &d = c.a;
constexpr const A &e = c.b;
constexpr const B &f = c.b;
static_assert (c.a.f () == 1 && c.b.f () == 2);
static_assert (d.f () == 1 && e.f () == 2 && f.f () == 2);
