// { dg-do compile }

struct H2 { int f (); };
struct J2 : H2
{
  struct f {};
  using H2::f;
};
