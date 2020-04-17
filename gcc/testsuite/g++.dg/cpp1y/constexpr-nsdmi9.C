// PR c++/94470
// { dg-do compile { target c++14 } }

struct Y
{
  int a;
};

struct X
{
  Y b = (c={5});
  Y c = (b={1});
};

constexpr X o = { };
static_assert(o.b.a == 1 && o.c.a == 1, "");
