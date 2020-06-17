// PR c++/94470
// { dg-do compile { target c++14 } }

struct X
{
  int b = (c=5);
  int c = (b=1);
};

constexpr X o = { };
static_assert(o.b == 1 && o.c == 1, "");
