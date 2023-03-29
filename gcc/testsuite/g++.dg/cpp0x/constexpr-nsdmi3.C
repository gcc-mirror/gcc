// PR c++/101073
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fno-elide-constructors" }

struct Y
{
  int a;
};

struct X
{
  Y b = Y{1};
  Y c = this->b;
};

constexpr X o = { };
static_assert(o.b.a == 1 && o.c.a == 1, "");
