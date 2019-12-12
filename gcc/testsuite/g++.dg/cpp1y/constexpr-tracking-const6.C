// PR c++/91264
// { dg-do compile { target c++14 } }

struct X {
  mutable int j;
  constexpr X() : j(0) { }
};

struct Y {
  X x;
  constexpr Y() : x{} { }
};

constexpr void
g ()
{
  const Y y;
  Y *p = const_cast<Y *>(&y);
  p->x.j = 99;
}

static_assert((g(), 1), "");
