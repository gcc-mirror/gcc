// PR c++/87097
// { dg-do compile { target c++11 } }

struct A {
  constexpr A() : data() {}
  struct X { int n; };
  X data[2];
};

static_assert((A(), true), "");
static_assert(A().data[0].n == 0, "");
static_assert(A().data[1].n == 0, "");
constexpr A x;
