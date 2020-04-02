// PR c++/94219
// { dg-do compile { target c++14 } }

struct A { long x; };

struct U;
constexpr A foo(U *up);

struct U {
  A a = foo(this); int y;
};

constexpr A foo(U *up) {
  up->y = 11;
  return {42};
}

extern constexpr U u = {};

static_assert(u.y == 11, "");
static_assert(u.a.x == 42, "");
