// PR c++/94219
// { dg-do compile { target c++14 } }

struct A { long x; };

struct U;
constexpr A foo(U *up);

struct U {
  U() = default;
  int y; A a = foo(this);
};

constexpr A foo(U *up) {
  up->y = 11;
  return {42};
}

extern constexpr U u = {};

static_assert(u.y == 11, "");
static_assert(u.a.x == 42, "");
