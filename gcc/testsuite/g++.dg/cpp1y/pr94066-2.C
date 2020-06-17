// PR c++/94066
// { dg-do compile { target c++14 } }

struct A { long x; };

union U;
constexpr A foo(U *up);

union U {
  U() = default;
  A a = foo(this); int y;
};

constexpr A foo(U *up) {
  up->y = 11;  // { dg-error "'U::a' to 'U::y'" }
  return {42};
}

extern constexpr U u = {};
