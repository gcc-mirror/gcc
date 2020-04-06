// PR c++/94066
// { dg-do compile { target c++14 } }

union U;
constexpr int foo(U *up);

union U {
  int a = foo(this); int y;
};

constexpr int foo(U *up) {
  up->y = 11; // { dg-error "'U::a' to 'U::y'" }
  return {42};
}

extern constexpr U u = {};
