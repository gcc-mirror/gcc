// { dg-do compile { target c++14 } }

union U;
constexpr int foo(U *up);

union U {
  int a = foo(this); int y;
};

constexpr int foo(U *up) {
  up->a++;
  return {42};
}

extern constexpr U u = {}; // { dg-error "accessing uninitialized member" }
