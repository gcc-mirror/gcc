// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that we properly detect compounded consteval-only types.

using info = decltype(^^int);
constexpr static info glob = ^^::;
constexpr info array[3] = { glob, glob, glob };
consteval info foo () { return ^^::; }

struct A {
  info i = ^^int;
};
constexpr A a;

struct B {
  A a;
};
constexpr B b;

struct C {
  const info *i = &glob;
};
constexpr C c;

struct D {
  int i;
  C c;
  consteval D() : c{}, i{42} { }
};
constexpr D d;

struct E {
  info arr[2];
  consteval E() : arr{glob, glob} { }
};
constexpr E e;

struct F {
  const info &r;
  consteval F() : r{glob} { }
};
constexpr F f;

struct G {
  B b;
};
constexpr G g;

struct H {
  info (*fp)();
};
constexpr H h{foo}; // { dg-error "address of immediate function" }

union U {
  int n;
  info i;
};
constexpr U u{.i = glob };

struct I {
  info i;
};
constexpr info I::*pmi = &I::i;
