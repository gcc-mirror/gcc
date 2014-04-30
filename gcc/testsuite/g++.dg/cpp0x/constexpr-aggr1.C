// PR c++/60951
// { dg-do compile { target c++11 } }

struct Foo {
  constexpr Foo(int x = 0) : memb(x) {}
  int memb;
};

struct FooContainer {
  Foo foo[2];
};

void fubar() {
  int nonConst = 0;
  FooContainer fooContainer;
  fooContainer = { { 0, nonConst } };
}
