// PR c++/61465
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-but-set-parameter" }

struct Foo {
  Foo(void* x) : y{static_cast<char*>(x)} {}
  char* y;
};
