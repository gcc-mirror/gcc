// PR c++/52718
// { dg-options "-Wzero-as-null-pointer-constant -Wno-return-type" }

struct foo
{
  foo(void* a = 0) {};      // { dg-warning "17: zero as null pointer" }
};

void* fun(void* a = 0) {};  // { dg-warning "zero as null pointer" }

struct bar: foo
{
  bar() {};
};

struct baz
{
  baz(const foo& f1 = foo(),
      void* f2 = fun()) {};
};
