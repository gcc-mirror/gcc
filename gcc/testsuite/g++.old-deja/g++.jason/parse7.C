// { dg-do assemble  }
// Bug: g++ tries to parse this as a constructor.

typedef int foo;
struct A {
  foo (*bar)();
};
