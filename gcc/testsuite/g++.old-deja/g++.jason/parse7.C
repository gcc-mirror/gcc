// Bug: g++ tries to parse this as a constructor.
// Build don't link:

typedef int foo;
struct A {
  foo (*bar)();
};
