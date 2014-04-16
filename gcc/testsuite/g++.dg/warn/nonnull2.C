// PR c++/60764
// { dg-options "-Wall" }

struct foo
{
  foo () __attribute__ ((nonnull (1)));
};

const foo &x = foo (); // { dg-bogus "null argument" }
foo y = foo (); // { dg-bogus "null argument" }
