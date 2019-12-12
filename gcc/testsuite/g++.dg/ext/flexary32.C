/* { dg-do compile { target c++11 } } */
/* { dg-options -Wno-pedantic } */

struct str { int len; char s[]; };

struct foo {
  str x = {3, {1,2,3}}; /* { dg-error "(non-static)|(initialization)" } */
  foo() {}
};

struct bar {
  static constexpr str x = {3, {1,2,3}};
  bar() {}
};

struct baz {
  str x = {3};
  baz() {}
};
