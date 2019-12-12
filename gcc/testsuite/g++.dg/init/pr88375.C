// { dg-do compile { target c++11 } }

enum struct a : int {
  one, two
};

constexpr int fn () { return 42; }

struct foo {
  int e1, e2;
  a e3;
} arr[] = {
  { 1, 2, a::one },
  { 3, a::two }, // { dg-error "11: cannot convert 'a' to 'int' in initialization" }
  { 6, 7, 8 }, // { dg-error "11: cannot convert 'int' to 'a' in initialization" }
  { 6, 7, fn() }, // { dg-error "13: cannot convert 'int' to 'a' in initialization" }
};

struct bar {
  const char *f1;
  int f2;
} arr_2[] = {
  { "hello world", 42 },
  { 42 }, // { dg-error "5: invalid conversion from 'int' to 'const char\\*'" }
  { "hello", "world" }, // { dg-error "14: invalid conversion from 'const char\\*' to 'int'" }
};
