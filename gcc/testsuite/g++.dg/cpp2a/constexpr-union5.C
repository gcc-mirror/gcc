// { dg-do compile { target c++20 } }

union U { int a; int b; int c[2]; };

constexpr int test1() {
  U u;
  u.a = 10;
  *&u.b = 20;  // { dg-error "accessing" }
  return u.b;
}
constexpr int x1 = test1();  // { dg-message "in .constexpr. expansion" }

constexpr int test2() {
  U u;
  u.a = 10;
  (0, u.b) = 20;  // { dg-error "accessing" }
  return u.b;
}
constexpr int x2 = test2();  // { dg-message "in .constexpr. expansion" }

constexpr int test3() {
  U u;
  u.a = 0;
  int* p = &u.b;
  p[u.a] = 10;  // { dg-error "accessing" }
  return u.b;
}
constexpr int x3 = test3();  // { dg-message "in .constexpr. expansion" }

constexpr int test4() {
  U u;
  u.a = 0;
  int* p = &u.b;
  u.a[p] = 10;  // { dg-error "accessing" }
  return u.b;
}
constexpr int x4 = test4();  // { dg-message "in .constexpr. expansion" }

struct S { U u[10]; };
constexpr int test5() {
  S s;
  s.u[4].a = 10;
  6[s.u].b = 15;
  return 4[s.u].a + s.u[6].b;
}
static_assert(test5() == 25);

constexpr int test6() {
  U u;
  u.a = 5;
  u.c[0] = 3;
  1[u.c] = 8;
  return 1[u.c] + u.c[0];
}
static_assert(test6() == 11);

constexpr int test7() {
  U u;  // default initialisation leaves no member initialised
  int* p = &u.a;
  *p = 10;  // { dg-error "accessing" }
  return *p;
}
constexpr int x7 = test7();  // { dg-message "in .constexpr. expansion" }

constexpr int test8() {
  U u {};  // value initialisation initialises first member
  int* p = &u.a;
  *p = 8;
  return *p;
}
static_assert(test8() == 8);

union V { int :0; static int x; void foo(); int a; };
constexpr int test9() {
  V v {}; // should skip zero-width bit fields, static members, and functions
  int* p = &v.a;
  *p = 9;
  return *p;
}
static_assert(test9() == 9);
