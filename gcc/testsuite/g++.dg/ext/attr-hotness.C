/* { dg-do compile } */
/* { dg-options "-O0 -Wattributes -fdump-tree-gimple" } */


struct __attribute((cold)) A {
  __attribute((noinline, used)) void foo(void) { }
  template <class T> void bar() {}
};
template void A::bar<int>();

struct __attribute((hot)) B {
  __attribute((noinline, used)) void foo(void) { }
  template <class T> void bar() {}
};
template void B::bar<int>();

struct __attribute((hot, cold)) C { __attribute((noinline, used)) void foo(void) { } }; /* { dg-warning "ignoring attribute .cold. because it conflicts with attribute .hot." } */

struct __attribute((cold, hot)) D { __attribute((noinline, used)) void foo(void) { } }; /* { dg-warning "ignoring attribute .hot. because it conflicts with attribute .cold." } */


/* { dg-final { scan-tree-dump-times "cold" 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "hot" 3 "gimple" } } */

