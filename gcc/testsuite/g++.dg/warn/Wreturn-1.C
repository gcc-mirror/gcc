// { dg-options "-Wreturn-type" }
// PR c++/15742

extern void exit(int) __attribute__ ((noreturn));

template<typename T>
struct A {
  int find_cmp(void) { exit(1); }
};
