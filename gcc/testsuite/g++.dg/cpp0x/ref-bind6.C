// PR c++/91889 - follow-up fix for DR 2352.
// { dg-do compile { target c++11 } }

template <typename T> struct A {
  A(const T &);
};

struct {
  int *m;
} a;

void fn1() { A<const int *>(a.m); }
