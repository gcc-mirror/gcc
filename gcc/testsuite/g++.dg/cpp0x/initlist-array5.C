// PR c++/80415
// { dg-do compile { target c++11 } }

struct A {
  A(int, int, const int (&)[1] = {});
};
A fn1() { return {0, 0}; }
