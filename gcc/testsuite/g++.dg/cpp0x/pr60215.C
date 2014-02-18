// PR c++/60215
// { dg-do compile { target c++11 } }

struct A
{
  void foo();
  int i : foo;  // { dg-error "width" }
};
