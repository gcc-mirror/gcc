// PR c++/28052
// { dg-do compile }

struct A
{
  double d : 2;  // { dg-error "10:bit-field .d. with non-integral type .double." }
  A() {}
  ~A() {}
};
