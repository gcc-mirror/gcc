// PR c++/58700

struct A
{
  static int : 4;  // { dg-error "bit-field" }
};
