// { dg-do compile }

struct S
{
  void f () {} // { dg-error "with" "" }
  int f () { return 0; } // { dg-error "overloaded" "" }
};
