// { dg-do compile }

struct S
{
  void f () {} // { dg-message "previous" }
  int f () { return 0; } // { dg-error "overloaded" }
};
