// { dg-do compile }

struct S
{
  void f () {}
  int f () { return 0; } // { dg-error "" "" }
};
