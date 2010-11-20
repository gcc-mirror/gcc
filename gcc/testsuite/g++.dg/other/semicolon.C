// PR c++/18368
// Origin: Chris Lattner <sabre@nondot.org>
// { dg-do compile }
// { dg-options "-fshow-column" }

struct A
{
  struct B { int i; } // { dg-error "after struct definition" }
  void foo();
};
