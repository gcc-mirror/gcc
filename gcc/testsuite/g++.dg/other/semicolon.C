// PR c++/18368
// Origin: Chris Lattner <sabre@nondot.org>
// { dg-do compile }

struct A
{
  struct B { int i; }
  void foo();   // { dg-error "two or more|return type" }
};              // { dg-error "semicolon is missing" "" { target *-*-* } 8 }
