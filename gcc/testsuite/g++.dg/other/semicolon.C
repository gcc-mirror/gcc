// PR c++/18368
// Origin: Chris Lattner <sabre@nondot.org>
// { dg-do compile }
// { dg-options "-fshow-column" }

struct A
{
  struct B { int i; } // { dg-error "3: error: new types may not be defined in a return type|note: \\(perhaps a semicolon is missing" }
  void foo();   // { dg-error "12: error: two or more|return type" }
};
