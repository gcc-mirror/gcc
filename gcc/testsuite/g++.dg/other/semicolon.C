// PR c++/18368
// Origin: Chris Lattner <sabre@nondot.org>
// { dg-do compile }
// { dg-options "-fshow-column" }

struct A
{
  struct B { int i; } // { dg-error "3:new types may not be defined in a return type" }
                      // { dg-message "perhaps a semicolon is missing" "note" { target *-*-* } 8 }
  void foo();   // { dg-error "12:two or more" }
};
