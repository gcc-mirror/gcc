// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

extern "C" void f (); // { dg-message "" } previous declaration
static void f () {} // { dg-error "" } extern redeclared static
