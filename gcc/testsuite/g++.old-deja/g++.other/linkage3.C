// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

extern "C" void f (); // ERROR - previous declaration
static void f () {} // ERROR - extern redeclared static
