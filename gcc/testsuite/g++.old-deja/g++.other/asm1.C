// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S {
  int i asm ("abc"); // ERROR - `asm' specifier not permitted 
};
