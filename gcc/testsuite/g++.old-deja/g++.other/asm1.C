// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S {
  int i asm ("abc"); // { dg-error "" } `asm' specifier not permitted 
};
