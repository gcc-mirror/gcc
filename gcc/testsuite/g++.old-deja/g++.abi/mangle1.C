// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S {};

void f (void (S::*)()) {}
void f (void (S::*)() const) {}
