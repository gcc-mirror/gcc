// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/link7745b.d
// PERMUTE_ARGS:

import imports.link7745b;

bool forceSemantic7745()
{
   C c;
   c.asdfg();
  return true;
}
static assert(forceSemantic7745());

void f(C c) { auto x = &c.asdfg; }

void main() {
    // extra test for bug 4820
    nextis!(int)();
}
