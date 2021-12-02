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
    // https://issues.dlang.org/show_bug.cgi?id=4820
    nextis!(int)();
}
