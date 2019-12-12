// REQUIRED_ARGS:
// PERMUTE_ARGS: -unittest
// EXTRA_SOURCES: imports/test14901a.d imports/test14901b.d imports/test14901c.d imports/test14901d.d
// COMPILE_SEPARATELY

module test14901;

import imports.test14901c;
import imports.test14901d;

extern(C) __gshared static int initCount;

extern(C) int printf(const char*, ...);

void main()
{
    caller1();
    caller2();
    assert(initCount == 1);
}
