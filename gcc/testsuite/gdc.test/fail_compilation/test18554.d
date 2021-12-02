/* REQUIRED_ARGS: -preview=dip1000
EXTRA_FILES: imports/imp18554.d
TEST_OUTPUT:
---
fail_compilation/test18554.d(16): Error: struct `imp18554.S` variable `i` is not accessible from `@safe` code
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18554

import imports.imp18554;

void test1() @safe
{
    S s;
    s.tupleof[0] = 1;
}

void test2()
{
    S s;
    s.tupleof[0] = 1;
}

