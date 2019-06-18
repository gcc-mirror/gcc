/*
EXTRA_SOURCES: imports/testminitAA.d imports/testminitBB.d
PERMUTE_ARGS:
RUN_OUTPUT:
---
AA
BB
hello
Success
---
*/

import core.stdc.stdio;

import imports.testminitAA;
private import imports.testminitBB;

static this()
{
    printf("hello\n");
    assert(aa == 1);
    assert(bb == 1);
}

int main()
{
    printf("Success\n");
    return 0;
}
