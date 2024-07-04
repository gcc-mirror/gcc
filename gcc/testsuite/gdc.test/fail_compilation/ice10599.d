// https://issues.dlang.org/show_bug.cgi?id=10599
// ICE(interpret.c)
/*
TEST_OUTPUT:
---
fail_compilation/ice10599.d(13): Error: cannot implicitly convert expression `3.45` of type `double` to `int`
fail_compilation/ice10599.d(21):        called from here: `bug10599()`
fail_compilation/ice10599.d(21):        while evaluating: `static assert(bug10599())`
---
*/

struct Bug {
    int val = 3.45;
}
int bug10599()
{
    Bug p = Bug();
    return 1;
}

static assert(bug10599());
