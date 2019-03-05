/*
TEST_OUTPUT:
---
fail_compilation/fail251.d(12): Error: undefined identifier `xs`
fail_compilation/fail251.d(16):        called from here: foo()
fail_compilation/fail251.d(16):        while evaluating: `static assert(foo())`
---
*/

bool foo()
{
    foreach (x; xs) {}
    return true;
}

static assert(foo());
