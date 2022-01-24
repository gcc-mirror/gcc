/*
TEST_OUTPUT:
---
fail_compilation/diag15974.d(21): Error: variable `f` cannot be read at compile time
fail_compilation/diag15974.d(21):        called from here: `format("%s", f)`
fail_compilation/diag15974.d(26): Error: variable `f` cannot be read at compile time
fail_compilation/diag15974.d(26):        called from here: `format("%s", f)`
---
*/

void test15974()
{
    string format(Args...)(string fmt, Args args)
    {
        return "";
    }

    string f = "vkCreateSampler";

    // CompileStatement
    mixin(format("%s", f));

    struct S
    {
        // CompileDeclaration
        mixin(format("%s", f));
    }
}
