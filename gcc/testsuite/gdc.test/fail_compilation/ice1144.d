/*
TEST_OUTPUT:
---
fail_compilation/ice1144.d(14): Error: undefined identifier `a`
fail_compilation/ice1144.d(23): Error: template instance ice1144.testHelper!("hello", "world") error instantiating
---
*/

// Issue 1144 - ICE(template.c) template mixin causes DMD crash

char[] testHelper(A ...)()
{
    char[] result;
    foreach (t; a)
    {
        result ~= "int " ~ t ~ ";\n";
    }
    return result;
}

void main()
{
    mixin(testHelper!("hello", "world")());
}
