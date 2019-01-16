// REQUIRED_ARGS: -transition=checkimports -de
/*
TEST_OUTPUT:
---
fail_compilation/checkimports1a.d(16): Deprecation: local import search method found struct imports.diag12598a.lines instead of variable checkimports1a.C.lines
---
*/


// new lookup + information
class C
{
    void f()
    {
        import imports.diag12598a;
        lines ~= "";
    }

    string[] lines;
}
