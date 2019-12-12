// REQUIRED_ARGS: -transition=import -transition=checkimports
/*
TEST_OUTPUT:
---
fail_compilation/checkimports1b.d(16): Deprecation: local import search method found struct imports.diag12598a.lines instead of variable checkimports1b.C.lines
fail_compilation/checkimports1b.d(16): Error: struct 'lines' is a type, not an lvalue
---
*/

// old lookup + information
class C
{
    void f()
    {
        import imports.diag12598a;
        lines ~= "";
    }

    string[] lines;
}
