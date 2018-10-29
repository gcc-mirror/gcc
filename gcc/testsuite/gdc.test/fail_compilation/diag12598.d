/*
REQUIRED_ARGS: -transition=import
TEST_OUTPUT:
---
fail_compilation/diag12598.d(14): Error: struct 'lines' is a type, not an lvalue
---
*/

class C
{
    void f()
    {
        import imports.diag12598a;
        lines ~= "";
    }

    string[] lines;
}

void main()
{
}
