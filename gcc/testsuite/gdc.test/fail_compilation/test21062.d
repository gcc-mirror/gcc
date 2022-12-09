/*
TEST_OUTPUT:
---
fail_compilation/test21062.d(16): Error: no identifier for declarator `bool`
fail_compilation/test21062.d(16):        `synchronized` is a keyword, perhaps append `_` to make it an identifier
fail_compilation/test21062.d(17): Error: no identifier for declarator `ubyte*`
fail_compilation/test21062.d(17):        `out` is a keyword, perhaps append `_` to make it an identifier
fail_compilation/test21062.d(21): Error: no identifier for declarator `uint`
fail_compilation/test21062.d(21):        `in` is a keyword, perhaps append `_` to make it an identifier
---
*/

// https://issues.dlang.org/show_bug.cgi?id=21062
// Confusing error when using a keyword as an identifier for a declaration

bool synchronized;
ubyte* out;

void main()
{
    foreach(uint in; [])
    {
    }
}
