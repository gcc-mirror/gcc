/*
TEST_OUTPUT:
---
fail_compilation/test21062.d(16): Error: variable name expected after type `bool`, not `synchronized`
fail_compilation/test21062.d(16):        `synchronized` is a keyword, perhaps append `_` to make it an identifier
fail_compilation/test21062.d(17): Error: variable name expected after type `ubyte*`, not `out`
fail_compilation/test21062.d(17):        `out` is a keyword, perhaps append `_` to make it an identifier
fail_compilation/test21062.d(21): Error: variable name expected after type `uint`, not `in`
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
