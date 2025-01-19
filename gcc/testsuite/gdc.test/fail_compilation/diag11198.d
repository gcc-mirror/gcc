/*
TEST_OUTPUT:
---
fail_compilation/diag11198.d(13): Error: version `blah` declaration must be at module level
fail_compilation/diag11198.d(14): Error: debug `blah` declaration must be at module level
fail_compilation/diag11198.d(15): Error: identifier expected, not `""`
fail_compilation/diag11198.d(16): Error: identifier expected, not `""`
---
*/

void main()
{
    version = blah;
    debug = blah;
    version = "";
    debug = "";
}
