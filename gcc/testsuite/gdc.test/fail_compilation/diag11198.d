/*
TEST_OUTPUT:
---
fail_compilation/diag11198.d(11): Error: version conditions can only be declared at module scope
fail_compilation/diag11198.d(12): Error: debug conditions can only be declared at module scope
---
*/

void main()
{
    version = blah;
    debug = blah;
}
