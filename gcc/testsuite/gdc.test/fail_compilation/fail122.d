/*
TEST_OUTPUT:
---
fail_compilation/fail122.d(12): Error: undefined identifier `y`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=228
// Crash on inferring function literal return type after prior errors
void main()
{
    y = 2;
    auto x = function(){};
}
