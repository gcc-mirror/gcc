/*
TEST_OUTPUT:
---
fail_compilation/diag20059.d(15): Error: expected return type of `string`, not `string[]`:
fail_compilation/diag20059.d(13):        Return type of `string` inferred here.
---
*/

auto fail()
{
    string ret;
    if (true)
        return ret;
    else
        return [ret];
}
