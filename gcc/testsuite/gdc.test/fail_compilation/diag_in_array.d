/**
TEST_OUTPUT:
---
fail_compilation/diag_in_array.d(17): Error: incompatible types for `(3) in (a)`: `int` and `int[4]`
fail_compilation/diag_in_array.d(17):        `in` is only allowed on associative arrays
fail_compilation/diag_in_array.d(17):        perhaps use `std.algorithm.find(3, a[])` instead
fail_compilation/diag_in_array.d(19): Error: incompatible types for `("s") in (b)`: `string` and `string[]`
fail_compilation/diag_in_array.d(19):        `in` is only allowed on associative arrays
fail_compilation/diag_in_array.d(19):        perhaps use `std.algorithm.find("s", b)` instead
---
*/

void main()
{
    int[4] a;
    string[] b;
    if (3 in a)
        return;
    auto c = "s" in b;
}
