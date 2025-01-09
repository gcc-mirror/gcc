/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail22366.d(22): Error: copying scope variable `s` into allocated memory is not allowed in a `@safe` function
fail_compilation/fail22366.d(25): Error: copying scope variable `s` into allocated memory is not allowed in a `@safe` function
fail_compilation/fail22366.d(26): Error: copying scope variable `s` into allocated memory is not allowed in a `@safe` function
fail_compilation/fail22366.d(27): Error: copying scope variable `s` into allocated memory is not allowed in a `@safe` function
fail_compilation/fail22366.d(28): Error: copying scope variable `s` into allocated memory is not allowed in a `@safe` function
fail_compilation/fail22366.d(31): Error: copying scope variable `s` into allocated memory is not allowed in a `@safe` function
fail_compilation/fail22366.d(32): Error: copying scope variable `s` into allocated memory is not allowed in a `@safe` function
---
*/

// Test escaping scope variables through AA keys / values
// https://issues.dlang.org/show_bug.cgi?id=22366
// https://issues.dlang.org/show_bug.cgi?id=23531

void fun(scope string s) @safe
{
    int[string] aa;
    aa[s] ^^= 3;

    string[string] saa;
    saa[""] = s;
    saa[""] ~= s;
    saa[s] = "";
    saa[s] ~= "";

    string[string][string] snaa;
    snaa[s][""] = "";
    snaa[""][s] = "";
}
