// https://issues.dlang.org/show_bug.cgi?id=2195
// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail2195.d(16): Deprecation: variable `variable` is shadowing variable `fail2195.main.variable`. Rename the `foreach` variable.
---
*/

void main()
{
    int[int] arr;
    int variable;
    foreach (i, j; arr)
    {
        int variable;  // shadowing is disallowed but not detected
    }
}
