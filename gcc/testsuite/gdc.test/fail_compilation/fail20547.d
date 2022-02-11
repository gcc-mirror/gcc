/*
TEST_OUTPUT:
---
fail_compilation/fail20547.d(12): Error: cannot create a `string[string]` with `new`
fail_compilation/fail20547.d(14): Error: cannot create a `string[string]` with `new`
---
*/

void main()
{
    //https://issues.dlang.org/show_bug.cgi?id=11790
    string[string] crash = new string[string];
    //https://issues.dlang.org/show_bug.cgi?id=20547
    int[string] c = new typeof(crash);
}
