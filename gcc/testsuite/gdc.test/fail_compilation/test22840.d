/* REQUIRED_ARGS: -preview=dip1000
 * TEST_OUTPUT:
---
fail_compilation/test22840.d(25): Error: returning `sb.slice()` escapes a reference to local variable `sb`
---
*/

// inout method with inferred @safe escapes local data
// https://issues.dlang.org/show_bug.cgi?id=22840

// See also: https://issues.dlang.org/show_bug.cgi?id=20149

struct S
{
    int buf;
    auto slice() inout
    {
        return &buf;
    }
}

int* fun() @safe
{
    S sb;
    return sb.slice(); // should error
}
