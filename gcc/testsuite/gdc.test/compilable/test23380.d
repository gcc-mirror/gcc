// REQUIRED_ARGS: -preview=dip1000

// https://issues.dlang.org/show_bug.cgi?id=23380
// Issue 23380 - [dip1000] class parameter should not be treated as ref qua lifetime

@safe void test(scope Object o0, scope Object o1)
{
    o1 = o0;
}
