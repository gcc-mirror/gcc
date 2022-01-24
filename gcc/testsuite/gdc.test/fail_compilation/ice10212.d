/*
TEST_OUTPUT:
---
fail_compilation/ice10212.d(13): Error: expected return type of `int`, not `int function() pure nothrow @nogc @safe`:
fail_compilation/ice10212.d(13):        Return type of `int` inferred here.
---
*/

int delegate() foo()
{
    // returns "int function() pure nothrow @safe function() pure nothrow @safe"
    // and it mismatches to "int delegate()"
    return () => () {
        return 1;
    };
}
