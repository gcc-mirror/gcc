/*
TEST_OUTPUT:
---
fail_compilation/ice10212.d(12): Error: mismatched function return type inference of `int function() pure nothrow @nogc @safe` and `int`
---
*/

int delegate() foo()
{
    // returns "int function() pure nothrow @safe function() pure nothrow @safe"
    // and it mismatches to "int delegate()"
    return () => {
        return 1;
    };
}
