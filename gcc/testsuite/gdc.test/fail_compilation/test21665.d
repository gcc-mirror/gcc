/* TEST_OUTPUT:
---
fail_compilation/test21665.d(18): Error: `void` initializing a struct with an invariant is not allowed in a `@safe` function
fail_compilation/test21665.d(30): Error: accessing overlapped field `U.s` with a structs invariant is not allowed in a `@safe` function
---
*/

// https://issues.dlang.org/show_bug.cgi?id=21665

struct ShortString {
    private ubyte length;
    private char[15] data;

    invariant { assert(length <= data.length); }
}

@safe void test1() {
    ShortString s = void;
}

union U
{
    int n;
    ShortString s;
}

@safe void test2()
{
    U u;
    u.s.length = 3;
}
