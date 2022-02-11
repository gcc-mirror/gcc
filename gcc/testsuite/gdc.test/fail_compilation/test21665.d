/* TEST_OUTPUT:
---
fail_compilation/test21665.d(18): Error: variable `test21665.test1.s` `void` initializers for structs with invariants are not allowed in safe functions
fail_compilation/test21665.d(30): Error: field `U.s` cannot access structs with invariants in `@safe` code that overlap other fields
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
