/* TEST_OUTPUT:
---
fail_compilation/test18607.d(10): Error: function `test18607.test!int.test` no `return exp;` or `assert(0);` at end of function
& test
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18607

int* test(T...)() pure @safe {
        L:foreach(_; T) {
                continue L;
                return null;
        }
}


pragma(msg, &test!(int));
