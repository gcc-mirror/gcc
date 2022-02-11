/* REQUIRED_ARGS: -preview=dip1000
 * TEST_OUTPUT:
---
fail_compilation/fail17927.d(13): Error: scope variable `this` may not be returned
fail_compilation/fail17927.d(21): Error: scope variable `ptr` may not be returned
fail_compilation/fail17927.d(23): Error: scope variable `ptr` may not be returned
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17927

struct String {
    const(char)* mem1() const scope @safe { return ptr; }

    inout(char)* mem2() inout scope @safe { return ptr; } // no error because `ref inout` implies `return`

    char* ptr;
}


const(char)* foo1(scope const(char)* ptr) @safe { return ptr; }

inout(char)* foo2(scope inout(char)* ptr) @safe { return ptr; }

