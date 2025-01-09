/*
TEST_OUTPUT:
---
fail_compilation/test11176.d(12): Error: using `b.ptr` (instead of `&b[0])` is not allowed in a `@safe` function
fail_compilation/test11176.d(16): Error: using `b.ptr` (instead of `&b[0])` is not allowed in a `@safe` function
---
*/

// https://issues.dlang.org/show_bug.cgi?id=11176

@safe ubyte oops(ubyte[] b) {
    return *b.ptr;
}

@safe ubyte oops(ubyte[0] b) {
    return *b.ptr;
}

@safe ubyte cool(ubyte[1] b) {
    return *b.ptr;
}
