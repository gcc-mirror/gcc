/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail20084.d(109): Error: escaping a reference to parameter `v` by returning `v.front()` is not allowed in a `@safe` function
---
*/

#line 100

// https://issues.dlang.org/show_bug.cgi?id=20084

struct W() {
    int value;
    @safe ref int front() return { return value; }
}

@safe ref int get(W!() v) {
    return v.front;
}
