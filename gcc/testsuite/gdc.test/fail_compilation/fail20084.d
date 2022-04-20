/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail20084.d(109): Error: returning `v.front()` escapes a reference to parameter `v`
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
