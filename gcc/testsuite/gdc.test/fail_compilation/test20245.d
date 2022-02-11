/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test20245.d(14): Error: scope variable `a` may not be returned
fail_compilation/test20245.d(18): Error: cannot take address of `scope` parameter `x` in `@safe` function `foo`
fail_compilation/test20245.d(33): Error: reference to local variable `price` assigned to non-scope `this.minPrice`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=20245
@safe int* foo(ref int x) {
    int* a = &x;
    return a;
}

@safe int** foo(ref scope int* x) {
    int** a = &x;
    return a;
}

@safe int* foo(return ref int x) {
    int* a = &x;
    return a;
}

// https://issues.dlang.org/show_bug.cgi?id=21212
class MinPointerRecorder
{
    int* minPrice;
    void update(ref int price) @safe
    {
        minPrice = &price; // Should not compile.
    }
}

void main() @safe
{
    auto r = new MinPointerRecorder;
    () { int mp = 42; r.update(mp); } ();
    () { ulong[1000] stomp = 13; } ();
    auto x = *r.minPrice; // "13"
}
