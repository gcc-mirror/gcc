// https://issues.dlang.org/show_bug.cgi?id=8150: nothrow check doesn't work for constructor
/*
TEST_OUTPUT:
---
fail_compilation/bug8150b.d(15): Error: `object.Exception` is thrown but not caught
fail_compilation/bug8150b.d(13): Error: constructor `bug8150b.Foo.__ctor!().this` may throw but is marked as `nothrow`
fail_compilation/bug8150b.d(20): Error: template instance `bug8150b.Foo.__ctor!()` error instantiating
---
*/

struct Foo
{
    this()(int) nothrow
    {
        throw new Exception("something");
    }
}

void main() {
    Foo(1);
}
