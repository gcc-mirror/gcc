// https://issues.dlang.org/show_bug.cgi?id=8150: nothrow check doesn't work for constructor
/*
TEST_OUTPUT:
---
fail_compilation/bug8150a.d(14): Error: `object.Exception` is thrown but not caught
fail_compilation/bug8150a.d(12): Error: `nothrow` constructor `bug8150a.Foo.this` may throw
---
*/

struct Foo
{
    this(int) nothrow
    {
        throw new Exception("something");
    }
}

void main() {
    Foo(1);
}
