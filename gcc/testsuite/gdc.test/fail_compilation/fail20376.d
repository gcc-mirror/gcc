// https://issues.dlang.org/show_bug.cgi?id=20376

/*
TEST_OUTPUT:
---
fail_compilation/fail20376.d(17): Error: return value `Foo()` of type `Foo` does not match return type `ubyte`, and cannot be implicitly converted
---
*/

struct Foo
{
    this(ref scope Foo);
}

ubyte fun()
{
    return Foo();
}

void main(){}
