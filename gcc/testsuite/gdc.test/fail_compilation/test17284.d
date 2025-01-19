/*
TEST_OUTPUT:
---
fail_compilation/test17284.d(17): Error: accessing overlapped field `U.c` with pointers is not allowed in a `@safe` function
pure nothrow @safe void(U t)
---
REQUIRED_ARGS: -preview=bitfields
*/

// https://issues.dlang.org/show_bug.cgi?id=17284

class C { }
union U { C c; int i; }

@safe void func(T)(T t)
{
        t.c = new C;
}

pragma(msg, typeof(func!U));
