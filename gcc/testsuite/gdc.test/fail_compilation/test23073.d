/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test23073.d(28): Error: assigning scope variable `c` to non-scope parameter `c` calling `assignNext` is not allowed in a `@safe` function
fail_compilation/test23073.d(22):        which is not `scope` because of `c.next = c`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23073
// scope inference from pure doesn't consider self-assignment

@safe:

class C
{
    C next;
}

void assignNext(C c) pure nothrow @nogc
{
    c.next = c;
}

C escape() @nogc
{
    scope C c = new C();
    assignNext(c);
    return c.next;
}

void main()
{
    C dangling = escape();
}
