/**
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test24680.d(19): Error: escaping a reference to local variable `buf` by returning `c.peek(buf[])`  is not allowed in a `@safe` function
---
*/

// https://issues.dlang.org/show_bug.cgi?id=24680

class C
{
    final auto peek(ubyte[] buf) { return buf; }
}

@safe escape(C c)
{
    ubyte[5] buf;
    return c.peek(buf[]);
}
