/*
TEST_OUTPUT:
---
fail_compilation/ctfe14207.d(13): Error: cannot convert `&immutable(ulong)` to `ubyte[8]*` at compile time
fail_compilation/ctfe14207.d(18):        called from here: `nativeToBigEndian()`
fail_compilation/ctfe14207.d(22):        called from here: `digest()`
---
*/

ubyte[8] nativeToBigEndian()
{
    immutable ulong res = 1;
    return *cast(ubyte[8]*) &res;
}

auto digest()
{
    ubyte[8] bits = nativeToBigEndian();
    return bits;
}

enum h = digest();
