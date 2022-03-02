/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22709.d(15): Error: address of variable `local` assigned to `arr` with longer lifetime
fail_compilation/test22709.d(20): Error: address of variable `local` assigned to `arr` with longer lifetime
---
*/

// https://issues.dlang.org/show_bug.cgi?id=22709
@safe:

void escape(ref ubyte[] arr, ref ubyte[64] local)
{
    arr = local[];
}

void escape1(ref ubyte[64] local, ref ubyte[] arr)
{
    arr = local[];
}

ubyte[] getArr()
{
    ubyte[64] blob;
    ubyte[] arr;
    escape(arr, blob[]);
    return arr;
}
