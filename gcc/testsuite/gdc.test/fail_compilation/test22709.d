/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22709.d(15): Error: assigning address of variable `local` to `arr` with longer lifetime is not allowed in a `@safe` function
fail_compilation/test22709.d(20): Error: assigning address of variable `local` to `arr` with longer lifetime is not allowed in a `@safe` function
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
