/*
TEST_OUTPUT:
---
fail_compilation/fail98.d(20): Error: cannot implicitly convert expression `256` of type `int` to `E`
fail_compilation/fail98.d(21): Error: cannot automatically assign value to enum member `fail98.D3DTS_WORLD1` because base type `E` is an enum; provide an explicit value
fail_compilation/fail98.d(22): Error: cannot automatically assign value to enum member `fail98.D3DTS_WORLD2` because base type `E` is an enum; provide an explicit value
fail_compilation/fail98.d(23): Error: cannot automatically assign value to enum member `fail98.D3DTS_WORLD3` because base type `E` is an enum; provide an explicit value
---
*/

// https://issues.dlang.org/show_bug.cgi?id=139

E foo(int index)
{
    return index + 256;
}

enum : E
{
    D3DTS_WORLD = 256,
    D3DTS_WORLD1,
    D3DTS_WORLD2,
    D3DTS_WORLD3
}

enum E
{
    D3DTS_VIEW         =  2,
    D3DTS_PROJECTION,
    D3DTS_TEXTURE0     = 16,
    D3DTS_TEXTURE1,
    D3DTS_TEXTURE2,
    D3DTS_TEXTURE3,
    D3DTS_TEXTURE4,
    D3DTS_TEXTURE5,
    D3DTS_TEXTURE6,
    D3DTS_TEXTURE7, // = 23
    D3DTS_FORCE_DWORD  = 0xffffffff
}
