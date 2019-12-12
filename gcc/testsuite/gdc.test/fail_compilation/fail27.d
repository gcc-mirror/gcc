/*
TEST_OUTPUT:
---
fail_compilation/fail27.d(15): Error: cannot implicitly convert expression `-32769` of type `int` to `short`
fail_compilation/fail27.d(16): Error: cannot implicitly convert expression `-129` of type `int` to `byte`
fail_compilation/fail27.d(17): Error: cannot implicitly convert expression `-1` of type `int` to `char`
fail_compilation/fail27.d(18): Error: cannot implicitly convert expression `65536` of type `int` to `wchar`
fail_compilation/fail27.d(19): Error: cannot implicitly convert expression `-1` of type `int` to `wchar`
fail_compilation/fail27.d(21): Error: cannot implicitly convert expression `-1` of type `int` to `dchar`
---
*/

void main()
{
    short a = -32769; // short.min-1
    byte  b = -129; // byte.min-1
    char  c = -1; // char.min-1
    wchar D = 65536; // wchar.max+1
    wchar d = -1; // wchar.min-1
    dchar E = 1114111; // dchar.max+1
    dchar e = -1; // dchar.min-1
}
