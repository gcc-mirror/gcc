/*
EXTRA_FILES: imports/test64a.d
TEST_OUTPUT:
---
fail_compilation/imports/test64a.d(1): Error: module `imports` from file fail_compilation/imports/test64a.d conflicts with package name imports
---
*/


//import std.stdio;

import imports.test64a;

int main(string[] args)
{
    //writefln(file1);
    return 0;
}
