/*
REQUIRED_ARGS: -preview=dip1021

https://issues.dlang.org/show_bug.cgi?id=20995
*/

void foo() @live
{
    throw new Exception("");
}

void main () {}
