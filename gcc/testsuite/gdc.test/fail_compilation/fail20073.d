// https://issues.dlang.org/show_bug.cgi?id=20073
/*
TEST_OUTPUT:
---
fail_compilation/fail20073.d(20): Error: cannot implicitly convert expression `s` of type `S` to `string`
fail_compilation/fail20073.d(21): Error: cannot implicitly convert expression `s` of type `S` to `string`
---
*/

struct S
{
    char[10] x;
    auto slice() inout { return x[0 .. 10]; }
    alias slice this;
}


string test() {
    S s;
    string str = s; // cannot implicitly convert expression `s` of type `S` to `string`
    return s;       // and suddenly we can!
}
