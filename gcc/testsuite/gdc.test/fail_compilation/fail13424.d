/*
TEST_OUTPUT:
---
fail_compilation/fail13424.d(12): Error: delegate fail13424.S.__lambda2 cannot be struct members
fail_compilation/fail13424.d(17): Error: delegate fail13424.U.__lambda2 cannot be union members
fail_compilation/fail13424.d(22): Error: delegate fail13424.C.__lambda2 cannot be class members
---
*/

struct S
{
    void delegate(dchar) onChar = (dchar) {};
}

union U
{
    void delegate(dchar) onChar = (dchar) {};
}

class C
{
    void delegate(dchar) onChar = (dchar) {};
}
