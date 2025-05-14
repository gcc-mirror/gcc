/*
TEST_OUTPUT:
---
fail_compilation/fail13424.d(12): Error: delegate `(dchar) { }` cannot be struct members
fail_compilation/fail13424.d(17): Error: delegate `(dchar) { }` cannot be union members
fail_compilation/fail13424.d(22): Error: delegate `(dchar) { }` cannot be class members
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
