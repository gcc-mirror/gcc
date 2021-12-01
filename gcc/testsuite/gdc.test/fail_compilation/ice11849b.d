/*
TEST_OUTPUT:
---
fail_compilation/ice11849b.d(11): Error: circular reference to enum base type `DWORD1`
fail_compilation/ice11849b.d(11): Error: `DWORD1` is used as a type
fail_compilation/ice11849b.d(16): Error: circular reference to enum base type `typeof(DWORD2)`
---
*/
enum REG_DWORD = 1;

enum : DWORD1
{
    DWORD1 = REG_DWORD
}

enum : typeof(DWORD2)
{
    DWORD2 = REG_DWORD
}
