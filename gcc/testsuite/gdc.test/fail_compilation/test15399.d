/* https://issues.dlang.org/show_bug.cgi?id=15399
TEST_OUTPUT:
---
fail_compilation/test15399.d(32): Error: field `S1.ptr` cannot modify misaligned pointers in `@safe` code
fail_compilation/test15399.d(33): Error: field `S2.ptr` cannot modify misaligned pointers in `@safe` code
fail_compilation/test15399.d(34): Error: field `S1.ptr` cannot modify misaligned pointers in `@safe` code
fail_compilation/test15399.d(35): Error: field `S2.ptr` cannot modify misaligned pointers in `@safe` code
fail_compilation/test15399.d(36): Error: field `S1.ptr` cannot modify misaligned pointers in `@safe` code
fail_compilation/test15399.d(37): Error: field `S2.ptr` cannot modify misaligned pointers in `@safe` code
fail_compilation/test15399.d(38): Error: field `S1.ptr` cannot modify misaligned pointers in `@safe` code
fail_compilation/test15399.d(39): Error: field `S2.ptr` cannot modify misaligned pointers in `@safe` code
---
*/

struct S1
{
        char c;
    align (1)
        int* ptr;
}

align (1)
struct S2
{
    int* ptr;
}

@safe void test(S1* s1, S2* s2)
{
    int* p = s1.ptr;
    p = s2.ptr;
    s1.ptr = null;
    s2.ptr = null;
    int** pp = &s1.ptr;
    pp = &s2.ptr;
    bar(s1.ptr);
    bar(s2.ptr);
    sinister(s1.ptr);
    sinister(s2.ptr);
    cbar(s1.ptr);
    cbar(s2.ptr);
}

@safe void bar(ref int*);
@safe void cbar(ref const int*);
@safe void sinister(out int*);
