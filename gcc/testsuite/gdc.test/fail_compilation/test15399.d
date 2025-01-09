/* https://issues.dlang.org/show_bug.cgi?id=15399
TEST_OUTPUT:
---
fail_compilation/test15399.d(32): Error: modifying misaligned pointers through field `S1.ptr` is not allowed in a `@safe` function
fail_compilation/test15399.d(33): Error: modifying misaligned pointers through field `S2.ptr` is not allowed in a `@safe` function
fail_compilation/test15399.d(34): Error: modifying misaligned pointers through field `S1.ptr` is not allowed in a `@safe` function
fail_compilation/test15399.d(35): Error: modifying misaligned pointers through field `S2.ptr` is not allowed in a `@safe` function
fail_compilation/test15399.d(36): Error: modifying misaligned pointers through field `S1.ptr` is not allowed in a `@safe` function
fail_compilation/test15399.d(37): Error: modifying misaligned pointers through field `S2.ptr` is not allowed in a `@safe` function
fail_compilation/test15399.d(38): Error: modifying misaligned pointers through field `S1.ptr` is not allowed in a `@safe` function
fail_compilation/test15399.d(39): Error: modifying misaligned pointers through field `S2.ptr` is not allowed in a `@safe` function
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
