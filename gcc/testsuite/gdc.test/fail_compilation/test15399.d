/* https://issues.dlang.org/show_bug.cgi?id=15399
---
fail_compilation/test15399.d(31): Error: writing to misaligned pointer in field S1.ptr is not @safe
fail_compilation/test15399.d(32): Error: writing to misaligned pointer in field S2.ptr is not @safe
fail_compilation/test15399.d(33): Error: taking address of misaligned pointer in field S1.ptr is not @safe
fail_compilation/test15399.d(34): Error: taking address of misaligned pointer in field S2.ptr is not @safe
fail_compilation/test15399.d(35): Error: 'ref' of misaligned pointer in field S1.ptr is not @safe
fail_compilation/test15399.d(36): Error: 'ref' of misaligned pointer in field S2.ptr is not @safe
fail_compilation/test15399.d(37): Error: 'out' of misaligned pointer in field S1.ptr is not @safe
fail_compilation/test15399.d(38): Error: 'out' of misaligned pointer in field S2.ptr is not @safe
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

