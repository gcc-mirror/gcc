/*
REQUIRED_ARGS:-preview=dip1021
TEST_OUTPUT:
---
fail_compilation/failob1.d(104): Error: variable `failob1.test1.a1` is not disposed of before return
fail_compilation/failob1.d(105): Error: variable `failob1.test2.a2` is not disposed of before return
fail_compilation/failob1.d(107): Error: variable `failob1.test4.s4` is not disposed of before return
fail_compilation/failob1.d(108): Error: variable `failob1.test5.dg5` is not disposed of before return
fail_compilation/failob1.d(115): Error: variable `failob1.test12.p12` is not disposed of before return
---
*/

struct S { int i; int* f; }
struct T { int i; const(int)* f; }
class C { int i; int* f; }

#line 100

@live
{
    // Test what is and is not a trackable variable
    void test1(int[] a1) { }            // error
    void test2(int*[3] a2) { }          // error
    void test3(const int*[3] a) { }     // ok
    void test4(S s4) { }                // error
    void test5(int delegate() dg5) { }  // error
    void test6(const(int*)[3] a) { }    // ok
    void test7(const(int)*[3] a) { }    // ok
    void test8(const(int)* p) { }       // ok
    void test9(T t) { }                 // ok
    void test10(C c) { }                // ok
    void test11(int i) { }              // ok
    void test12(int* p12) { }           // error
}
