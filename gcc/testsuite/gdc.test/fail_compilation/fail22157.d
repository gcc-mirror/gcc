// https://issues.dlang.org/show_bug.cgi?id=22157

/*
TEST_OUTPUT:
---
fail_compilation/fail22157.d(32): Error: `fail22157.S!true.S.foo` called with argument types `()` matches multiple overloads exactly:
fail_compilation/fail22157.d(21):     `fail22157.S!true.S.foo()`
and:
fail_compilation/fail22157.d(22):     `fail22157.S!true.S.foo()`
fail_compilation/fail22157.d(33): Error: `fail22157.S!false.S.foo` called with argument types `()` matches multiple overloads exactly:
fail_compilation/fail22157.d(26):     `fail22157.S!false.S.foo()`
and:
fail_compilation/fail22157.d(27):     `fail22157.S!false.S.foo()`
---
*/

struct S(bool b)
{
    static if(b)
    {
        void foo() {}
        static void foo() {}
    }
    else
    {
        static void foo() {}
        void foo() {}
    }
}

void main() {
    S!true.foo;
    S!false.foo;
}
