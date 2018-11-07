/*
TEST_OUTPUT:
---
fail_compilation/fail36.d(13): Error: template t(int L) does not have property 'a'
fail_compilation/fail36.d(18): Error: mixin fail36.func.t!10 error instantiating
---
*/

template t(int L)
{
    int a;
    // void foo(int b = t!(L).a) {} // correct
    void foo(int b = t.a) {} // wrong
}

void func()
{
    mixin t!(10);
}
