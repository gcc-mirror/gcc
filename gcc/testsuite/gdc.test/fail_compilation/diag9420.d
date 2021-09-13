/*
TEST_OUTPUT
---
fail_compilation/diag9420.d(21): Error: function `diag9420.S.t3!().tx()` is not callable using argument types `(int)`
fail_compilation/diag9420.d(21):        expected 0 argument(s), not 1
---
*/

mixin template Mixin() { }
struct S
{
    template t3(T...)
    {
        void tx(T){}
        alias t3 = tx;
    }
}
void main()
{
    S s1;
    s1.t3!()(1);
}
