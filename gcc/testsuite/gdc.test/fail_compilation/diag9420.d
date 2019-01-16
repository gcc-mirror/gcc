/*
TEST_OUTPUT
---
fail_compilation/diag9420.d(20): Error: function diag9420.S.t3!().tx () is not callable using argument types (int)
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
