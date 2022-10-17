/*
TEST_OUTPUT:
---
fail_compilation/diag9451.d(26): Error: cannot create instance of abstract class `C2`
fail_compilation/diag9451.d(26):        function `void f1()` is not implemented
fail_compilation/diag9451.d(26):        function `void f2(int)` is not implemented
fail_compilation/diag9451.d(26):        function `void f2(float) const` is not implemented
fail_compilation/diag9451.d(26):        function `int f2(float) pure` is not implemented
---
*/

class C1
{
    abstract void f1();
    abstract void f2(int);
    abstract void f2(float) const;
    abstract int f2(float) pure;
}

class C2 : C1
{
}

void main()
{
    auto c2 = new C2;
}
