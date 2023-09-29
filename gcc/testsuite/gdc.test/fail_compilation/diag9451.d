/*
TEST_OUTPUT:
---
fail_compilation/diag9451.d(27): Error: cannot create instance of abstract class `C2`
fail_compilation/diag9451.d(21):        class `C2` is declared here
fail_compilation/diag9451.d(15):        function `void f1()` is not implemented
fail_compilation/diag9451.d(16):        function `void f2(int)` is not implemented
fail_compilation/diag9451.d(17):        function `void f2(float) const` is not implemented
fail_compilation/diag9451.d(18):        function `int f2(float) pure` is not implemented
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
