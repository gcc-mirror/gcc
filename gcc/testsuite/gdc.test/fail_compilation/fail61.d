/*
TEST_OUTPUT:
---
fail_compilation/fail61.d(22): Error: no property `B` for type `fail61.A.B`
fail_compilation/fail61.d(23): Error: no property `B` for type `fail61.A.B`
fail_compilation/fail61.d(32): Error: no property `A2` for type `fail61.B2`
fail_compilation/fail61.d(41): Error: need `this` for `foo` of type `void()`
---
*/

class A
{
    class B : A
    {
        static const int C = 5;
    }
}

void main()
{
    int n1 = A.B.C;
    int n2 = A.B.B.C;       // Line22
    int n3 = A.B.B.B.C;     // Line23
}

class A2 { void foo(){ assert(0);} }
class B2 : A2 { override void foo(){} }
class C2 : B2
{
    void bar()
    {
        B2.A2.foo();        // Line32
    }
}

class B3 { void foo(){ assert(0); } }
class C3
{
    void bar()
    {
        B3.foo();           // Line41
    }
}
