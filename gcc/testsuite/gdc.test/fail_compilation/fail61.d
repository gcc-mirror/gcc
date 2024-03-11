/*
TEST_OUTPUT:
---
fail_compilation/fail61.d(25): Error: no property `B` for type `fail61.A.B`
fail_compilation/fail61.d(16):        class `B` defined here
fail_compilation/fail61.d(26): Error: no property `B` for type `fail61.A.B`
fail_compilation/fail61.d(16):        class `B` defined here
fail_compilation/fail61.d(35): Error: no property `A2` for type `fail61.B2`
fail_compilation/fail61.d(30):        class `B2` defined here
fail_compilation/fail61.d(44): Error: calling non-static function `foo` requires an instance of type `B3`
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
