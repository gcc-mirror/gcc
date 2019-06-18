// https://issues.dlang.org/show_bug.cgi?id=5973

class A { int a = 1; }
class B { int b = 2; }
class C : A
{
    B obj;
    alias obj this;
    this(){ obj = new B(); }
}
class X : C {}

class D
{
    int i;
}

class E
{
    D x;
    alias x this;
}

class F : E
{
    void test()
    {
        i = 5;
    }
}

void main()
{
    auto c = new C();
    assert(c.a == 1);   // lookup C -> A, OK
    assert(c.b == 2);   // lookup C => B, OK

    auto x = new X();
    assert(x.a == 1);   // lookup X -> C -> A, OK
    assert(x.b == 2);   // lookup X -> C => B, NG (Line 17)
}
