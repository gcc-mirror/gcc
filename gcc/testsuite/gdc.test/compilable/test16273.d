// https://issues.dlang.org/show_bug.cgi?id=16273

class A()
{
    alias MyD = D!();
}

class B
{
    void f() {}
    alias MyA = A!();
}

class C : B
{
    override void f() {}
}

class D() : A!()
{
    void g() { new C; }
}
