mixin template T()
{
    void f() { }
}

class A {
    mixin T;
    mixin T;
}

class B : A
{
    override void f() { }
    // raises "cannot determine overridden function" error.
}

void main(){}
