void test72()
{
    class A {}
    class B : A {}

    class X
    {
        abstract A func();
    }
    class Y : X
    {
        B func() { return new A(); }
    }
}

void main() {}
