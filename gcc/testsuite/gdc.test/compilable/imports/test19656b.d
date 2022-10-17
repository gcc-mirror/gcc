import test19656;

class Bar { }

class Qux(T): Foo
{
    override void thunk() { }
}

class Fred
{
    Qux!Bar _q;
}
