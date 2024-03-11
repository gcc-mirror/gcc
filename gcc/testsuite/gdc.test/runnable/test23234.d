// https://issues.dlang.org/show_bug.cgi?id=23234

class Bar
{
}

class Foo
{
    Bar get() { return new Bar; }
    alias get this;
}

void main()
{
    auto foo = new Foo;
    void test(Bar delegate() dg)
    {
        assert(dg() !is null);
    }

    test(() => foo);
}
