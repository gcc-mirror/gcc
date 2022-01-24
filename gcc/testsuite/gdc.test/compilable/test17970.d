shared struct Shared
{
    static Shared make()
    {
        return Shared();
    }

    ~this()
    {
    }
}

shared struct Foo
{
    ~this()
    {
    }
}

struct Inner { ~this() {} }
struct Outer { shared(Inner) inner; }

void main()
{
    Foo x = Foo();
    auto s = Shared.make();
    Outer _;
}
