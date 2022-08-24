// REQUIRED_ARGS: -unittest -main -O
// https://issues.dlang.org/show_bug.cgi?id=15568

auto filter(alias pred)(D[])
{
    struct FilterResult
    {
        void popFront()
        {
            pred(null);
        }

        D[] array()
        {
            return null;
        }
    }
    return FilterResult();
}

class A
{
    B foo(C c, D[] ds, bool f)
    in
    {
        assert(c !is null);
    }
    do
    {
        D[] ds2 = ds.filter!(a => c).array;

        return new B(ds2, f);
    }
}

class B
{
    this(D[], bool)
    {
    }
}

class C
{
}

struct D
{
}

unittest
{
    auto a = new A;
    C c = new C;

    a.foo(c, null, false);
}
