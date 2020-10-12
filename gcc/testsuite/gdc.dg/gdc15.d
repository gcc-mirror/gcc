// https://bugzilla.gdcproject.org/show_bug.cgi?id=15
// { dg-do compile }

template map(fun...)
{
    auto map(Range)(Range r)
    {
        return MapResult!(fun, Range)(r);
    }
}

private struct MapResult(alias fun, Range)
{
    Range _input;

    this(Range input)
    {
        _input = input;
    }
}

class B
{
    class A { }
    A a;
}

class C
{
    void visit(B b)
    {
        auto as = [b.a];
        map!((d) { return d; })(as);
    }
}
