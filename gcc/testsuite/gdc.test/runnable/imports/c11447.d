module imports.c11447;
template map(fun...)
{
    auto map(Range)(Range r)
    {
        return MapResult!(fun, Range)(r);
    }
}
private struct MapResult(alias fun, R)
{
    R _input;
    this(R input) { _input = input; }
    @property bool empty() { return _input.length == 0; }
    @property auto ref front() { return fun(_input[0]); }
    void popFront() { _input = _input[1..$]; }
}

class A {}

struct TemplateInstancier
{
    auto instanciateFromResolvedArgs(A a)
    {
        auto bs = [B(a)];

        static A gladeulfeurah;
        gladeulfeurah = a;

        auto r = bs.map!(
            b => b.apply!(
                function string() { assert(0); },
                delegate string(i) {
                    assert(a is gladeulfeurah, "tagazok");
                    return "";
                }
            )
        );

        foreach (e; r) {}
    }
}

enum Tag { Undefined, A, }

struct B
{
    A a;
    Tag tag;

    this(A a)
    {
        tag = Tag.A;
        this.a = a;
    }
}

auto apply(alias undefinedHandler, alias handler)(B b)
{
    final switch(b.tag) with(Tag)
    {
        case Undefined :
            return undefinedHandler();

        case A :
            return handler(b.a);
    }
}
