/*
TEST_OUTPUT:
---
fail_compilation/diag13884.d(14): Error: functions cannot return a sequence (use `std.typecons.Tuple`)
fail_compilation/diag13884.d(21):        instantiated from here: `MapResult!((t) => t.tupleof, Foo[])`
fail_compilation/diag13884.d(14):        instantiated from here: `map!(Foo[])`
---
*/

struct Foo { int x; }

void main()
{
    [Foo(1)].map!(t => t.tupleof);
}

template map(fun...)
{
    auto map(Range)(Range r)
    {
        return MapResult!(fun, Range)(r);
    }
}

struct MapResult(alias fun, R)
{
    R _input;

    @property auto ref front()
    {
        return fun(_input[0]);
    }

}
