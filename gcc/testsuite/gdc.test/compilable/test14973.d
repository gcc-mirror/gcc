template map(fun...)
{
    auto map(R)(R r)
    {
        return MapResult!(fun, R)(r);
    }
}

struct MapResult(alias fun, R)
{
    R _input;

    @property bool empty() { return _input.length == 0; }
    @property auto front() { return fun(_input[0]); }
    void popFront() { _input = _input[1..$]; }
}

class Foo
{
    int baz() { return 1; }
    void bar()
    {
        auto s = [1].map!(i => baz()); // compiles
        auto r = [1].map!(  // returns MapResult-1
            // lambda1
            i =>
                [1].map!(   // returns MapResult-2
                    // lambda2
                    j =>
                        baz()
                )
        ); // compiles <- error

        // When lambda1 is called in MapResult-1.front(), it was changed to
        // TOKfunction in functionResolve. But in that time, MapResult-2 semantic3
        // was not yet finished, then the lambda2 call in MapResult-2.front()
        // could not access to enclosing scope frame to call baz().
        // To fix the issue, MapResult-2 semantic3 should be finished during the
        // lambda1 body analysis.
    }
}

class Bar
{
    int baz;
    void bar()
    {
        auto s = [1].map!(i => baz); // compiles
        auto r = [1].map!(
            // lambda1
            i =>
                [1].map!(
                    // lambda2
                    j =>
                        baz
                )
        ); // compiles <- error
    }
}

/*******************************************/

struct ChunkByImpl(alias eq)
{
    struct Group
    {
        int[] start;
        int[] current;

        void popFront()
        {
            // In here:
            //  SortedRange.pred == (a, b) => a  @ test14978b()
            //  ChunkByImpl.eq == (a, b) => pred(a, b)  @ SortedRange.groupBy()
            //
            // The context deduction should be:
            //  First pred is deduced to function pointer,
            //  and then, eq is also deduced to function pointer because pred is function pointer.
            //
            // Therefore, when ChunkByImpl is instantiated in groupBy(), its semantic3
            // needs to be invoked to analyze ???
            eq(start, current);
        }
    }
}

struct SortedRange(alias pred)
{
    int[] input;

    auto groupBy()
    {
        ChunkByImpl!(
            (a, b) => pred(a, b)
        ) r;
    }
}

void test14973b()
{
    SortedRange!(
        (a, b) => a
    ) r;
}
