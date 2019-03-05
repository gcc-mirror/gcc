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

struct Foo
{
    int baz(int v)
    {
        static int id;
        return v + id++;
    }
    void bar()
    {
        auto arr1 = [1, 2, 3];
        auto arr2 = [4, 5, 6];
        arr1.map!(
            // lambda1
            i =>
                arr2.map!(
                    // lambda2
                    j =>
                        baz(i + j)
                )
        );
    }
}

void main() {}
