// std.array
@property bool empty(T)(in T[] a) { return !a.length; }
@property ref T front(T)(T[] a) { return a[0]; }
void popFront(T)(ref T[] a) { a = a[1 .. $]; }

// std.typecons
struct Tuple(T...)
{
    T field;
    alias field this;
}
Tuple!T tuple(T...)(T args) { return typeof(return)(args); }

// std.range
template ElementType(R)
{
    static if (is(typeof(R.init.front.init) T))
        alias T ElementType;
    else
        alias void ElementType;
}

struct Repeat(T)
{
    private T _value;

    enum bool empty = false;
    @property inout(T) front() inout { return _value; }
    void popFront() {}
}
Repeat!T repeat(T)(T value) { return Repeat!T(value); }

struct Zip(R...)
{
    //alias Tuple!(staticMap!(.ElementType, R)) ElementType;
    static if (R.length == 3)
        alias Tuple!(int, int, int) ElementType;
    static if (R.length == 2)
        alias Tuple!(int, int) ElementType;

    R ranges;

    this(R rs)
    {
        foreach (i, Unused; R)
        {
            ranges[i] = rs[i];
        }
    }

    @property bool empty()
    {
        foreach (i, Unused; R)
        {
            if (ranges[i].empty)
                return true;
        }
        return false;
    }
    @property ElementType front()
    {
        ElementType result;
        return result;
    }
    void popFront()
    {
        foreach (i, Unused; R)
        {
            ranges[i].popFront();
        }
    }

    ElementType opIndex(size_t n)
    {
        ElementType result;
        return result;
    }
}
auto zip(Rs...)(Rs ranges) { return Zip!Rs(ranges); }

// std.algorithm
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

    this(R input)
    {
        _input = input;
    }

    @property bool empty() { return _input.empty; }
    @property auto ref front() { return fun(_input.front); }
    void popFront() { _input.popFront(); }
}

auto cartesianProduct(R1, R2)(R1 range1, R2 range2)
{
    return range2.map!((ElementType!R2 a) => zip(range1, repeat(a)));
}
auto cartesianProduct(R1, R2, RR...)(R1 range1, R2 range2, RR otherRanges)
{
    return map!(a => tuple(a[0], a[1][0], a[1][1]))(
        cartesianProduct(range1, cartesianProduct(range2, otherRanges))
    );
}

// test
void main()
{
    foreach (i, j, k; cartesianProduct([1], [1], [1])) {}
}
