// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

struct Take(R)
{
    public R source;
    private size_t _maxAvailable;

    alias R Source;

    @property bool empty()
    {
        return _maxAvailable == 0 || source.empty;
    }

    @property auto ref front()
    {
        return source.front;
    }

    void popFront()
    {
        source.popFront();
        --_maxAvailable;
    }

    @property size_t length() const
    {
        return _maxAvailable;
    }
}

struct Repeat(T)
{
    private T _value;

    enum bool empty = false;
    @property inout(T) front() inout { return _value; }
    void popFront() {}
}

Take!(Repeat!T) repeat(T)(T value, size_t n)
{
    return typeof(return)(Repeat!T(value), n);
}

auto array(Range)(Range r)
{
    alias E = typeof(r.front);
    //static if (hasLength!Range)
    {
        if (r.length == 0)
            return null;

        auto result = new E[](r.length);

        size_t i;
        static auto trustedGetAddr(T)(ref T t) @trusted nothrow pure
        {
            return &t;
        }
        foreach (e; r)
        {
            *trustedGetAddr(result[i]) = e;
            ++i;
        }
        return cast(E[])result;
    }
}

enum r = [1].repeat(1).array;
static assert(r == [[1]]);
