//import std.array, std.range;
module imports.a11850;


template filter(alias pred)
{
    auto filter(Range)(Range rs)
    {
        return FilterResult!(pred, Range)(rs);
    }
}


private struct FilterResult(alias pred, Range)
{
    alias Range R;
    R _input;


    this(R r)
    {
        _input = r;
        while (_input.length != 0 && !pred(_input[0]))
        {
            _input = _input[1..$];
        }
    }


    auto opSlice() { return this; }


    @property bool empty() { return _input.length == 0; }


    void popFront()
    {
        do
        {
            _input = _input[1..$];
        } while (_input.length != 0 && !pred(_input[0]));
    }


    @property auto ref front()
    {
        return _input[0];
    }
}
