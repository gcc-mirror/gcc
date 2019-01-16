module imports.ice15200b;

template filter(alias pred)
{
    auto filter(R)(R range)
    {
        return FilterResult!(pred, R)(range);
    }
}

struct FilterResult(alias pred, R)
{
    R _input;

    this(R r)
    {
        _input = r;
        while (_input.length && !pred(_input[0]))
        {
            _input = _input[1..$];
        }
    }

    @property bool empty()
    {
        return _input.length == 0;
    }

    @property auto ref front()
    {
        return _input[0];
    }

    void popFront()
    {
        do
        {
            _input = _input[1..$];
        } while (_input.length && !pred(_input[0]));
    }
}
