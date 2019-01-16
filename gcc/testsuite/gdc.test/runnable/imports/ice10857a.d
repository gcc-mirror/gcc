module imports.ice10857a;

template filter(alias pred)
{
    auto filter(Range)(Range rs)
    {
        return FilterResult!(pred, Range)(rs);
    }
}

private struct FilterResult(alias pred, R)
{
    R _input;

    void popFront()
    {
        assert(pred(_input[0]) == 123);
    }
}
