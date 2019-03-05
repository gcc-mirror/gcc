module imports.ice4481a;

template reduce(alias pred)
{
    auto reduce(R)(R range)
    {
        return pred(range[0]);
    }
}
