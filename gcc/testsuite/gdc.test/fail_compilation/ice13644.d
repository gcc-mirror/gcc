
struct Tuple(T...)
{
    T field;
    alias field this;
}

Tuple!(string, string)[] foo()
{
    Tuple!(string, string)[] res;
    return res;
}

void main()
{
    foreach (string k2, string v2; foo())
    {
    }
}
