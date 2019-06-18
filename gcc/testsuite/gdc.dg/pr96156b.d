@safe unittest
{
    struct CustomString
    {
    @safe:
        string _impl;
        @property bool empty() const { return !_impl.length; }
    }

    CustomString find(CustomString a, CustomString b)
    {
        return CustomString.init;
    }

    auto r = find(CustomString("a"), CustomString("b"));
    assert(r.empty);
}
