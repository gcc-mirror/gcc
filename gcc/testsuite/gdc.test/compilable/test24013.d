// https://issues.dlang.org/show_bug.cgi?id=24013

struct PropDescriptor(T)
{
    void define(T delegate() aGetter, string aName) {}
}

enum Get;

mixin template PropertyPublisherImpl()
{
    void collectPublicationsFromPairs(T)()
    {
        foreach (member; __traits(derivedMembers, T))
        foreach (overload; __traits(getOverloads, T, member))
        {
            alias Attributes = __traits(getAttributes, overload);
            static if (Attributes.length != 0)
            {
                auto descriptor = new PropDescriptor!size_t;
                auto dg = &overload;
                descriptor.define(dg, member);
            }
        }
    }
}

void main()
{
    class Bar
    {
        size_t _field;
        mixin PropertyPublisherImpl;
        this()
        {
            collectPublicationsFromPairs!Bar;
        }
        @Get size_t field()
        {
            return _field;
        }
    }
}
