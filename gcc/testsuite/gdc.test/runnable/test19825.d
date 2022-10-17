struct JSONValue
{
    TaggedUnion payload;
}

struct TaggedUnion
{
    size_t[2] m_data;
    int m_kind;

    JSONValue opIndex(size_t i)
    {
        return JSONValue();
    }
}

void yap(lazy JSONValue arg)
{
    arg();
}

struct Foo
{
    int a;
    string name;
}

Foo makeFoo()
{
    JSONValue root;
    yap(root.payload[0]
            .payload[0]
            .payload[0]);

    Foo foo;
    return foo;
}

void main()
{
    auto foo = makeFoo();
}
