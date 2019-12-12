
struct Foo
{
    enum __vector(long[2]) y = 1;
}

struct Bar
{
    __vector(long[2]) x;

    bool spam() const
    {
        return x == Foo.y;
    }
}

