struct Foo
{
    void bar()(typeof(cast()this) x)
    {
    }
}

void main()
{
    Foo x;
    x.bar(x);
}
