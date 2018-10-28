template Foo(alias b)
{
    int abc() { return b; }
}

void main()
{
    int y = 8;
    mixin Foo!(y);
    mixin Foo!(y);
    assert(abc() == 8);
}

