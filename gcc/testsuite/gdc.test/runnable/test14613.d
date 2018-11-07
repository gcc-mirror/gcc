
// https://issues.dlang.org/show_bug.cgi?id=14613

T foo(T)(T b)
{
    return (b / (b == 0)) == 0;
}

void main()
{
    assert(foo(0.0f) == 1.0f);
    assert(foo(1.0f) == 0.0f);

    assert(foo(0.0) == 1.0);
    assert(foo(1.0) == 0.0);

    assert(foo(0.0L) == 1.0L);
    assert(foo(1.0L) == 0.0L);
}
