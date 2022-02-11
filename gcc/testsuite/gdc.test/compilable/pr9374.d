// REQUIRED_ARGS: -preview=dip1000

// https://github.com/dlang/dmd/pull/9374

struct OnlyResult
{
    this(return scope ref int v2);

    void* data;
}

OnlyResult foo(return scope ref int v2);

OnlyResult only(int y)
{
    if (y)
        return OnlyResult(y);
    return foo(y);
}
