// https://issues.dlang.org/show_bug.cgi?id=20401

void main()
{
    int i;
    assert(&passthrough(i) == &i);
}

ref int passthrough(return ref int i)
{
    return get().flag ? i : i;
}

S get() { return S(); }

struct S
{
    bool flag;
    ~this(){}
}
