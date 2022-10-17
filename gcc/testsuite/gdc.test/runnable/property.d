

/******************************************/

struct Foo
{
    int v;

    int bar(int value) { return v = value + 2; }
    int bar() { return 73; }
}

int test1()
{
    Foo f;
    int i;

    i = (f.bar = 6);
    assert(i == 8);
    assert(f.v == 8);

    i = f.bar;
    assert(i == 73);

    return 0;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=6259

struct S6259
{
    private int m_prop;
    ref const(int) prop() return { return m_prop; }
    void prop(int v) { m_prop = v; }
}

void test6259()
{
    S6259 s;
    s.prop = 1;
}

/******************************************/

void main()
{
    test1();
    test6259();
}
