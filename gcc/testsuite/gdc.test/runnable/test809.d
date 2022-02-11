// https://issues.dlang.org/show_bug.cgi?id=809
void test(lazy int dg)
{
    int delegate() dg_ = &dg;
    assert(dg_() == 7);
    assert(dg == dg_());
}

void main()
{
    int a = 7;
    test(a);
}
