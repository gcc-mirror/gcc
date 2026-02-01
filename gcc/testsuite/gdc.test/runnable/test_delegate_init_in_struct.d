struct S
{
    string[] delegate() dg;
}

S s = {
    dg: () => ["hello"] // SEGFAULT without explicit `delegate`
};

void main()
{
    auto result = s.dg();
    assert(result.length == 1);
    assert(result[0] == "hello");
}
