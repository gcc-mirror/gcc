// https://issues.dlang.org/show_bug.cgi?id=19735

extern int test1(int par)
{
    int var = 42;
    return var + par;
}

extern
{
    int test2(int par)
    {
        int var = 42;
        return var + par;
    }
}

void main()
{
    assert(test1(1) == test2(1));
}

