class K
{
    inout(int) f() inout
    {
        return var;
    }

    void bug()
    {
        auto d = &f;
        d();
    }

    int var;
}
