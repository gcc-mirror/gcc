// https://issues.dlang.org/show_bug.cgi?id=17352
void bug(Args...)()
{
}

void test(bool coin)
{
    if (coin)
    {
        string foobar;
        bug!foobar();
    }
    else
    {
        string foobar;
        bug!foobar();
    }
}
