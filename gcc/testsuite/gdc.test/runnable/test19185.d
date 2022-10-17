// https://issues.dlang.org/show_bug.cgi?id=19185

int fun()
{
    int x = 2;
    struct A
    {
        int a;
        this(int a)
        {
            this.a = a + x;      // segault here
        }
    }

    A a = 5;
    return a.a;
}

void main()
{
    assert(fun() == 7);
}
