// PERMUTE_ARGS: -preview=dip1000

// https://issues.dlang.org/show_bug.cgi?id=20596

struct S(T)
{
    void delegate() dg;

    this(scope void delegate() dg)
    {
        this.dg = dg;
    }
}

@nogc void fooTemplate()
{
    int num;

    void foo() { int dummy = num; }

    scope s = S!int(&foo);
}

void test3032() @nogc
{
    int n = 1;
    scope fp = (){ n = 10; };       // no closure
    fp();
}
