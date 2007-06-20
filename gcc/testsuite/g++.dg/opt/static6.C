// PR c++/31806
// { dg-do run }
// { dg-options "-O2 -fno-inline -fno-threadsafe-statics" }

extern "C" void abort(void);

struct A
{
    void *d;
};

static const A& staticA()
{
    static A s_static;
    return s_static;
}

void assert_failed()
{
    abort();
}

A testMethod()
{
    static const A& s = staticA( );
    if (&s == 0)
        assert_failed();
    return s;
}

int main()
{
    testMethod();
    return 0;
}
