// https://bugzilla.gdcproject.org/show_bug.cgi?id=218
// { dg-do compile }

struct S218a
{
    this(int* pdata_)
    {
        pdata = pdata_;
    }

    void opIndexAssign(int, size_t) { }
    int* pdata;
};

struct S218
{
    S218a getS218a()
    {
        return S218a(data.ptr);
    }

    int[] data;
    int[] tab2;
};

S218 f()
{
    S218 r;

    for(int i = 0; i < 1; ++i)
        r.getS218a()[0] = 0;

    return r;
}

S218 var;

static this()
{
    var = f();
}
