// { dg-do compile }
struct UDA
{
    int delegate() foo;
}

static UDA getUDA(alias S)()
{
    return __traits(getAttributes, S)[0];
}

struct S124026
{
    @UDA({ return 42; }) int a;
}

void f124026()
{
    S124026 m;
    enum uda = getUDA!(m.a);
}
