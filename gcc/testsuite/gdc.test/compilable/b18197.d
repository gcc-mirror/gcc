// REQUIRED_ARGS: -c -m32 -O -inline

struct A
{
    double a;
}

A makeA(double value)
{
    return A(value);
}

double test(double x)
{
    ulong p = *cast(ulong *)&x;
    return makeA(x).a;
}
