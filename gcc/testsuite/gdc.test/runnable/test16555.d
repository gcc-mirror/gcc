// https://issues.dlang.org/show_bug.cgi?id=16555

void outer(
    double x,
    double a, double b, double c, double d,
    double e, double f, double g, double h)
{
    assert(x == 999.0 && a == 1 && b == 2 && c == 3 && d == 4
        && e == 5 && f == 6 && g == 7 && h == 8);
}

void main()
{
    void inner(double x)
    {
        outer(x, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0);
    }

    inner(999.0);
}
