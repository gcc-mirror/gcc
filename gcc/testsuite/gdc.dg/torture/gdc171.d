// https://bugzilla.gdcproject.org/show_bug.cgi?id=171
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void test171a()
{
    int count = 0;
    short a = -1;
    while (a != 0)
    {
        a >>>= 1;
        count++;
        assert(count <= 16);
    }
}

void test171b()
{
    uint[3] lhs = [99, 201, 300],
            rhs = [-1, 0, 0];
    long t = 0;

    for (int i = 0; i < 3; i++)
    {
        t += lhs[i];
        t -= rhs[i];
        lhs[i] = cast(uint) t;
        t >>= uint.sizeof * 8;
    }

    assert(lhs == [100, 200, 300]);
}

void main()
{
    test171a();
    test171b();
}
