// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

__gshared int testsroa_x;

template SROA(T1, T2)
{
    struct FPoint
    {
        T1 x;
        T2 y;
    }

    void sroa(FPoint p1, ref FPoint quad)
    {
        quad = FPoint(p1.x, p1.y);
    }

    void testit()
    {
        FPoint p1 = FPoint(1, 2);

        FPoint quad;
        sroa(p1, quad);

        if (quad != p1)
        {
            assert(0);
        }
        ++testsroa_x;
    }
}

void main()
{
    SROA!(int,   int  ).testit();
    SROA!(int,   float).testit();
    SROA!(float, float).testit();
    SROA!(float, int  ).testit();

    SROA!(long,   long  ).testit();
    SROA!(long,   double).testit();
    SROA!(double, double).testit();
    SROA!(double, long  ).testit();
}
