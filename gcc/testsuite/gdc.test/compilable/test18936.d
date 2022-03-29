// REQUIRED_ARGS: -fPIC -O -release -inline -m64 -betterC
// DISABLED: win32 win64

// https://issues.dlang.org/show_bug.cgi?id=18936
// produces assert failure cgxmm.c line 684

import core.stdc.math;

struct S
{
    double re, im;


    static S sqrtcx(S* z)
    {
        S c;
        real x,y,w,r;

        {
            x = fabs(z.re);
            y = fabs(z.im);
            if (z.re >= 0)
            {
                c.im = (z.im >= 0) ? w : -w;
                c.re = z.im / (c.im + c.im);
            }
        }
        return c;
    }
}
