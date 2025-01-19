/*
PERMUTE_ARGS: -O
TRANSFORM_OUTPUT: remove_lines("warning: sprintf\(\) is often misused")
*/

/*
 WHETSTONE BENCHMARK PROGRAM

 This program uses a carefully chosen mix of instructions typical of
 scientific (floating point) calculations.

 See H.J. Curnow and B.A. Wichmann,
 "A Synthetic Benchmark", Computer J., V19 #1, Feb. 1976, pp. 43-49.

 Table of times for various computers in <info-ibmpc>whetst.answers
 compiled by Richard Gillmann (GILLMANN@ISIB)

Whetstone Fortran Benchmark
(I=10, optimization off, CPU seconds)

DEC       1.1 sec   DECsystem 2060 (TOPS-20 v4, F66)
PR1ME     1.4 sec   PR1ME 750 (PRIMOS v18.1, F66)
PR1ME     1.5 sec   PR1ME 750 (PRIMOS v18.1, F77)
DEC       2.1 sec   VAX 11/780 (Unix, F77)
Apollo    6.2 sec   10 MHz MC68000 w/hardware float. point (AEGIS v4.0, F77)
Apollo   13.1 sec   10 MHz MC68000 w/software float. point (AEGIS v4.0, F77)
Intel    16.0 sec   8086/8087 (286WD Micro Development System,Intel FORTRAN)
IBM      16.0 sec   4.77 MHz 8088 PC w/8087 (DOS 2, Microsoft F77/3.10)
Z80     124.0 sec   4 MHz Z80 with Microsoft Fortran, CP/M
IBM     268.9 sec   4.77 MHz 8088 PC ($NODEBUG) (DOS 1, Microsoft F77/1.0)
Intel   390.0 sec   8086 alone (286WD Micro Development System,Intel FORTRAN)

Table compiled by Richard Gillmann (Gillmann@ISIB).
*/

import core.stdc.stdio;
import core.stdc.time;
import core.stdc.math;

double t, t1, t2;
double[5] e1;
int j, k, l;

int main()
{
    clock_t start, stop;
    double x1, x2, x3, x4, x, y, z;
    int i, isave, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12;

    start = clock();

/* I=10 CORRESPONDS TO ONE MILLION WHETSTONE INSTRUCTIONS */

    i = 10;
    t1 = 0.50025000;
    t = 0.499975000;
    t2 = 2.0000;
    isave = i;
    n1 = 0;
    n2 = 12 * i;
    n3 = 14 * i;
    n4 = 348 * i;
    n5 = 0;
    n6 = 210 * i;
    n7 = 32 * i;
    n8 = 899 * i;
    n9 = 516 * i;
    n10 = 0;
    n11 = 93 * i;
    n12 = 0;
    x1 = 1.0;
    x2 = -1.0;
    x3 = -1.0;
    x4 = -1.0;
    for (i = 1; i <= n1; i++)
    {
        x1 = (x1+x2+x3-x4)*t;
        x2 = (x1+x2-x3+x4)*t;
        x3 = (x1-x2+x3+x4)*t;
        x4 = (-x1+x2+x3+x4)*t;
    }
    auto s = pout(n1,n1,n1,x1,x2,x3,x4);
    assert(s[] == "       0       0       0   1.0000e+00  -1.0000e+00  -1.0000e+00  -1.0000e+00");

    e1[1] = 1.0;
    e1[2] = -1.0;
    e1[3] = -1.0;
    e1[4] = -1.0;

    for (i = 1; i <= n2; i++)
    {
        e1[1] = (e1[1]+e1[2]+e1[3]-e1[4])*t;
        e1[2] = (e1[1]+e1[2]-e1[3]+e1[4])*t;
        e1[3] = (e1[1]-e1[2]+e1[3]+e1[4])*t;
        e1[4] = (-e1[1]+e1[2]+e1[3]+e1[4])*t;
    }
    pout(n2,n3,n2,e1[1],e1[2],e1[3],e1[4]);
    assert(s == "     120     140     120  -6.8342e-02  -4.6264e-01  -7.2972e-01  -1.1240e+00");

    for (i = 1; i <= n3; i++)
    {
        pa(e1);
    }
    pout(n3,n2,n2,e1[1],e1[2],e1[3],e1[4]);
    assert(s == "     140     120     120  -5.5336e-02  -4.4744e-01  -7.1097e-01  -1.1031e+00");

    j = 1;
    for (i = 1; i <= n4; i++)
    {
        j = (j-1) ? 3 : 2;
        j = (j-2 < 0) ? 0 : 1;
        j = (j-1 < 0) ? 1 : 0;
    }
    pout(n4, j, j, x1, x2, x3, x4);
    assert(s == "    3480       0       0   1.0000e+00  -1.0000e+00  -1.0000e+00  -1.0000e+00");

    j = 1;
    k = 2;
    l = 3;
    for (i = 1; i <= n6; i++)
    {
        j = j*(k-j)*(l-k);
        k = l*k-(l-j)*k;
        l = (l-k)*(k+j);
        e1[l-1] = j+k+l;
        e1[k-1] = j*k*l;
    }
    pout(n6,j,k,e1[1],e1[2],e1[3],e1[4]);
    assert(s == "    2100       1       2   6.0000e+00   6.0000e+00  -7.1097e-01  -1.1031e+00");

    x = 0.5;
    y = 0.5;
    for (i = 1; i <= n7; i++)
    {
        x = t * atan(t2* sin(x)* cos(x) /
            ( cos(x+y)+ cos(x-y)-1.0  ));
        y = t * atan(t2* sin(y)* cos(y) /
            ( cos(x+y)+ cos(x-y)-1.0  ));
    }
    pout(n7, j, k, x, x, y, y);
    assert(s == "     320       1       2   4.9041e-01   4.9041e-01   4.9039e-01   4.9039e-01");

    x = 1.0;
    y = 1.0;
    z = 1.0;

    for (i = 1; i <= n8; i++)
    {
        z = p3(x, y);
    }
    pout(n8, j, k, x, y, z, z);
    assert(s == "    8990       1       2   1.0000e+00   1.0000e+00   9.9994e-01   9.9994e-01");

    j = 1;
    k = 2;
    l = 3;
    e1[1] = 1.0;
    e1[2] = 2.0;
    e1[3] = 3.0;

    for (i = 1; i <= n9; i++)
    {
        p0();
    }
    pout(n9, j, k, e1[1], e1[2], e1[3], e1[4]);
    assert(s == "    5160       1       2   3.0000e+00   2.0000e+00   3.0000e+00  -1.1031e+00");

    j = 2;
    k = 3;

    for (i = 1; i <= n10; i++)
    {
        j = j + k;
        k = j + k;
        j = j - k;
        k = k - j - j;
    }
    pout(n10, j, k, x1, x2, x3, x4);
    assert(s == "       0       2       3   1.0000e+00  -1.0000e+00  -1.0000e+00  -1.0000e+00");

    x = 0.75;

    for (i = 1; i <= n11; i++)
    {
        x = sqrt( exp( log(x) / t1));
    }

    pout(n11,j,k,x,x,x,x);
    assert(s == "     930       2       3   8.3467e-01   8.3467e-01   8.3467e-01   8.3467e-01");

    stop = clock();

    version (none)
    printf("Elapsed time = %d.%02d seconds\n",
            cast(int)(stop - start)/CLOCKS_PER_SEC,
            cast(int)(stop - start)%CLOCKS_PER_SEC);
    return 0;
}

void pa(double[] e)
{
    for (j = 0; j < 6; j++)
    {
        e[1] = (e[1] + e[2] + e[3] - e[4]) * t;
        e[2] = (e[1] + e[2] - e[3] + e[4]) * t;
        e[3] = (e[1] - e[2] + e[3] + e[4]) * t;
        e[4] = (-e[1] + e[2] + e[3] + e[4]) / t2;
    }
}

void p0()
{
    e1[j] = e1[k];
    e1[k] = e1[l];
    e1[l] = e1[j];
}

double p3(double x, double y)
{
    x = t * (x + y);
    y = t * (x + y);
    return (x + y) / t2;
}

char[] pout(int n, int j, int k, double x1, double x2, double x3, double x4)
{
    __gshared char[80] result;
    const len = sprintf(result.ptr, " %7d %7d %7d %12.4e %12.4e %12.4e %12.4e",
            n, j, k, x1, x2, x3, x4);
    printf("%s\n", result.ptr);
    return result[0 .. len];
}
