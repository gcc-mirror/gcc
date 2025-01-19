/*
REQUIRED_ARGS: -inline
*/

// https://issues.dlang.org/show_bug.cgi?id=24884

pragma(inline, false)
bool norm(int a) => 0;

pragma(inline, false)
void inlinebug(ref double[4] point1, ref double[4] point2, ref double[4] point3, ref double[4] abcd)
{
    double[4] v1 = 0.0;
    double[4] v2 = 0.0;

    v1[0] = point1[0] - point2[0];
    v1[1] = point1[1] - point2[1];
    v1[2] = point1[2] - point2[2];
    v1[3] = point1[3];
    v2[0] = point2[0] - point3[0];
    v2[1] = point2[1] - point3[1];
    v2[2] = point2[2] - point3[2];

    int p = cast(int) &abcd;
    int q = cast(int) &point1;
    abcd[0] = norm(7) + p;
    abcd[1] = q + p;
}

extern(C) void main()
{
    double[4] a = 0.0;
    inlinebug(a, a, a, a);
}
