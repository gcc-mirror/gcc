// https://issues.dlang.org/show_bug.cgi?id=21432
auto issue21432()
{
    enum int[] a = [];
    return a;
}

enum test21432a = issue21432;

///////////////////////

double issue21432b(double r)
{
    enum double[4] poly = [
        0x1.ffffffffffdbdp-2,
        0x1.555555555543cp-3,
        0x1.55555cf172b91p-5,
        0x1.1111167a4d017p-7,
    ];

    immutable r2 = r * r;
    return r + r2 * (poly[0] + r * poly[1]) + r2 * r2 * (poly[2] + r * poly[3]);
}

enum test21432b = issue21432b(-0x1p-1);
