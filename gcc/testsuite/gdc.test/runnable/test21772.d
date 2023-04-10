// https://issues.dlang.org/show_bug.cgi?id=21772

import core.stdc.string;

int main()
{
    //import std.stdio : writeln;

    double[] a = [-double.nan, double.nan,  double.nan,
                          1.0, double.nan, -double.nan];
    //writeln(a); // Writes "[-nan, -nan, -nan, 1, nan, nan]" (Uh-oh!)
    assert(memcmp(&a[0], &a[1], double.sizeof) != 0);
    return 0;
}
