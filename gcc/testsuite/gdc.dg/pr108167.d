// { dg-do compile }
auto pr108167(const(ubyte[32])[] a)
{
    return cast(const(ubyte)*)&a[1][0];
}
