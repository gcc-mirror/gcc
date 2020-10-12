// https://issues.dlang.org/show_bug.cgi?id=10447
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }

void test10447()
{
    immutable __vector(double[2]) a = [1.0, 2.0];
    __vector(double[2]) r;
    r += a;
    r = r * a;
}
