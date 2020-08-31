// https://issues.dlang.org/show_bug.cgi?id=17695
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }

void test17695(__vector(ubyte[16]) a)
{
    auto b = -a;
}
