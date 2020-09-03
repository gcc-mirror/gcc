// https://issues.dlang.org/show_bug.cgi?id=13927
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

void test13927(ulong2 a)
{
    ulong2 b = [long.min, long.min];
    auto tmp = a - b;
}
