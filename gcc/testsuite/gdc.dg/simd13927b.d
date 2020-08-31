// https://issues.dlang.org/show_bug.cgi?id=13927
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

alias double8 = __vector(double[8]);

void test13927(double8 a)
{
    double8 b = [long.min, long.min, long.max, long.max];
    auto tmp = a - b;
}
