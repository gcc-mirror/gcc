// https://issues.dlang.org/show_bug.cgi?id=19224
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

float test19224(const float[4] val)
{
    float sum = 0;
    foreach (x; val) sum += x;
    return sum;
}

enum x19224 = test19224(float4.init.array);
static assert(x19224 is float.nan);

enum y19224 = test19224(float4(1).array);
static assert(y19224 == 4);
