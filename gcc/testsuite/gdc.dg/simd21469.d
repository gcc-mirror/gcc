// https://issues.dlang.org/show_bug.cgi?id=18867
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

int4 test21469(short a)
{
    return cast(int4)(short8(a));
}
