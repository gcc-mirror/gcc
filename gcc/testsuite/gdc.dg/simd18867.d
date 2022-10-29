// https://issues.dlang.org/show_bug.cgi?id=18867
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

ulong2 test18867(ulong s)
{
    ulong2 v;
    v[0] = s;
    return v;
}
