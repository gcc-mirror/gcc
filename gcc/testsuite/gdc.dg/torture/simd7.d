// { dg-additional-options "-mavx2" { target avx2_runtime } }
// { dg-do run { target { avx2_runtime || vect_sizes_32B_16B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

double4 test7r(double4 v)
{
    return v;
}

void main()
{
    // 32 bytes sliced down to 16 bytes
    double4 v = 1;
    double4 r = test7r(v);
    assert(v[2] == r[2]);
    assert(v[3] == r[3]);
}
