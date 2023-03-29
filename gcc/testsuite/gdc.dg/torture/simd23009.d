// https://issues.dlang.org/show_bug.cgi?id=23009
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

double2 _mm_loadl_pd(double2 a, const(double)* mem_addr)
{
    a[0] = *mem_addr;
    return a;
}

void main()
{
    double A = 7.0;
    double2 B;
    B[0] = 4.0;
    B[1] = -5.0;
    double2 R = _mm_loadl_pd(B, &A);
    double[2] correct = [ 7.0, -5.0 ];
    assert(R.array == correct);
}
