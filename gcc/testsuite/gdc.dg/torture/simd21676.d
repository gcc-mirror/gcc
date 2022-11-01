// https://issues.dlang.org/show_bug.cgi?id=23009
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

double2 loadUnaligned21676(const(double)* pvec)
{
    double2 result;
    foreach(i; 0..2)
    {
        result[i] = pvec[i];
    }
    return result;
}

double2 _mm_setr_pd(double e1, double e0)
{
    double[2] result = [e1, e0];
    return loadUnaligned21676(result.ptr);
}

double2 fun(double2 a, double2 b)
{
    a[0] = (a[0] < b[0]) ? a[0] : b[0];
    return a;
}

void main()
{
    double2 A = _mm_setr_pd(1.0, 2.0);
    double2 B = _mm_setr_pd(4.0, 1.0);
    double2 C = fun(A, B);
    assert(C.array[0] == 1.0);
    assert(C.array[1] == 2.0);
}
