// https://issues.dlang.org/show_bug.cgi?id=9200
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

void bar9200(double[2] a)
{
    assert(a[0] == 1);
    assert(a[1] == 2);
}

double2 * v9200(double2* a)
{
    return a;
}

void main()
{
    double2 a = [1, 2];

    *v9200(&a) = a;

    bar9200(a.array);
}
