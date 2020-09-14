// https://issues.dlang.org/show_bug.cgi?id=7411
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

void BaseTypeOfVector(T : __vector(T[N]), size_t N)(int i)
{
    assert(is(T == int));
    assert(N == 4);
}

void main()
{
    BaseTypeOfVector!(__vector(int[4]))(3);
}
