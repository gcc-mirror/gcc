// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

void main()
{
    int4 c = 7;
    (cast(int[4])c)[3] = 4;
    (cast(int*)&c)[2] = 4;
    c.array[1] = 4;
    c.ptr[3] = 4;
    assert(c.length == 4);
}
