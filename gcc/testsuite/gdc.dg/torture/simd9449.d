// https://issues.dlang.org/show_bug.cgi?id=9449
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

void test9449()
{
    ubyte16[1] table;
}

void test9449_2()
{
    float[4][2] m = [[2.0, 1, 3, 4], [5.0, 6, 7, 8]];   // segfault

    assert(m[0][0] == 2.0);
    assert(m[0][1] == 1);
    assert(m[0][2] == 3);
    assert(m[0][3] == 4);

    assert(m[1][0] == 5.0);
    assert(m[1][1] == 6);
    assert(m[1][2] == 7);
    assert(m[1][3] == 8);
}

void main()
{
    test9449();
    test9449_2();
}
