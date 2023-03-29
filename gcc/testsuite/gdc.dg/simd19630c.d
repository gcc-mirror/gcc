// https://issues.dlang.org/show_bug.cgi?id=19630
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

int4 testz19630()
{
    return [0,0,0,0];
}

void test19630()
{
    assert(testz19630()[] == [0,0,0,0]);
    // { dg-error "'__vector\\\(int\\\[4\\\]\\\)' cannot be sliced with '\\\[\\\]'" "" { target *-*-* } .-1 }
}
