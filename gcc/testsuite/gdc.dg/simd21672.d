// https://bugzilla.gdcproject.org/show_bug.cgi?id=213
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }

import core.simd;

struct S213
{
    int4 vec;
}

void test213()
{
    S213 s, b;

    assert(s == b);
}
