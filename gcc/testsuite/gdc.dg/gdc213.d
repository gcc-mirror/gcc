// https://bugzilla.gdcproject.org/show_bug.cgi?id=213
// { dg-options "-Wno-psabi" }
// { dg-do compile }

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
