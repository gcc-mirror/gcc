// https://bugzilla.gdcproject.org/show_bug.cgi?id=284
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }

alias v284 = __vector(int[2]);

v284 test284(v284 a, ...)
{
    return a + a;
}
