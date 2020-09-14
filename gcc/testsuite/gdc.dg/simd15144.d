// https://issues.dlang.org/show_bug.cgi?id=15144
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

void test15144()
{
        enum      ubyte16 csXMM1 = ['a','b','c',0,0,0,0,0];
        __gshared ubyte16 csXMM2 = ['a','b','c',0,0,0,0,0];
        immutable ubyte16 csXMM3 = ['a','b','c',0,0,0,0,0];
}
