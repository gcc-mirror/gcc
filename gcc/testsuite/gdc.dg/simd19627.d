// https://issues.dlang.org/show_bug.cgi?id=19627
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

enum int[4] fail19627 = cast(int[4])int4(0);
