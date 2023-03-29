// https://issues.dlang.org/show_bug.cgi?id=19630
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

enum fail19630a = int4.init[1..2];
// { dg-error "'__vector\\\(int\\\[4\\\]\\\)' cannot be sliced with '\\\[\\\]'" "" { target *-*-* } .-1 }
enum fail19630e = int4(0)[1..2];
// { dg-error "'__vector\\\(int\\\[4\\\]\\\)' cannot be sliced with '\\\[\\\]'" "" { target *-*-* } .-1 }

enum int4 v19630a = int4.init;
enum slice19630a = v19630a[1..2];
// { dg-error "'__vector\\\(int\\\[4\\\]\\\)' cannot be sliced with '\\\[\\\]'" "" { target *-*-* } .-1 }

enum int4 v19630e = int4(0);
enum slice19630e = v19630e[1..2];
// { dg-error "'__vector\\\(int\\\[4\\\]\\\)' cannot be sliced with '\\\[\\\]'" "" { target *-*-* } .-1 }
