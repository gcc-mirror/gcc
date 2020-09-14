// https://issues.dlang.org/show_bug.cgi?id=19629
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

enum fail19629a = int4(0)[0];
enum fail19629b = int4(0).array[0];
enum fail19629c = (cast(int[4])int4(0).array)[0];
enum fail19628d = (cast(int[4])int4(0))[0];

enum int4 v19629a = int4(0);
enum idx19629a = v19629a[0];
static assert(idx19629a == 0);

enum int[4] v19629b = int4(0).array;
enum idx19629b = v19629b[0];
static assert(idx19629b == 0);

enum int[4] v19629c = cast(int[4])int4(0).array;
enum idx19629c = v19629c[0];
static assert(idx19629c == 0);

enum int[4] v19629d = cast(int[4])int4(0);
enum idx19629d = v19629d[0];
static assert(idx19629d == 0);

immutable int4 v19629e = int4(0);
immutable idx19629e = v19629e[0];
static assert(idx19629e == 0);

immutable int[4] v19629f = int4(0).array;
immutable idx19629f = v19629f[0];
static assert(idx19629f == 0);

immutable int[4] v19629g = cast(int[4])int4(0).array;
immutable idx19629g = v19629g[0];
static assert(idx19629g == 0);

immutable int[4] v19629h = cast(int[4])int4(0);
immutable idx19629h = v19629h[0];
static assert(idx19629h == 0);
