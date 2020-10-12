// https://issues.dlang.org/show_bug.cgi?id=19628
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

enum ice19628a = int4.init[0];
enum ice19628b = int4.init.array[0];
enum ice19628c = (cast(int[4])int4.init.array)[0];
enum ice19628d = (cast(int[4])int4.init)[0];

enum int4 v19628a = int4.init;
enum idx19628a = v19628a[0];
static assert(idx19628a == 0);

enum int[4] v19628b = int4.init.array;
enum idx19628b = v19628b[0];
static assert(idx19628b == 0);

enum int[4] v19628c = cast(int[4])int4.init.array;
enum idx19628c = v19628c[0];
static assert(idx19628c == 0);

enum int[4] v19628d = cast(int[4])int4.init;
enum idx19628d = v19628d[0];
static assert(idx19628d == 0);

immutable int4 v19628e = int4.init;
immutable idx19628e = v19628e[0];
static assert(idx19628e == 0);

immutable int[4] v19628f = int4.init.array;
immutable idx19628f = v19628f[0];
static assert(idx19628f == 0);

immutable int[4] v19628g = cast(int[4])int4.init.array;
immutable idx19628g = v19628g[0];
static assert(idx19628g == 0);

immutable idx19628h = v19628h[0];
immutable int[4] v19628h = cast(int[4])int4.init;
static assert(idx19628h == 0);
