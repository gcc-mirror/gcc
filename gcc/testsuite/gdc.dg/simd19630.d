// https://issues.dlang.org/show_bug.cgi?id=19630
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

enum fail19630a = int4.init[1..2];
enum fail19630b = int4.init.array[1..2];
enum fail19630c = (cast(int[4])int4.init.array)[1..2];
enum fail19630d = (cast(int[4])int4.init)[1..2];
enum fail19630e = int4(0)[1..2];
enum fail19630f = int4(0).array[1..2];
enum fail19630g = (cast(int[4])int4(0).array)[1..2];
enum fail19630h = (cast(int[4])int4(0))[1..2];

enum int4 v19630a = int4.init;
enum slice19630a = v19630a[1..2];
static assert(slice19630a == [0]);

enum int[4] v19630b = int4.init.array;
enum slice19630b = v19630b[1..2];
static assert(slice19630b == [0]);

enum int[4] v19630c = cast(int[4])int4.init.array;
enum slice19630c = v19630c[1..2];
static assert(slice19630c == [0]);

enum int[4] v19630d = cast(int[4])int4.init;
enum slice19630d = v19630d[1..2];
static assert(slice19630d == [0]);

enum int4 v19630e = int4(0);
enum slice19630e = v19630e[1..2];
static assert(slice19630e == [0]);

enum int[4] v19630f = int4(0).array;
enum slice19630f = v19630f[1..2];
static assert(slice19630f == [0]);

enum int[4] v19630g = cast(int[4])int4(0).array;
enum slice19630g = v19630g[1..2];
static assert(slice19630g == [0]);

enum int[4] v19630h = cast(int[4])int4(0);
enum slice19630h = v19630h[1..2];
static assert(slice19630h == [0]);

immutable int4 v19630i = int4.init;
immutable slice19630i = v19630i[1..2];
static assert(slice19630i == [0]);

immutable int[4] v19630j = int4.init.array;
immutable slice19630j = v19630j[1..2];
static assert(slice19630j == [0]);

immutable int[4] v19630k = cast(int[4])int4.init.array;
immutable slice19630k = v19630k[1..2];
static assert(slice19630k == [0]);

immutable int[4] v19630l = cast(int[4])int4.init;
immutable slice19630l = v19630l[1..2];
static assert(slice19630l == [0]);

immutable int4 v19630m = int4(0);
immutable slice19630m = v19630m[1..2];
static assert(slice19630m == [0]);

immutable int[4] v19630n = int4(0).array;
immutable slice19630n = v19630n[1..2];
static assert(slice19630n == [0]);

immutable int[4] v19630o = cast(int[4])int4(0).array;
immutable slice19630o = v19630o[1..2];
static assert(slice19630o == [0]);

immutable int[4] v19630p = cast(int[4])int4(0);
immutable slice19630p = v19630p[1..2];
static assert(slice19630p == [0]);
