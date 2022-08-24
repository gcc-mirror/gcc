// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

// https://issues.dlang.org/show_bug.cgi?id=19627
enum int[4] fail19627 = cast(int[4])int4(0);

// https://issues.dlang.org/show_bug.cgi?id=19628
enum ice19628a = int4.init[0];
enum ice19628b = int4.init.array[0];
enum ice19628c = (cast(int[4])int4.init.array)[0];
enum ice19628d  = (cast(int[4])int4.init)[0];

// https://issues.dlang.org/show_bug.cgi?id=19629
enum fail19629a = int4(0)[0];
enum fail19629b = int4(0).array[0];
enum fail19629c = (cast(int[4])int4(0).array)[0];
enum fail19628d = (cast(int[4])int4(0))[0];

// https://issues.dlang.org/show_bug.cgi?id=19630
enum fail19630a = int4.init[1..2];
enum fail19630b = int4.init.array[1..2];
enum fail19630c = (cast(int[4])int4.init.array)[1..2];
enum fail19630d = int4(0)[1..2];
enum fail19630e = int4(0).array[1..2];
enum fail19630f = (cast(int[4])int4(0).array)[1..2];
enum fail19630g = (cast(int[4])int4.init)[1..2];
enum fail19630h = (cast(int[4])int4(0))[1..2];

// Same tests as above, but use access via enum.
enum int4   V1 = int4.init;
enum int[4] V2 = int4.init.array;
enum int[4] V3 = cast(int[4])int4.init;
enum int[4] V4 = cast(int[4])int4.init.array;
enum int4   V5 = int4(0);
enum int[4] V6 = int4(0).array;
enum int[4] V7 = cast(int[4])int4(0);
enum int[4] V8 = cast(int[4])int4(0).array;

// CTFE index tests
enum I1 = V1[0];    static assert(I1 == 0);
enum I2 = V2[0];    static assert(I2 == 0);
enum I3 = V3[0];    static assert(I3 == 0);
enum I4 = V4[0];    static assert(I4 == 0);
enum I5 = V5[0];    static assert(I5 == 0);
enum I6 = V6[0];    static assert(I6 == 0);
enum I7 = V7[0];    static assert(I7 == 0);
enum I8 = V8[0];    static assert(I8 == 0);

// CTFE slice tests
enum S1 = V1[1..2]; static assert(S1 == [0]);
enum S2 = V2[1..2]; static assert(S2 == [0]);
enum S3 = V3[1..2]; static assert(S3 == [0]);
enum S4 = V4[1..2]; static assert(S4 == [0]);
enum S5 = V5[1..2]; static assert(S5 == [0]);
enum S6 = V6[1..2]; static assert(S6 == [0]);
enum S7 = V7[1..2]; static assert(S7 == [0]);
enum S8 = V8[1..2]; static assert(S8 == [0]);

// Same tests as above, but use access via immutable.
//immutable int4   v1 = int4.init;      // Cannot cast to immutable at compile time
immutable int[4] v2 = int4.init.array;
immutable int[4] v3 = cast(int[4])int4.init;
immutable int[4] v4 = cast(int[4])int4.init.array;
//immutable int4   v5 = int4(0);        // Cannot cast to immutable at compile time
immutable int[4] v6 = int4(0).array;
immutable int[4] v7 = cast(int[4])int4(0);
immutable int[4] v8 = cast(int[4])int4(0).array;

// CTFE index tests
//immutable i1 = v1[0];    static assert(i1 == 0);
immutable i2 = v2[0];    static assert(i2 == 0);
immutable i3 = v3[0];    static assert(i3 == 0);
immutable i4 = v4[0];    static assert(i4 == 0);
//immutable i5 = v5[0];    static assert(i5 == 0);
immutable i6 = v6[0];    static assert(i6 == 0);
immutable i7 = v7[0];    static assert(i7 == 0);
immutable i8 = v8[0];    static assert(i8 == 0);

// CTFE slice tests
//immutable s1 = v1[1..2]; static assert(s1 == [0]);
immutable s2 = v2[1..2]; static assert(s2 == [0]);
immutable s3 = v3[1..2]; static assert(s3 == [0]);
immutable s4 = v4[1..2]; static assert(s4 == [0]);
//immutable s5 = v5[1..2]; static assert(s5 == [0]);
immutable s6 = v6[1..2]; static assert(s6 == [0]);
immutable s7 = v7[1..2]; static assert(s7 == [0]);
immutable s8 = v8[1..2]; static assert(s8 == [0]);
