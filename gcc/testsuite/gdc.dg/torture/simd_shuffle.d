// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import gcc.simd;

void testshuffle(V, VI = V)()
{
    alias E = typeof(V.array[0]);
    enum numElements = V.sizeof / E.sizeof;

    static if (numElements == 16)
    {
        // Test fragment for vectors with 16 elements
        immutable V[5] in1 =
            [[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ],
             [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ],
             [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ],
             [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ],
             [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ]];

        immutable VI[5] mask1 =
            [[ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ],
             [ 0x10, 0x21, 0x32, 0x43, 0x54, 0x65, 0x76, 0x87,
               0x98, 0xa9, 0xba, 0xcb, 0xdc, 0xed, 0xfe, 0xff ]	,
             [ 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 ],
             [ 0, 2, 4, 6, 8, 10, 12, 14, 1, 3, 5, 7, 9, 11, 13, 15 ],
             [ 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 ]];

        immutable V[5] out1 =
            [[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ],
             [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ],
             [ 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 ],
             [ 1, 3, 5, 7, 9, 11, 13, 15, 2, 4, 6, 8, 10, 12, 14, 16 ],
             [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ]];

        immutable V[5] in2 =
            [[ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ],
             [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ],
             [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ],
             [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ],
             [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ]];

        immutable V in3 =
            [ 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45 ];

        immutable VI[5] mask2 =
            [[ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ],
             [ 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 ],
             [ 7, 6, 5, 4, 16, 17, 18, 19, 31, 30, 29, 28, 3, 2, 1, 0 ],
             [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
             [ 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63 ]];

        immutable V[5] out2 =
            [[ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ],
             [ 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45 ],
             [ 17, 16, 15, 14, 30, 31, 32, 33, 45, 44, 43, 42, 13, 12, 11, 10 ],
             [ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 ],
             [ 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 ]];
    }
    else static if (numElements == 8)
    {
        // Test fragment for vectors with 8 elements
        static if (is(E == uint))
        {
            enum E A1 = 0x11121314;
            enum E B1 = 0x21222324;
            enum E C1 = 0x31323334;
            enum E D1 = 0x41424344;
            enum E E1 = 0x51525354;
            enum E F1 = 0x61626364;
            enum E G1 = 0x71727374;
            enum E H1 = 0x81828384;

            enum E A2 = 0x91929394;
            enum E B2 = 0xa1a2a3a4;
            enum E C2 = 0xb1b2b3b4;
            enum E D2 = 0xc1c2c3c4;
            enum E E2 = 0xd1d2d3d4;
            enum E F2 = 0xe1e2e3e4;
            enum E G2 = 0xf1f2f3f4;
            enum E H2 = 0x01020304;
        }
        else static if (is(E == ushort))
        {
            enum E A1 = 0x1112;
            enum E B1 = 0x2122;
            enum E C1 = 0x3132;
            enum E D1 = 0x4142;
            enum E E1 = 0x5152;
            enum E F1 = 0x6162;
            enum E G1 = 0x7172;
            enum E H1 = 0x8182;

            enum E A2 = 0x9192;
            enum E B2 = 0xa1a2;
            enum E C2 = 0xb1b2;
            enum E D2 = 0xc1c2;
            enum E E2 = 0xd1d2;
            enum E F2 = 0xe1e2;
            enum E G2 = 0xf1f2;
            enum E H2 = 0x0102;
        }
        else static if (is(E == ubyte))
        {
            enum E A1 = 0x11;
            enum E B1 = 0x12;
            enum E C1 = 0x13;
            enum E D1 = 0x14;
            enum E E1 = 0x15;
            enum E F1 = 0x16;
            enum E G1 = 0x17;
            enum E H1 = 0x18;

            enum E A2 = 0xf1;
            enum E B2 = 0xf2;
            enum E C2 = 0xf3;
            enum E D2 = 0xf4;
            enum E E2 = 0xf5;
            enum E F2 = 0xf6;
            enum E G2 = 0xf7;
            enum E H2 = 0xf8;
        }
        else
            enum unsupported = true;

        static if (!__traits(compiles, unsupported))
        {
            immutable V[8] in1 =
                [[ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A2, B2, C2, D2, E2, F2, G2, H2 ],
                 [ A2, B2, C2, D2, E2, F2, G2, H2 ],
                 [ A2, B2, C2, D2, E2, F2, G2, H2 ]];

            immutable VI[8] mask1 =
                [[  0,  1,  2,  3,  4,  5,  6,  7 ],
                 [ 0x10, 0x21, 0x32, 0x43, 0x54, 0x65, 0x76, 0x87 ],
                 [  7,  6,  5,  4,  3,  2,  1,  0 ],
                 [  7,  0,  5,  3,  2,  4,  1,  6 ],
                 [  0,  2,  1,  3,  4,  6,  5,  7 ],
                 [  3,  1,  2,  0,  7,  5,  6,  4 ],
                 [ 0, 0, 0, 0 ],
                 [  1,  6,  1,  6,  1,  6,  1,  6 ]];

            immutable V[8] out1 =
                [[ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ H1, G1, F1, E1, D1, C1, B1, A1 ],
                 [ H1, A1, F1, D1, C1, E1, B1, G1 ],
                 [ A1, C1, B1, D1, E1, G1, F1, H1 ],
                 [ D2, B2, C2, A2, H2, F2, G2, E2 ],
                 [ A2, A2, A2, A2, A2, A2, A2, A2 ],
                 [ B2, G2, B2, G2, B2, G2, B2, G2 ]];

            immutable V[6] in2 =
                [[ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ]];


            immutable V in3 =
                [ A2, B2, C2, D2, E2, F2, G2, H2 ];

            immutable VI[6] mask2 =
                [[ 0, 1, 2, 3, 4, 5, 6, 7 ],
                 [  8,  9, 10, 11, 12, 13, 14, 15 ],
                 [  0,  8,  1,  9,  2, 10,  3, 11 ],
                 [  0, 15,  4, 11, 12,  3,  7,  8 ],
                 [  0,  0,  0,  0,  0,  0,  0,  0 ],
                 [ 0x1e, 0x2e, 0x3e, 0x4e, 0x5e, 0x6e, 0x7e, 0x8e ]];

            immutable V[6] out2 =
                [[ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A2, B2, C2, D2, E2, F2, G2, H2 ],
                 [ A1, A2, B1, B2, C1, C2, D1, D2 ],
                 [ A1, H2, E1, D2, E2, D1, H1, A2 ],
                 [ A1, A1, A1, A1, A1, A1, A1, A1 ],
                 [ G2, G2, G2, G2, G2, G2, G2, G2 ]];
        }
    }
    else static if (numElements == 4)
    {
        // Test fragment for vectors with 4 elements
        static if (is(E == double))
        {
            enum E A = 0.69314718055994530942;
            enum E B = 2.7182818284590452354;
            enum E C = 2.30258509299404568402;
            enum E D = 1.4426950408889634074;

            enum E W = 0.31830988618379067154;
            enum E X = 3.14159265358979323846;
            enum E Y = 1.41421356237309504880;
            enum E Z = 0.70710678118654752440;
        }
        else static if (is(E == float))
        {
            enum E A = 0.69314718055994530942f;
            enum E B = 2.7182818284590452354f;
            enum E C = 2.30258509299404568402f;
            enum E D = 1.4426950408889634074f;

            enum E W = 0.31830988618379067154f;
            enum E X = 3.14159265358979323846f;
            enum E Y = 1.41421356237309504880f;
            enum E Z = 0.70710678118654752440f;
        }
        else static if (is(E == ulong))
        {
            enum E A = 0x1112131415161718;
            enum E B = 0x2122232425262728;
            enum E C = 0x3132333435363738;
            enum E D = 0x4142434445464748;

            enum E W = 0xc1c2c3c4c5c6c7c8;
            enum E X = 0xd1d2d3d4d5d6d7d8;
            enum E Y = 0xe1e2e3e4e5e6e7e8;
            enum E Z = 0xf1f2f3f4f5f6f7f8;
        }
        else static if (is(E == uint))
        {
            enum E A = 0x11121314;
            enum E B = 0x21222324;
            enum E C = 0x31323334;
            enum E D = 0x41424344;

            enum E W = 0xc1c2c3c4;
            enum E X = 0xd1d2d3d4;
            enum E Y = 0xe1e2e3e4;
            enum E Z = 0xf1f2f3f4;
        }
        else
            enum unsupported = true;

        static if (!__traits(compiles, unsupported))
        {
            immutable V[8] in1 =
                [[ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ],
                 [ W, X, Y, Z ],
                 [ W, X, Y, Z ],
                 [ W, X, Y, Z ]];

            immutable VI[8] mask1 =
                [[ 0, 1, 2, 3 ],
                 [ 0+1*4, 1+2*4, 2+3*4, 3+4*4 ],
                 [ 3, 2, 1, 0 ],
                 [ 0, 3, 2, 1 ],
                 [ 0, 2, 1, 3 ],
                 [ 3, 1, 2, 0 ],
                 [ 0, 0, 0, 0 ],
                 [ 1, 2, 1, 2 ]];

            immutable V[8] out1 =
                [[ A, B, C, D ],
                 [ A, B, C, D ],
                 [ D, C, B, A ],
                 [ A, D, C, B ],
                 [ A, C, B, D ],
                 [ Z, X, Y, W ],
                 [ W, W, W, W ],
                 [ X, Y, X, Y ]];


            immutable V[6] in2 =
                [[ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ]];

            immutable V in3 = [ W, X, Y, Z ];

            immutable VI[6] mask2 =
                [[ 0, 1, 2, 3 ],
                 [ 4, 5, 6, 7 ],
                 [ 0, 4, 1, 5 ],
                 [ 0, 7, 4, 3 ],
                 [ 0, 0, 0, 0 ],
                 [ 7, 7, 7, 7 ]];

            immutable V[6] out2 =
                [[ A, B, C, D ],
                 [ W, X, Y, Z ],
                 [ A, W, B, X ],
                 [ A, Z, W, D ],
                 [ A, A, A, A ],
                 [ Z, Z, Z, Z ]];
        }
    }
    else static if (numElements == 2)
    {
        // Test fragment for vectors with 2 elements
        static if (is(E == double))
        {
            enum E A = 0.69314718055994530942;
            enum E B = 2.7182818284590452354;

            enum E X = 3.14159265358979323846;
            enum E Y = 1.41421356237309504880;
        }
        else static if (is(E == float))
        {
            enum E A = 0.69314718055994530942f;
            enum E B = 2.7182818284590452354f;

            enum E X = 3.14159265358979323846f;
            enum E Y = 1.41421356237309504880f;
        }
        else static if (is(E == ulong))
        {
            enum E A = 0x1112131415161718;
            enum E B = 0x2122232425262728;

            enum E X = 0xc1c2c3c4c5c6c7c8;
            enum E Y = 0xd1d2d3d4d5d6d7d8;
        }
        else static if (is(E == uint))
        {
            enum E A = 0x11121314;
            enum E B = 0x21222324;

            enum E X = 0xd1d2d3d4;
            enum E Y = 0xe1e2e3e4;
        }
        else
            enum unsupported = true;

        static if (!__traits(compiles, unsupported))
        {
            immutable V[6] in1 =
                [[ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ X, Y ],
                 [ X, Y ]];

            immutable VI[6] mask1 =
                [[ 0, 1 ],
                 [ -16, 1 ],
                 [ 1, 0 ],
                 [ 0, 0 ],
                 [ 1, 1 ],
                 [ 1, 0 ]];

            immutable V[6] out1 =
                [[ A, B ],
                 [ A, B ],
                 [ B, A ],
                 [ A, A ],
                 [ Y, Y ],
                 [ Y, X ]];

            immutable V[7] in2 =
                [[ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ A, B ]];

            immutable V in3 = [ X, Y ];

            immutable VI[7] mask2 =
                [[ 0, 1 ],
                 [ 2, 3 ],
                 [ 0, 2 ],
                 [ 2, 1 ],
                 [ 3, 0 ],
                 [ 0, 0 ],
                 [ 3, 3 ]];

            immutable V[7] out2 =
                [[ A, B ],
                 [ X, Y ],
                 [ A, X ],
                 [ X, B ],
                 [ Y, A ],
                 [ A, A ],
                 [ Y, Y ]];
        }
    }
    else
        enum unsupported = true;

    static if (!__traits(compiles, unsupported))
    {
        static foreach (i; 0 .. in1.length)
            assert(shuffle(in1[i], mask1[i]).array == out1[i].array);
        static foreach (i; 0 .. in2.length)
            assert(shuffle(in2[i], in3, mask2[i]).array == out2[i].array);
    }
}

void main()
{
    static if (__traits(compiles, __vector(ubyte[16])))
        testshuffle!(__vector(ubyte[16]))();

    static if (__traits(compiles, __vector(ushort[16])))
        testshuffle!(__vector(ushort[16]))();

    static if (__traits(compiles, __vector(ubyte[8])))
        testshuffle!(__vector(ubyte[8]))();

    static if (__traits(compiles, __vector(ushort[8])))
        testshuffle!(__vector(ushort[8]))();

    static if (__traits(compiles, __vector(uint[8])))
        testshuffle!(__vector(uint[8]))();

    static if (__traits(compiles, __vector(ulong[4])))
    {
        testshuffle!(__vector(ulong[4]));

        static if (__traits(compiles, __vector(double[4])))
            testshuffle!(__vector(double[4]), __vector(ulong[4]));
    }

    static if (__traits(compiles, __vector(uint[4])))
    {
        testshuffle!(__vector(uint[4]));

        static if (__traits(compiles, __vector(float[4])))
            testshuffle!(__vector(float[4]), __vector(uint[4]));
    }

    static if (__traits(compiles, __vector(ulong[2])))
    {
        testshuffle!(__vector(ulong[2]));

        static if (__traits(compiles, __vector(double[2])))
            testshuffle!(__vector(double[2]), __vector(ulong[2]));
    }

    static if (__traits(compiles, __vector(uint[2])))
    {
        testshuffle!(__vector(uint[2]));

        static if (__traits(compiles, __vector(float[2])))
            testshuffle!(__vector(float[2]), __vector(uint[2]));
    }
}
