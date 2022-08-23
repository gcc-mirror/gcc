// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import gcc.simd;

void testblendvector(V, VI = V)()
{
    alias E = typeof(V.array[0]);
    enum numElements = V.sizeof / E.sizeof;

    static if (numElements == 16)
    {
        // Test fragment for vectors with 16 elements
        immutable V[5] in1 =
            [[ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ],
             [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ],
             [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ],
             [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ],
             [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ]];

        immutable V in2 =
            [ 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45 ];

        immutable VI[5] mask1 =
            [[ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ],
             [ 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 ],
             [ 7, 6, 5, 4, 16, 17, 18, 19, 31, 30, 29, 28, 3, 2, 1, 0 ],
             [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
             [ 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63 ]];

        immutable V[5] out1 =
            [[30, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25],
             [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25],
             [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 45],
             [30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45],
             [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]];
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
            immutable V[6] in1 =
                [[ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ]];


            immutable V in2 =
                [ A2, B2, C2, D2, E2, F2, G2, H2 ];

            immutable VI[6] mask1 =
                [[ 0,  1,  2 , 3 , 4 , 5 , 6 , 0 ],
                 [ 8,  9,  0, 11, 12, 13,  0, 15 ],
                 [ 0,  8,  1,  0,  2,  0,  3, 11 ],
                 [ 0, 15,  4, 11,  0,  3,  7,  8 ],
                 [ 0,  0,  0,  0,  0,  0,  0,  0 ],
                 [ 0x1e, 0x2e, 0x3e, 0x4e, 0x5e, 0x6e, 0x7e, 0x8e ]];

            immutable V[6] out1 =
                [[ A2, B1, C1, D1, E1, F1, G1, H2 ],
                 [ A1, B1, C2, D1, E1, F1, G2, H1 ],
                 [ A2, B1, C1, D2, E1, F2, G1, H1 ],
                 [ A2, B1, C1, D1, E2, F1, G1, H1 ],
                 [ A2, B2, C2, D2, E2, F2, G2, H2 ],
                 [ A1, B1, C1, D1, E1, F1, G1, H1 ]];
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
            immutable V[6] in1 =
                [[ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ],
                 [ A, B, C, D ]];

            immutable V in2 = [ W, X, Y, Z ];

            immutable VI[6] mask1 =
                [[ 0, 1, 2, 3 ],
                 [ 4, 0, 6, 7 ],
                 [ 0, 4, 0, 5 ],
                 [ 0, 7, 4, 0 ],
                 [ 0, 0, 0, 0 ],
                 [ 7, 7, 7, 7 ]];

            immutable V[6] out1 =
                [[ W, B, C, D ],
                 [ A, X, C, D ],
                 [ W, B, Y, D ],
                 [ W, B, C, Z ],
                 [ W, X, Y, Z ],
                 [ A, B, C, D ]];
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
            immutable V[7] in1 =
                [[ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ A, B ],
                 [ A, B ]];

            immutable V in2 = [ X, Y ];

            immutable VI[7] mask1 =
                [[ 0, 1 ],
                 [ 2, 3 ],
                 [ 0, 2 ],
                 [ 2, 1 ],
                 [ 3, 0 ],
                 [ 0, 0 ],
                 [ 3, 3 ]];

            immutable V[7] out1 =
                [[ X, B ],
                 [ A, B ],
                 [ X, B ],
                 [ A, B ],
                 [ A, Y ],
                 [ X, Y ],
                 [ A, B ]];
        }
    }
    else
        enum unsupported = true;

    static if (!__traits(compiles, unsupported))
    {
        static foreach (i; 0 .. in1.length)
            assert(blendvector(in1[i], in2, mask1[i]).array == out1[i].array);
    }
}

void main()
{
    static if (__traits(compiles, __vector(ubyte[16])))
        testblendvector!(__vector(ubyte[16]))();

    static if (__traits(compiles, __vector(ushort[16])))
        testblendvector!(__vector(ushort[16]))();

    static if (__traits(compiles, __vector(ubyte[8])))
        testblendvector!(__vector(ubyte[8]))();

    static if (__traits(compiles, __vector(ushort[8])))
        testblendvector!(__vector(ushort[8]))();

    static if (__traits(compiles, __vector(uint[8])))
        testblendvector!(__vector(uint[8]))();

    static if (__traits(compiles, __vector(ulong[4])))
    {
        testblendvector!(__vector(ulong[4]));

        static if (__traits(compiles, __vector(double[4])))
            testblendvector!(__vector(double[4]), __vector(ulong[4]));
    }

    static if (__traits(compiles, __vector(uint[4])))
    {
        testblendvector!(__vector(uint[4]));

        static if (__traits(compiles, __vector(float[4])))
            testblendvector!(__vector(float[4]), __vector(uint[4]));
    }

    static if (__traits(compiles, __vector(ulong[2])))
    {
        testblendvector!(__vector(ulong[2]));

        static if (__traits(compiles, __vector(double[2])))
            testblendvector!(__vector(double[2]), __vector(ulong[2]));
    }

    static if (__traits(compiles, __vector(uint[2])))
    {
        testblendvector!(__vector(uint[2]));

        static if (__traits(compiles, __vector(float[2])))
            testblendvector!(__vector(float[2]), __vector(uint[2]));
    }
}
