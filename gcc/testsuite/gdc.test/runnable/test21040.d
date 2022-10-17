// REQUIRED_ARGS:
// PERMUTE_ARGS: -mcpu=native
// https://issues.dlang.org/show_bug.cgi?id=21040

import core.simd;

alias AliasSeq(A ...) = A;

void main()
{
    static foreach (T; AliasSeq!(
        float[8], float[4], double[4], double[2],
        byte[32], ubyte[32], byte[16], ubyte[16],
        short[16], ushort[16], short[8], ushort[8],
        int[8], uint[8], int[4], uint[4],
        long[4], ulong[4], long[2], ulong[2],
        void[32], void[16]))
    {
        static if (__traits(compiles, __vector(T)))
        {{
            __vector(T) v;

            static if (__traits(compiles, { __vector(T) x = 2; }))
                v = 2;
            static if (__traits(compiles, { __vector(T) x; x = +x; }))
                v = +v;
            static if (__traits(compiles, { __vector(T) x; x = -x; }))
                v = -v;
            static if (__traits(compiles, { __vector(T) x; x = x + x; }))
                v = v + v;
            static if (__traits(compiles, { __vector(T) x; x += 2; }))
                v += 2;
            static if (__traits(compiles, { __vector(T) x; x = x - x; }))
                v = v - v;
            static if (__traits(compiles, { __vector(T) x; x -= 2; }))
                v -= 2;
            static if (__traits(compiles, { __vector(T) x; x = x * x; }))
                v = v * v;
            static if (__traits(compiles, { __vector(T) x; x *= 2; }))
                v *= 2;
            static if (__traits(compiles, { __vector(T) x; x = x / x; }))
                v = v / v;
            static if (__traits(compiles, { __vector(T) x; x /= 2; }))
                v /= 2;
            static if (__traits(compiles, { __vector(T) x; x = x & x; }))
                v = v & v;
            static if (__traits(compiles, { __vector(T) x; x &= 2; }))
                v &= 2;
            static if (__traits(compiles, { __vector(T) x; x = x | x; }))
                v = v | v;
            static if (__traits(compiles, { __vector(T) x; x |= 2; }))
                v |= 2;
            static if (__traits(compiles, { __vector(T) x; x = x ^ x; }))
                v = v ^ v;
            static if (__traits(compiles, { __vector(T) x; x ^= 2; }))
                v ^= 2;
            static if (__traits(compiles, { __vector(T) x; x = ~x; }))
                v = ~v;
        }}
    }
}
