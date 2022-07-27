// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import gcc.simd;

void main()
{
    ubyte[32] data;
    foreach (i; 0..data.length)
    {   
        data[i] = cast(ubyte)i;
    }

    // to test all alignments from 1 ~ 16
    foreach (i; 0..16)
    {
        ubyte* d = &data[i];

        void test(T)()
        {
            // load the data
            T v = loadUnaligned(cast(T*)d);

            // check that the data was loaded correctly
            ubyte* ptrToV = cast(ubyte*)&v;
            foreach (j; 0..T.sizeof)
                assert(ptrToV[j] == d[j]);
        }

        static if (__traits(compiles, __vector(void[16])))
            test!(__vector(void[16]))();
        static if (__traits(compiles, __vector(byte[16])))
            test!(__vector(byte[16]))();
        static if (__traits(compiles, __vector(ubyte[16])))
            test!(__vector(ubyte[16]))();
        static if (__traits(compiles, __vector(short[8])))
            test!(__vector(short[8]))();
        static if (__traits(compiles, __vector(ushort[8])))
            test!(__vector(ushort[8]))();
        static if (__traits(compiles, __vector(int[4])))
            test!(__vector(int[4]))();
        static if (__traits(compiles, __vector(uint[4])))
            test!(__vector(uint[4]))();
        static if (__traits(compiles, __vector(long[2])))
            test!(__vector(long[2]))();
        static if (__traits(compiles, __vector(ulong[2])))
            test!(__vector(ulong[2]))();
        static if (__traits(compiles, __vector(double[2])))
            test!(__vector(double[2]))();
        static if (__traits(compiles, __vector(float[4])))
            test!(__vector(float[4]))();
    }
}
