// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import gcc.simd;
import gcc.attributes;

void main()
{
    static if (__traits(compiles, __vector(int[4])))
        alias int4 = __vector(int[4]);
    static if (__traits(compiles, __vector(int[8])))
        alias int8 = __vector(int[8]);

    static if (__traits(compiles, int4) && __traits(compiles, int8))
    {
        __gshared int4[5] res;
        __gshared int4 a;
        __gshared int4 b;
        __gshared int8[3] res8;
        __gshared int8 a8;
        __gshared int8 b8;

        @noipa static void foo()
        {
            res[0] = shufflevector(a, b, 0, 1, 4, 5);
            res[1] = shufflevector(a, b, 0, 1, 2, 5);
            res8[0] = shufflevector(a, b, 0, 1, 2, 2 + 1, 4, 5, 6, 7);
            res[2] = shufflevector(a8, b8, 0, 8, 1, 9);
            res[3] = shufflevector(a8, b, 0, 8, 1, 9);
            res[4] = shufflevector(a, b8, 0, 4, 1, 5);
            res8[1] = shufflevector(a8, b, 0, 8, 1, 9, 10, 11, 2, 3);
            res8[2] = shufflevector(a, b8, 0, 4, 1, 5, 4, 5, 6, 7);
        }

        a = [0, 1, 2, 3];
        b = [4, 5, 6, 7];
        a8 = [0, 1, 2, 3, 4, 5, 6, 7];
        b8 = [8, 9, 10, 11, 12, 13, 14, 15];
        foo();
        assert(res[0].array == [0, 1, 4, 5]);

        res[1][2] = 9;
        assert(res[1].array == [0, 1, 9, 5]);
        assert(res8[0].array == [0, 1, 2, 3, 4, 5, 6, 7]);
        assert(res[2].array == [0, 8, 1, 9]);
        assert(res[3].array == [0, 4, 1, 5]);
        assert(res[4].array == [0, 8, 1, 9]);
        assert(res8[1].array == [0, 4, 1, 5, 6, 7, 2, 3]);

        res8[2][4] = 42;
        res8[2][5] = 42;
        res8[2][6] = 42;
        res8[2][7] = 42;
        assert(res8[2].array == [0, 8, 1, 9, 42, 42, 42, 42]);
    }
}
