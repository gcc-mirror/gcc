// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

import gcc.simd;

void main()
{
    static if (__traits(compiles, __vector(int[4])))
    {
        __gshared __vector(int[4]) a = [1,0,-1,2];

        assert(notMask(a).array == [0,-1,0,0]);

        assert(andAndMask(a, 1).array == [-1,0,-1,-1]);
        assert(andAndMask(a, 0).array == [0,0,0,0]);

        assert(orOrMask(a, 1).array == [-1,-1,-1,-1]);
        assert(orOrMask(a, 0).array == [-1,0,-1,-1]);
    }
}
