// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

import gcc.simd;

void main()
{
    static if (__traits(compiles, __vector(int[4])))
    {
        __gshared __vector(int[4]) a = [1,3,5,7];
        __gshared __vector(int[4]) b = [2,3,4,5];

        assert(equalMask(a, b).array == [0,-1,0,0]);
        assert(notEqualMask(a, b).array == [-1,0,-1,-1]);
        assert(greaterMask(a, b).array == [0,0,-1,-1]);
        assert(greaterOrEqualMask(a, b).array == [0,-1,-1,-1]);
    }
}
