// PERMUTE_ARGS: -O -inline
// EXTRA_SOURCES: imports/test24390a.d imports/test24390b.d
static if (__traits(compiles, __vector(int[4])) && __traits(compiles, __vector(byte[16])))
{
    import imports.test24390a;

    void main()
    {
        __vector(int[4]) mmA ;
        __vector(int[4]) mmB ;
        auto mask = _mm_cmpestrm(mmA, mmB);
    }

    __vector(int[4]) _mm_cmpestrm(__vector(int[4]) mmA, __vector(int[4]) mmB)
    {
        __vector(int[4]) R;
        for (int pos ; pos < 16; ++pos)
        {
            byte charK = (cast(__vector(byte[16]))mmA).array[pos];
            __vector(int[4]) eqMask = _mm_set1_epi8(charK);
            R = R & eqMask;

        }
        return R;
    }
}
