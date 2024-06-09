module imports.test24390b;
static if (__traits(compiles, __vector(int[4])) && __traits(compiles, __vector(byte[16])))
{
    __vector(int[4]) _mm_set1_epi8 (byte a)
    {
        __vector(byte[16]) b = a;
        return cast(__vector(int[4]))b;
    }
}
