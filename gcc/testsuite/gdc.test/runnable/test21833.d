// REQUIRED_ARGS: -O -inline
// https://issues.dlang.org/show_bug.cgi?id=20855

void testit()
{
    pragma(inline, false);
    short[4] arr = [-1, 6, 0, 4];
    long1 A = *cast(long1*)(arr.ptr);
    assert(_mm_extract_pi16(A, 0) == 65535);
}

struct short4
{
    short[4] array;
}

struct long1
{
    long[1] array;
}

int _mm_extract_pi16 (long1 a, int imm8)
{
    return cast(ushort)((cast(short4)a).array[imm8]);
}

void main()
{
    testit();
}
