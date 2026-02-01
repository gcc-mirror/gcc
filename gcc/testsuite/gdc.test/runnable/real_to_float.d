/* The test related to https://github.com/dlang/dmd/issues/22322
 * The issue title:
 * "converting real to float uses double rounding for 64-bit code
 * causing unexpected results"
 */
pragma(inline, false)
void test(real r)
{
    assert(r == 0x1.000002fffffffcp-1);
    double d = r;
    assert(d == 0x1.000003p-1);
    float f = r;
    assert(f == 0x1.000002p-1);
    float fd = d;
    assert(fd == 0x1.000004p-1);
    real rd = d;
    assert(rd == 0x1.000003p-1);
    float frd = rd;
    assert(frd == 0x1.000004p-1);
}

void main()
{
    static if (real.sizeof > 8)
    {
        test(0x1.000002fffffffcp-1);
    }
}
