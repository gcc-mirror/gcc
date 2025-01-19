// https://issues.dlang.org/show_bug.cgi?id=20652
// EXTRA_CPP_SOURCES: test20652.cpp

import core.simd;

static if (!__traits(compiles, float4)) // No __vector support
{
    void main() {}
}
else
{
    extern(C++) void test20652(ref const float4);

    void main()
    {
        float4 f4 = 1;
        test20652(f4);
    }
}
