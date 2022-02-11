// REQUIRED_ARGS: -m64 -O -inline
// DISABLED: win32 linux32 freebsd32 osx32 netbsd32 dragonflybsd32
// https://issues.dlang.org/show_bug.cgi?id=21727

import core.simd;

@nogc nothrow pure @safe:

struct Float4
{
    float4 mVector;

    pragma(inline, false) ref typeof(this) doubleInPlace() return
    @nogc nothrow pure @safe
    {
        mVector = mVector + mVector;
        return this;
    }
}

pragma(inline, false) Float4 identity(Float4 a)
{
    return a;
}

pragma(inline, true) Float4 twoTimes(const ref Float4 a)
{
    version (D_SIMD)
        return Float4(cast(float4) __simd(XMM.ADDPS, a.mVector, a.mVector));
    else // Allow non-DMD compilers to compile this test.
        return Float4(a.mVector + a.mVector);
}

pragma(inline, false) Float4 fourTimes(const Float4 a)
{
    auto x = identity(a);
    auto y = x.doubleInPlace(); // This crashed in dmd.backend.cgxmm.xmmload.
    auto z = twoTimes(y);
    return z;
}

void main()
{
    const c = fourTimes(Float4([5,7,11,13]));
    assert(c.mVector.array == [20, 28, 44, 52]);
}
