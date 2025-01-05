/**
 * A library bitfields utility
 *
 * Copyright: Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:   Dennis Korpel
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/common/bitfields.d, common/bitfields.d)
 * Documentation: https://dlang.org/phobos/dmd_common_bitfields.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/common/bitfields.d
 */
module dmd.common.bitfields;

/**
 * Generate code for bit fields inside a struct/class body
 * Params:
 *   S = type of a struct with only boolean fields, which should become bit fields
 *   T = type of bit fields variable, must have enough bits to store all booleans
 * Returns: D code with a bit fields variable and getter / setter functions
 */
extern (D) string generateBitFields(S, T)()
if (__traits(isUnsigned, T))
{
    import core.bitop: bsr;

    string result = "extern (C++) pure nothrow @nogc @safe final {";

    struct BitInfo
    {
        int[] offset;
        int[] size;
        T initialValue;
        int totalSize;
    }

    // Iterate over members to compute bit offset and bit size for each of them
    enum BitInfo bitInfo = () {
        BitInfo result;
        int bitOffset = 0;
        foreach (size_t i, mem; __traits(allMembers, S))
        {
            alias memType = typeof(__traits(getMember, S, mem));
            enum int bitSize = bsr(memType.max | 1) + 1;
            result.offset ~= bitOffset;
            result.size ~= bitSize;
            result.initialValue |= cast(T) __traits(getMember, S.init, mem) << bitOffset;
            bitOffset += bitSize;
        }
        result.totalSize = bitOffset;
        return result;
    } ();

    alias TP = typeof(T.init + 0u); // type that `T` gets promoted to, uint or ulong
    enum string toString(TP i) = i.stringof; // compile time 'integer to string'

    static assert(bitInfo.totalSize <= T.sizeof * 8,
        "sum of bit field size "~toString!(bitInfo.totalSize)~" exceeds storage type `"~T.stringof~"`");

    foreach (size_t i, mem; __traits(allMembers, S))
    {
        enum typeName = typeof(__traits(getMember, S, mem)).stringof;
        enum shift = toString!(bitInfo.offset[i]);
        enum sizeMask = toString!((1 << bitInfo.size[i]) - 1); // 0x01 for bool, 0xFF for ubyte etc.
        result ~= "
        "~typeName~" "~mem~"() const scope { return cast("~typeName~") ((bitFields >>> "~shift~") & "~sizeMask~"); }
        "~typeName~" "~mem~"("~typeName~" v) scope
        {
            bitFields &= ~("~sizeMask~" << "~shift~");
            bitFields |= v << "~shift~";
            return v;
        }";
    }
    enum TP initVal = bitInfo.initialValue;
    return result ~ "\n}\n private "~T.stringof~" bitFields = " ~ toString!(initVal) ~ ";\n";
}

///
unittest
{
    enum E
    {
        a, b, c,
    }

    static struct B
    {
        bool x;
        bool y;
        E e = E.c;
        bool z = 1;
        private ubyte w = 77;
    }

    static struct S
    {
        mixin(generateBitFields!(B, ushort));
    }

    S s;
    assert(!s.x);
    s.x = true;
    assert(s.x);
    s.x = false;
    assert(!s.x);

    s.y = true;
    assert(s.y);
    assert(!s.x);

    assert(s.e == E.c);
    s.e = E.a;
    assert(s.e == E.a);

    assert(s.z);
    assert(s.w == 77);
    s.w = 3;
    assert(s.w == 3);
}
