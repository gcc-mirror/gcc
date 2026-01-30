/**
 * A library bitfields utility
 *
 * Copyright: Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:   Dennis Korpel
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/common/bitfields.d, common/bitfields.d)
 * Documentation: https://dlang.org/phobos/dmd_common_bitfields.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/common/bitfields.d
 */
module dmd.common.bitfields;

//version = Has_Bitfields; // does not work (yet) because hashOf doesn't work on bitfields
version(Has_Bitfields)
    version = Debugger_friendly; // without Has_Bitfields, this uses more space by using S

/**
 * Generate code for bit fields inside a struct/class body
 * Params:
 *   S = type of a struct with only boolean fields, which should become bit fields
 *   T = type of bit fields variable, must have enough bits to store all booleans
 *   field = if provided, assume it is declared and initialized elsewhere
 *   bitOff = start using bits at the given offset
 * Returns: D code with a bit fields variable and getter / setter functions
 */
extern (D) string generateBitFields(S, T, string field = "", int bitOff = 0, int ID = __LINE__)()
if (__traits(isUnsigned, T))
{
    import core.bitop: bsr;
    // if _fieldName provided, assume it declared and initialized elsewhere
    enum fieldName = field.length == 0 ? "bitFields" : field;
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
        int bitOffset = bitOff;
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

    version(Debugger_friendly)
    {
        // unique name needed to allow same name as in base class using `alias`, but without overloading
        string bitfieldsName = fieldName ~ toString!(ID);
        string bitfieldsRead = T.stringof~" "~bitfieldsName~"() const pure { return 0";
        string bitfieldsWrite = "void "~bitfieldsName~"("~T.stringof~" v) {\n";
    }

    foreach (size_t i, mem; __traits(allMembers, S))
    {
        enum typeName = typeof(__traits(getMember, S, mem)).stringof;
        enum shift = toString!(bitInfo.offset[i]);
        enum sizeMask = toString!((1 << bitInfo.size[i]) - 1); // 0x01 for bool, 0xFF for ubyte etc.
        version(Debugger_friendly)
        {
            string memacc = mem;
            bitfieldsRead ~= "\n| (cast("~T.stringof~")("~memacc~" & "~sizeMask~") << "~shift~")";
            bitfieldsWrite ~= memacc~" = cast("~typeName~")((v >> "~shift~") & "~sizeMask~");\n";
            result ~= typeName~" "~mem;
            version(Has_Bitfields)
                result ~= " : "~toString!(bitInfo.size[i]);
            enum meminit = __traits(getMember, S.init, mem);
            result ~= " = "~meminit.stringof~";\n";
        }
        else
        {
            result ~= "
                "~typeName~" "~mem~"() const scope { return cast("~typeName~") (("~fieldName~" >>> "~shift~") & "~sizeMask~"); }
            "~typeName~" "~mem~"("~typeName~" v) scope
            {
                "~fieldName~" &= ~("~sizeMask~" << "~shift~");
                "~fieldName~" |= v << "~shift~";
                return v;
            }";
        }
    }
    version(Debugger_friendly)
    {
        bitfieldsRead ~= ";\n}\n";
        bitfieldsWrite ~= "}\n";
        if (field.length == 0)
            result ~= "alias "~fieldName~" = "~bitfieldsName~";\n";
        result ~= bitfieldsRead ~ bitfieldsWrite;
        result ~= "\n}\n";
        return result;
    }
    else
    {
        result ~= "\n}\n";
        enum TP initVal = bitInfo.initialValue;
        if (field.length == 0)
            result ~= " private "~T.stringof~" "~fieldName~" = " ~ toString!(initVal) ~ ";\n";
        return result;
    }
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
