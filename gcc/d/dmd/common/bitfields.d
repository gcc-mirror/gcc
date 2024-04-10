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
    string result = "extern (C++) pure nothrow @nogc @safe final {";
    enum structName = __traits(identifier, S);

    string initialValue = "";
    foreach (size_t i, mem; __traits(allMembers, S))
    {
        static assert(is(typeof(__traits(getMember, S, mem)) == bool));
        static assert(i < T.sizeof * 8, "too many fields for bit field storage of type `"~T.stringof~"`");
        enum mask = "(1 << "~i.stringof~")";
        result ~= "
        /// set or get the corresponding "~structName~" member
        bool "~mem~"() const scope { return !!(bitFields & "~mask~"); }
        /// ditto
        bool "~mem~"(bool v)
        {
            v ? (bitFields |= "~mask~") : (bitFields &= ~"~mask~");
            return v;
        }";

        initialValue = (__traits(getMember, S.init, mem) ? "1" : "0") ~ initialValue;
    }
    return result ~ "}\n private "~T.stringof~" bitFields = 0b" ~ initialValue ~ ";\n";
}

///
unittest
{
    static struct B
    {
        bool x;
        bool y;
        bool z = 1;
    }

    static struct S
    {
        mixin(generateBitFields!(B, ubyte));
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
    assert(s.z);
}
