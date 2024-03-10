/**
 * Defines an identifier, which is the name of a `Dsymbol`.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/identifier.d, _identifier.d)
 * Documentation:  https://dlang.org/phobos/dmd_identifier.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/identifier.d
 */

module dmd.identifier;

import core.stdc.ctype;
import core.stdc.stdio;
import core.stdc.string;
import dmd.id;
import dmd.location;
import dmd.common.outbuffer;
import dmd.root.rootobject;
import dmd.root.string;
import dmd.root.stringtable;
import dmd.root.utf;
import dmd.tokens;


/***********************************************************
 */
extern (C++) final class Identifier : RootObject
{
    private const int value;

    // Indicates if this is an identifier used for an anonymous symbol.
    private const bool isAnonymous_ = false;

    private const char[] name;

nothrow:

    /// Construct an identifier from the given name.
    extern (D) this(const(char)* name)
    {
        //printf("Identifier('%s', %d)\n", name, value);
        this(name.toDString(), TOK.identifier);
    }

    /**
       Construct an identifier from the given name.

       Params:
         name = the identifier name. There must be `'\0'` at `name[length]`.
         length = the length of `name`, excluding the terminating `'\0'`
         value = Identifier value (e.g. `Id.unitTest`) or `TOK.identifier`
     */
    extern (D) this(const(char)* name, size_t length, int value)
    in
    {
        assert(name[length] == '\0');
    }
    do
    {
        //printf("Identifier('%s', %d)\n", name, value);
        this(name[0 .. length], value);
    }

    /// ditto
    extern (D) this(const(char)[] name, int value)
    {
        //printf("Identifier('%.*s', %d)\n", cast(int)name.length, name.ptr, value);
        this(name, value, false);
    }

    extern (D) private this(const(char)[] name, int value, bool isAnonymous)
    {
        //printf("Identifier('%.*s', %d, %d)\n", cast(int)name.length, name.ptr, value, isAnonymous);
        this.name = name;
        this.value = value;
        isAnonymous_ = isAnonymous;
    }

    static Identifier create(const(char)* name)
    {
        return new Identifier(name);
    }

    override const(char)* toChars() const pure
    {
        return name.ptr;
    }

    extern (D) override const(char)[] toString() const pure @safe
    {
        return name;
    }

    int getValue() const pure
    {
        return value;
    }

    bool isAnonymous() const pure @nogc @safe
    {
        return isAnonymous_;
    }

    const(char)* toHChars2() const
    {
        const(char)* p = null;
        if (this == Id.ctor)
            p = "this";
        else if (this == Id.dtor)
            p = "~this";
        else if (this == Id.unitTest)
            p = "unittest";
        else if (this == Id.dollar)
            p = "$";
        else if (this == Id.withSym)
            p = "with";
        else if (this == Id.result)
            p = "result";
        else if (this == Id.returnLabel)
            p = "return";
        else
        {
            p = toChars();
            if (*p == '_')
            {
                if (strncmp(p, "_staticCtor", 11) == 0)
                    p = "static this";
                else if (strncmp(p, "_staticDtor", 11) == 0)
                    p = "static ~this";
                else if (strncmp(p, "__invariant", 11) == 0)
                    p = "invariant";
            }
        }
        return p;
    }

    override DYNCAST dyncast() const
    {
        return DYNCAST.identifier;
    }

    private extern (D) __gshared StringTable!Identifier stringtable;

    /**
     * Generates a new identifier.
     *
     * Params:
     *  prefix = this will be the prefix of the name of the identifier. For debugging
     *      purpose.
     */
    extern(D) static Identifier generateId(const(char)[] prefix)
    {
        return generateId(prefix, newSuffix, false);
    }

    /**
     * Generates a new anonymous identifier.
     *
     * Params:
     *  name = this will be part of the name of the identifier. For debugging
     *      purpose.
     */
    extern(D) static Identifier generateAnonymousId(const(char)[] name)
    {
        return generateId("__anon" ~ name, newSuffix, true);
    }

    /**
     * Generates a new identifier.
     *
     * Params:
     *  prefix = this will be the prefix of the name of the identifier. For debugging
     *      purpose.
     *  suffix = this will be the suffix of the name of the identifier. This is
     *      what makes the identifier unique
     */
    extern(D) static Identifier generateId(const(char)[] prefix, size_t suffix)
    {
        return generateId(prefix, suffix, false);
    }

    /// ditto
    static Identifier generateId(const(char)* prefix, size_t length, size_t suffix)
    {
        return generateId(prefix[0 .. length], suffix);
    }

    // Generates a new, unique, suffix for an identifier.
    extern (D) private static size_t newSuffix()
    {
        __gshared size_t i;
        return ++i;
    }

    extern(D) private static Identifier generateId(const(char)[] prefix, size_t suffix, bool isAnonymous)
    {
        OutBuffer buf;
        buf.write(prefix);
        buf.print(suffix);
        return idPool(buf[], isAnonymous);
    }

    /***************************************
     * Generate deterministic named identifier based on a source location,
     * such that the name is consistent across multiple compilations.
     * A new unique name is generated. If the prefix+location is already in
     * the stringtable, an extra suffix is added (starting the count at "_1").
     *
     * Params:
     *      prefix      = first part of the identifier name.
     *      loc         = source location to use in the identifier name.
     * Returns:
     *      Identifier (inside Identifier.idPool) with deterministic name based
     *      on the source location.
     */
    extern (D) static Identifier generateIdWithLoc(string prefix, const ref Loc loc)
    {
        // generate `<prefix>_L<line>_C<col>`
        OutBuffer idBuf;
        idBuf.writestring(prefix);
        idBuf.writestring("_L");
        idBuf.print(loc.linnum);
        idBuf.writestring("_C");
        idBuf.print(loc.charnum);

        /**
         * Make sure the identifiers are unique per filename, i.e., per module/mixin
         * (`path/to/foo.d` and `path/to/foo.d-mixin-<line>`). See issues
         * https://issues.dlang.org/show_bug.cgi?id=16995
         * https://issues.dlang.org/show_bug.cgi?id=18097
         * https://issues.dlang.org/show_bug.cgi?id=18111
         * https://issues.dlang.org/show_bug.cgi?id=18880
         * https://issues.dlang.org/show_bug.cgi?id=18868
         * https://issues.dlang.org/show_bug.cgi?id=19058
         */
        static struct Key { Loc loc; string prefix; }
        __gshared uint[Key] counters;

        static if (__traits(compiles, counters.update(Key.init, () => 0u, (ref uint a) => 0u)))
        {
            // 2.082+
            counters.update(Key(loc, prefix),
                () => 1u,          // insertion
                (ref uint counter) // update
                {
                    idBuf.writestring("_");
                    idBuf.print(counter);
                    return counter + 1;
                }
            );
        }
        else
        {
            const key = Key(loc, prefix);
            if (auto pCounter = key in counters)
            {
                idBuf.writestring("_");
                idBuf.print((*pCounter)++);
            }
            else
                counters[key] = 1;
        }

        return idPool(idBuf[]);
    }

    /********************************************
     * Create an identifier in the string table.
     */
    static Identifier idPool(const(char)* s, uint len)
    {
        return idPool(s[0 .. len]);
    }

    extern (D) static Identifier idPool(const(char)[] s, bool isAnonymous = false)
    {
        auto sv = stringtable.update(s);
        auto id = sv.value;
        if (!id)
        {
            id = new Identifier(sv.toString(), TOK.identifier, isAnonymous);
            sv.value = id;
        }
        return id;
    }

    /******************************************
     * Used for inserting keywords into the string table.
     * Params:
     *  s = string for keyword
     *  value = TOK.xxxx for the keyword
     */
    extern (D) static void idPool(const(char)[] s, TOK value)
    {
        auto sv = stringtable.insert(s, null);
        assert(sv);
        auto id = new Identifier(sv.toString(), value);
        sv.value = id;
    }

    /**********************************
     * Determine if string is a valid Identifier.
     * Params:
     *      str = string to check
     * Returns:
     *      false for invalid
     */
    static bool isValidIdentifier(const(char)* str)
    {
        return str && isValidIdentifier(str.toDString);
    }

    /**********************************
     * ditto
     */
    extern (D) static bool isValidIdentifier(const(char)[] str)
    {
        if (str.length == 0 ||
            (str[0] >= '0' && str[0] <= '9')) // beware of isdigit() on signed chars
        {
            return false;
        }

        size_t idx = 0;
        while (idx < str.length)
        {
            dchar dc;
            const s = utf_decodeChar(str, idx, dc);
            if (s ||
                !((dc >= 0x80 && isUniAlpha(dc)) || isalnum(dc) || dc == '_'))
            {
                return false;
            }
        }
        return true;
    }

    extern (D) static Identifier lookup(const(char)* s, size_t len)
    {
        return lookup(s[0 .. len]);
    }

    extern (D) static Identifier lookup(const(char)[] s)
    {
        auto sv = stringtable.lookup(s);
        if (!sv)
            return null;
        return sv.value;
    }

    extern (D) static void initTable()
    {
        stringtable._init(28_000);
    }
}
