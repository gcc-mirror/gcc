/**
 * Defines an identifier, which is the name of a `Dsymbol`.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/identifier.d, _identifier.d)
 * Documentation:  https://dlang.org/phobos/dmd_identifier.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/identifier.d
 */

module dmd.identifier;

import core.stdc.ctype;
import core.stdc.stdio;
import core.stdc.string;
import dmd.id;
import dmd.location;
import dmd.common.outbuffer;
import dmd.rootobject;
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
    extern (D) this(const(char)[] name, int value) @safe
    {
        //printf("Identifier('%.*s', %d)\n", cast(int)name.length, name.ptr, value);
        this(name, value, false);
    }

    extern (D) private this(const(char)[] name, int value, bool isAnonymous) @safe
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
        else if (this == Id.dtor || this == Id.__xdtor || this == Id.__fieldDtor ||
            this == Id.__aggrDtor || this == Id.cppdtor || this == Id.ticppdtor)
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
        else if (this == Id.postblit)
            p = "this(this)";
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
     *      parent      = (optional) extra part to be used in uniqueness check,
     *                    if (prefix1, loc1) == (prefix2, loc2), but
     *                    parent1 != parent2, no new name will be generated.
     * Returns:
     *      Identifier (inside Identifier.idPool) with deterministic name based
     *      on the source location.
     */
    extern (D) static Identifier generateIdWithLoc(string prefix, Loc loc, string parent = "")
    {
        // generate `<prefix>_L<line>_C<col>`
        auto sl = SourceLoc(loc);
        OutBuffer idBuf;
        idBuf.writestring(prefix);
        idBuf.writestring("_L");
        idBuf.print(sl.line);
        idBuf.writestring("_C");
        idBuf.print(sl.column);

        /**
         * Make sure the identifiers are unique per filename, i.e., per module/mixin
         * (`path/to/foo.d` and `path/to/foo.d-mixin-<line>`). See issues
         * https://issues.dlang.org/show_bug.cgi?id=16995
         * https://issues.dlang.org/show_bug.cgi?id=18097
         * https://issues.dlang.org/show_bug.cgi?id=18111
         * https://issues.dlang.org/show_bug.cgi?id=18880
         * https://issues.dlang.org/show_bug.cgi?id=18868
         * https://issues.dlang.org/show_bug.cgi?id=19058
         *
         * It is a bit trickier for lambdas/dgliterals: we want them to be unique per
         * module/mixin + function/template instantiation context. So we use extra parent
         * argument for that when dealing with lambdas. We could have added it to prefix
         * directly, but that would unnecessary lengthen symbols names. See issue:
         * https://issues.dlang.org/show_bug.cgi?id=23722
         */
        static struct Key { string locKey; string prefix; string parent; }
        __gshared uint[Key] counters;

        string locKey = cast(string) (sl.filename ~ idBuf[]);
        static if (__traits(compiles, counters.update(Key.init, () => 0u, (ref uint a) => 0u)))
        {
            // 2.082+
            counters.update(Key(locKey, prefix, parent),
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
            const key = Key(locKey, prefix, parent);
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
    static Identifier idPool(scope const(char)* s, uint len)
    {
        return idPool(s[0 .. len]);
    }

    extern (D) static Identifier idPool(scope const(char)[] s, bool isAnonymous = false)
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
    extern (D) static void idPool(scope const(char)[] s, TOK value)
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
    extern (D) static bool isValidIdentifier(const(char)[] str) @trusted
    {
        import dmd.common.charactertables;

        if (str.length == 0 ||
            (str[0] >= '0' && str[0] <= '9')) // beware of isdigit() on signed chars
        {
            return false;
        }

        // In a previous implementation this was implemented quite naively,
        //  by utilizing the libc.
        // However we can do better, by copying the lexer approach to identifier validation.

        const(char)* p = &str[0], pEnd = str.ptr + str.length;

        // handle start characters
        {
            const c = *p;

            if (isidchar(c))
                p++;
            else if (c & 0x80)
            {
                size_t countDecoded;
                dchar decoded;

                if (utf_decodeChar(p[0 .. pEnd - p], countDecoded, decoded) is null ||
                    isAnyStart(decoded))
                    p += countDecoded;
                else
                    return false;
            }
            else
                return false;
        }

        // handle continue characters
        while(p !is pEnd)
        {
            const c = *p;

            if (isidchar(c)) // handles ASCII subset
            {
                p++;
                continue;
            }
            else if (c & 0x80)
            {
                size_t countDecoded;
                dchar decoded;

                if (utf_decodeChar(p[0 .. pEnd - p], countDecoded, decoded) is null ||
                    isAnyContinue(decoded))
                {
                    p += countDecoded;
                    continue;
                }
                else
                    return false;
            }
            else
                return false;
        }

        return true;
    }

    ///
    unittest
    {
        assert(Identifier.isValidIdentifier("tes123_t".ptr));
        assert(!Identifier.isValidIdentifier("tes123_^t".ptr));
        assert(Identifier.isValidIdentifier("te123s_ğt".ptr));
        assert(!Identifier.isValidIdentifier("t^e123s_ğt".ptr));
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
