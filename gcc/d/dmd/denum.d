/**
 * Define `enum` declarations and `enum` members.
 *
 * Specification: $(LINK2 https://dlang.org/spec/enum.html, Enums)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/denum.d, _denum.d)
 * Documentation:  https://dlang.org/phobos/dmd_denum.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/denum.d
 * References:  https://dlang.org/spec/enum.html
 */

module dmd.denum;

import core.stdc.stdio;

import dmd.astenums;
import dmd.attrib;
import dmd.gluelayer;
import dmd.declaration;
import dmd.dsymbol;
import dmd.expression;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.visitor;

/***********************************************************
 * AST node for `EnumDeclaration`
 * https://dlang.org/spec/enum.html#EnumDeclaration
 */
extern (C++) final class EnumDeclaration : ScopeDsymbol
{
    /* The separate, and distinct, cases are:
     *  1. enum { ... }
     *  2. enum : memtype { ... }
     *  3. enum id { ... }
     *  4. enum id : memtype { ... }
     *  5. enum id : memtype;
     *  6. enum id;
     */
    Type type;              // the TypeEnum
    Type memtype;           // type of the members

    Visibility visibility;
    Expression maxval;
    Expression minval;
    Expression defaultval;  // default initializer

    // `bool` fields that are compacted into bit fields in a string mixin
    private extern (D) static struct BitFields
    {
        bool isdeprecated;
        bool added;
        bool inuse;
    }

    import dmd.common.bitfields : generateBitFields;
    mixin(generateBitFields!(BitFields, ubyte));

    Symbol* sinit;

    extern (D) this(const ref Loc loc, Identifier ident, Type memtype)
    {
        super(loc, ident);
        //printf("EnumDeclaration() %p %s : %s\n", this, toChars(), memtype.toChars());
        type = new TypeEnum(this);
        this.memtype = memtype;
        visibility = Visibility(Visibility.Kind.undefined);
    }

    override EnumDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto ed = new EnumDeclaration(loc, ident, memtype ? memtype.syntaxCopy() : null);
        ScopeDsymbol.syntaxCopy(ed);
        return ed;
    }

    override bool oneMember(out Dsymbol ps, Identifier ident)
    {
        if (isAnonymous())
            return Dsymbol.oneMembers(members, ps, ident);
        return Dsymbol.oneMember(ps, ident);
    }

    override Type getType()
    {
        return type;
    }

    override const(char)* kind() const
    {
        return "enum";
    }

    // is Dsymbol deprecated?
    override bool isDeprecated() const
    {
        return isdeprecated;
    }

    override Visibility visible() pure nothrow @nogc @safe
    {
        return visibility;
    }


    /****************
     * Determine if enum is a special one.
     * Returns:
     *  `true` if special
     */
    bool isSpecial() const nothrow @nogc
    {
        return isSpecialEnumIdent(ident) && memtype;
    }

    override inout(EnumDeclaration) isEnumDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * AST node representing a member of an enum.
 * https://dlang.org/spec/enum.html#EnumMember
 * https://dlang.org/spec/enum.html#AnonymousEnumMember
 */
extern (C++) final class EnumMember : VarDeclaration
{
    /* Can take the following forms:
     *  1. id
     *  2. id = value
     *  3. type id = value
     */
    @property ref value() { return (cast(ExpInitializer)_init).exp; }

    // A cast() is injected to 'value' after dsymbolSemantic(),
    // but 'origValue' will preserve the original value,
    // or previous value + 1 if none was specified.
    Expression origValue;

    Type origType;

    EnumDeclaration ed;

    extern (D) this(const ref Loc loc, Identifier id, Expression value, Type origType)
    {
        super(loc, null, id ? id : Id.empty, new ExpInitializer(loc, value));
        this.origValue = value;
        this.origType = origType;
    }

    extern(D) this(Loc loc, Identifier id, Expression value, Type memtype,
        StorageClass stc, UserAttributeDeclaration uad, DeprecatedDeclaration dd)
    {
        this(loc, id, value, memtype);
        storage_class = stc;
        userAttribDecl = uad;
        depdecl = dd;
    }

    override EnumMember syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new EnumMember(
            loc, ident,
            value ? value.syntaxCopy() : null,
            origType ? origType.syntaxCopy() : null,
            storage_class,
            userAttribDecl ? userAttribDecl.syntaxCopy(s) : null,
            depdecl ? depdecl.syntaxCopy(s) : null);
    }

    override const(char)* kind() const
    {
        return "enum member";
    }

    override inout(EnumMember) isEnumMember() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/******************************************
 * Check for special enum names.
 *
 * Special enum names are used by the C++ name mangler to represent
 * C++ types that are not basic D types.
 * Params:
 *      ident = identifier to check for specialness
 * Returns:
 *      `true` if it is special
 */
bool isSpecialEnumIdent(const Identifier ident) @nogc nothrow
{
    return  ident == Id.__c_long ||
            ident == Id.__c_ulong ||
            ident == Id.__c_longlong ||
            ident == Id.__c_ulonglong ||
            ident == Id.__c_long_double ||
            ident == Id.__c_wchar_t ||
            ident == Id.__c_complex_float ||
            ident == Id.__c_complex_double ||
            ident == Id.__c_complex_real;
}
