/**
 * Does name mangling for `extern(D)` symbols.
 *
 * Specification: $(LINK2 https://dlang.org/spec/abi.html#name_mangling, Name Mangling)
 *
 * Copyright: Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors: Walter Bright, http://www.digitalmars.com
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dmangle.d, _dmangle.d)
 * Documentation:  https://dlang.org/phobos/dmd_dmangle.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dmangle.d
 * References:  https://dlang.org/blog/2017/12/20/ds-newfangled-name-mangling/
 */

module dmd.dmangle;

import dmd.astenums;

/******************************************************************************
 * Returns exact mangled name of function.
 */
extern (C++) const(char)* mangleExact(FuncDeclaration fd)
{
    if (!fd.mangleString)
    {
        OutBuffer buf;
        scope Mangler v = new Mangler(&buf);
        v.mangleExact(fd);
        fd.mangleString = buf.extractChars();
    }
    return fd.mangleString;
}

extern (C++) void mangleToBuffer(Type t, OutBuffer* buf)
{
    if (t.deco)
        buf.writestring(t.deco);
    else
    {
        scope Mangler v = new Mangler(buf, t);
        v.visitWithMask(t, 0);
    }
}

extern (C++) void mangleToBuffer(Expression e, OutBuffer* buf)
{
    scope Mangler v = new Mangler(buf);
    e.accept(v);
}

extern (C++) void mangleToBuffer(Dsymbol s, OutBuffer* buf)
{
    scope Mangler v = new Mangler(buf);
    s.accept(v);
}

extern (C++) void mangleToBuffer(TemplateInstance ti, OutBuffer* buf)
{
    scope Mangler v = new Mangler(buf);
    v.mangleTemplateInstance(ti);
}

/// Returns: `true` if the given character is a valid mangled character
package bool isValidMangling(dchar c) nothrow
{
    return
        c >= 'A' && c <= 'Z' ||
        c >= 'a' && c <= 'z' ||
        c >= '0' && c <= '9' ||
        c != 0 && strchr("$%().:?@[]_", c) ||
        isUniAlpha(c);
}

// valid mangled characters
unittest
{
    assert('a'.isValidMangling);
    assert('B'.isValidMangling);
    assert('2'.isValidMangling);
    assert('@'.isValidMangling);
    assert('_'.isValidMangling);
}

// invalid mangled characters
unittest
{
    assert(!'-'.isValidMangling);
    assert(!0.isValidMangling);
    assert(!'/'.isValidMangling);
    assert(!'\\'.isValidMangling);
}

/**********************************************
 * Convert a string representing a type (the deco) and
 * return its equivalent Type.
 * Params:
 *      deco = string containing the deco
 * Returns:
 *      null for failed to convert
 *      Type for succeeded
 */

public Type decoToType(const(char)[] deco)
{
    //printf("decoToType(): %.*s\n", cast(int)deco.length, deco.ptr);
    if (auto sv = Type.stringtable.lookup(deco))
    {
        if (sv.value)
        {
            Type t = cast(Type)sv.value;
            assert(t.deco);
            return t;
        }
    }
    return null;
}


/***************************************** private ***************************************/

private:


import core.stdc.ctype;
import core.stdc.stdio;
import core.stdc.string;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.dclass;
import dmd.declaration;
import dmd.dmodule;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.mtype;
import dmd.root.ctfloat;
import dmd.root.outbuffer;
import dmd.root.aav;
import dmd.root.string;
import dmd.root.stringtable;
import dmd.target;
import dmd.tokens;
import dmd.utf;
import dmd.visitor;

private immutable char[TMAX] mangleChar =
[
    Tchar        : 'a',
    Tbool        : 'b',
    Tcomplex80   : 'c',
    Tfloat64     : 'd',
    Tfloat80     : 'e',
    Tfloat32     : 'f',
    Tint8        : 'g',
    Tuns8        : 'h',
    Tint32       : 'i',
    Timaginary80 : 'j',
    Tuns32       : 'k',
    Tint64       : 'l',
    Tuns64       : 'm',
    Tnull        : 'n',
    Timaginary32 : 'o',
    Timaginary64 : 'p',
    Tcomplex32   : 'q',
    Tcomplex64   : 'r',
    Tint16       : 's',
    Tuns16       : 't',
    Twchar       : 'u',
    Tvoid        : 'v',
    Tdchar       : 'w',
    //              x   // const
    //              y   // immutable
    Tint128      : 'z', // zi
    Tuns128      : 'z', // zk

    Tarray       : 'A',
    Ttuple       : 'B',
    Tclass       : 'C',
    Tdelegate    : 'D',
    Tenum        : 'E',
    Tfunction    : 'F', // D function
    Tsarray      : 'G',
    Taarray      : 'H',
    //              I   // in
    //              J   // out
    //              K   // ref
    //              L   // lazy
    //              M   // has this, or scope
    //              N   // Nh:vector Ng:wild Nn:noreturn
    //              O   // shared
    Tpointer     : 'P',
    //              Q   // Type/symbol/identifier backward reference
    Treference   : 'R',
    Tstruct      : 'S',
    //              T   // Ttypedef
    //              U   // C function
    //              W   // Windows function
    //              X   // variadic T t...)
    //              Y   // variadic T t,...)
    //              Z   // not variadic, end of parameters

    // '@' shouldn't appear anywhere in the deco'd names
    Tnone        : '@',
    Tident       : '@',
    Tinstance    : '@',
    Terror       : '@',
    Ttypeof      : '@',
    Tslice       : '@',
    Treturn      : '@',
    Tvector      : '@',
    Ttraits      : '@',
    Tmixin       : '@',
    Ttag         : '@',
    Tnoreturn    : '@',         // becomes 'Nn'
];

unittest
{
    foreach (i, mangle; mangleChar)
    {
        if (mangle == char.init)
        {
            fprintf(stderr, "ty = %u\n", cast(uint)i);
            assert(0);
        }
    }
}

/***********************
 * Mangle basic type ty to buf.
 */

private void tyToDecoBuffer(OutBuffer* buf, int ty)
{
    const c = mangleChar[ty];
    buf.writeByte(c);
    if (c == 'z')
        buf.writeByte(ty == Tint128 ? 'i' : 'k');
}

/*********************************
 * Mangling for mod.
 */
private void MODtoDecoBuffer(OutBuffer* buf, MOD mod)
{
    switch (mod)
    {
    case 0:
        break;
    case MODFlags.const_:
        buf.writeByte('x');
        break;
    case MODFlags.immutable_:
        buf.writeByte('y');
        break;
    case MODFlags.shared_:
        buf.writeByte('O');
        break;
    case MODFlags.shared_ | MODFlags.const_:
        buf.writestring("Ox");
        break;
    case MODFlags.wild:
        buf.writestring("Ng");
        break;
    case MODFlags.wildconst:
        buf.writestring("Ngx");
        break;
    case MODFlags.shared_ | MODFlags.wild:
        buf.writestring("ONg");
        break;
    case MODFlags.shared_ | MODFlags.wildconst:
        buf.writestring("ONgx");
        break;
    default:
        assert(0);
    }
}

private extern (C++) final class Mangler : Visitor
{
    alias visit = Visitor.visit;
public:
    static assert(Key.sizeof == size_t.sizeof);
    AssocArray!(Type, size_t) types;        // Type => (offset+1) in buf
    AssocArray!(Identifier, size_t) idents; // Identifier => (offset+1) in buf
    OutBuffer* buf;
    Type rootType;

    extern (D) this(OutBuffer* buf, Type rootType = null)
    {
        this.buf = buf;
        this.rootType = rootType;
    }

    /**
    * writes a back reference with the relative position encoded with base 26
    *  using upper case letters for all digits but the last digit which uses
    *  a lower case letter.
    * The decoder has to look up the referenced position to determine
    *  whether the back reference is an identifier (starts with a digit)
    *  or a type (starts with a letter).
    *
    * Params:
    *  pos           = relative position to encode
    */
    void writeBackRef(size_t pos)
    {
        buf.writeByte('Q');
        enum base = 26;
        size_t mul = 1;
        while (pos >= mul * base)
            mul *= base;
        while (mul >= base)
        {
            auto dig = cast(ubyte)(pos / mul);
            buf.writeByte('A' + dig);
            pos -= dig * mul;
            mul /= base;
        }
        buf.writeByte('a' + cast(ubyte)pos);
    }

    /**
    * Back references a non-basic type
    *
    * The encoded mangling is
    *       'Q' <relative position of first occurrence of type>
    *
    * Params:
    *  t = the type to encode via back referencing
    *
    * Returns:
    *  true if the type was found. A back reference has been encoded.
    *  false if the type was not found. The current position is saved for later back references.
    */
    bool backrefType(Type t)
    {
        if (t.isTypeBasic())
            return false;

        /**
         * https://issues.dlang.org/show_bug.cgi?id=21591
         *
         * Special case for unmerged TypeFunctions: use the generic merged
         * function type as backref cache key to avoid missed backrefs.
         *
         * Merging is based on mangling, so we need to avoid an infinite
         * recursion by excluding the case where `t` is the root type passed to
         * `mangleToBuffer()`.
         */
        if (t != rootType)
        {
            if (t.isFunction_Delegate_PtrToFunction())
            {
                t = t.merge2();
            }
        }

        return backrefImpl(types, t);
    }

    /**
    * Back references a single identifier
    *
    * The encoded mangling is
    *       'Q' <relative position of first occurrence of type>
    *
    * Params:
    *  id = the identifier to encode via back referencing
    *
    * Returns:
    *  true if the identifier was found. A back reference has been encoded.
    *  false if the identifier was not found. The current position is saved for later back references.
    */
    bool backrefIdentifier(Identifier id)
    {
        return backrefImpl(idents, id);
    }

    private extern(D) bool backrefImpl(T)(ref AssocArray!(T, size_t) aa, T key)
    {
        auto p = aa.getLvalue(key);
        if (*p)
        {
            const offset = *p - 1;
            writeBackRef(buf.length - offset);
            return true;
        }
        *p = buf.length + 1;
        return false;
    }

    void mangleSymbol(Dsymbol s)
    {
        s.accept(this);
    }

    void mangleType(Type t)
    {
        if (!backrefType(t))
            t.accept(this);
    }

    void mangleIdentifier(Identifier id, Dsymbol s)
    {
        if (!backrefIdentifier(id))
            toBuffer(id.toString(), s);
    }

    ////////////////////////////////////////////////////////////////////////////
    /**************************************************
     * Type mangling
     */
    void visitWithMask(Type t, ubyte modMask)
    {
        if (modMask != t.mod)
        {
            MODtoDecoBuffer(buf, t.mod);
        }
        mangleType(t);
    }

    override void visit(Type t)
    {
        tyToDecoBuffer(buf, t.ty);
    }

    override void visit(TypeNext t)
    {
        visit(cast(Type)t);
        visitWithMask(t.next, t.mod);
    }

    override void visit(TypeVector t)
    {
        buf.writestring("Nh");
        visitWithMask(t.basetype, t.mod);
    }

    override void visit(TypeSArray t)
    {
        visit(cast(Type)t);
        if (t.dim)
            buf.print(t.dim.toInteger());
        if (t.next)
            visitWithMask(t.next, t.mod);
    }

    override void visit(TypeDArray t)
    {
        visit(cast(Type)t);
        if (t.next)
            visitWithMask(t.next, t.mod);
    }

    override void visit(TypeAArray t)
    {
        visit(cast(Type)t);
        visitWithMask(t.index, 0);
        visitWithMask(t.next, t.mod);
    }

    override void visit(TypeFunction t)
    {
        //printf("TypeFunction.toDecoBuffer() t = %p %s\n", t, t.toChars());
        //static int nest; if (++nest == 50) *(char*)0=0;
        mangleFuncType(t, t, t.mod, t.next);
    }

    void mangleFuncType(TypeFunction t, TypeFunction ta, ubyte modMask, Type tret)
    {
        //printf("mangleFuncType() %s\n", t.toChars());
        if (t.inuse && tret)
        {
            // printf("TypeFunction.mangleFuncType() t = %s inuse\n", t.toChars());
            t.inuse = 2; // flag error to caller
            return;
        }
        t.inuse++;
        if (modMask != t.mod)
            MODtoDecoBuffer(buf, t.mod);

        char mc;
        final switch (t.linkage)
        {
        case LINK.default_:
        case LINK.system:
        case LINK.d:
            mc = 'F';
            break;
        case LINK.c:
            mc = 'U';
            break;
        case LINK.windows:
            mc = 'W';
            break;
        case LINK.cpp:
            mc = 'R';
            break;
        case LINK.objc:
            mc = 'Y';
            break;
        }
        buf.writeByte(mc);

        if (ta.purity)
            buf.writestring("Na");
        if (ta.isnothrow)
            buf.writestring("Nb");
        if (ta.isref)
            buf.writestring("Nc");
        if (ta.isproperty)
            buf.writestring("Nd");
        if (ta.isnogc)
            buf.writestring("Ni");

        if (ta.isreturn && !ta.isreturninferred)
            buf.writestring("Nj");
        else if (ta.isScopeQual && !ta.isscopeinferred)
            buf.writestring("Nl");

        if (ta.islive)
            buf.writestring("Nm");

        switch (ta.trust)
        {
            case TRUST.trusted:
                buf.writestring("Ne");
                break;
            case TRUST.safe:
                buf.writestring("Nf");
                break;
            default:
                break;
        }

        // Write argument types
        foreach (idx, param; t.parameterList)
            param.accept(this);
        //if (buf.data[buf.length - 1] == '@') assert(0);
        buf.writeByte('Z' - t.parameterList.varargs); // mark end of arg list
        if (tret !is null)
            visitWithMask(tret, 0);
        t.inuse--;
    }

    override void visit(TypeIdentifier t)
    {
        visit(cast(Type)t);
        auto name = t.ident.toString();
        buf.print(cast(int)name.length);
        buf.writestring(name);
    }

    override void visit(TypeEnum t)
    {
        visit(cast(Type)t);
        mangleSymbol(t.sym);
    }

    override void visit(TypeStruct t)
    {
        //printf("TypeStruct.toDecoBuffer('%s') = '%s'\n", t.toChars(), name);
        visit(cast(Type)t);
        mangleSymbol(t.sym);
    }

    override void visit(TypeClass t)
    {
        //printf("TypeClass.toDecoBuffer('%s' mod=%x) = '%s'\n", t.toChars(), mod, name);
        visit(cast(Type)t);
        mangleSymbol(t.sym);
    }

    override void visit(TypeTuple t)
    {
        //printf("TypeTuple.toDecoBuffer() t = %p, %s\n", t, t.toChars());
        visit(cast(Type)t);
        Parameter._foreach(t.arguments, (idx, param) {
                param.accept(this);
                return 0;
        });
        buf.writeByte('Z');
    }

    override void visit(TypeNull t)
    {
        visit(cast(Type)t);
    }

    override void visit(TypeNoreturn t)
    {
        buf.writestring("Nn");
    }

    ////////////////////////////////////////////////////////////////////////////
    void mangleDecl(Declaration sthis)
    {
        mangleParent(sthis);
        assert(sthis.ident);
        mangleIdentifier(sthis.ident, sthis);
        if (FuncDeclaration fd = sthis.isFuncDeclaration())
        {
            mangleFunc(fd, false);
        }
        else if (sthis.type)
        {
            visitWithMask(sthis.type, 0);
        }
        else
            assert(0);
    }

    void mangleParent(Dsymbol s)
    {
        //printf("mangleParent() %s %s\n", s.kind(), s.toChars());
        Dsymbol p;
        if (TemplateInstance ti = s.isTemplateInstance())
            p = ti.isTemplateMixin() ? ti.parent : ti.tempdecl.parent;
        else
            p = s.parent;
        if (p)
        {
            uint localNum = s.localNum;
            mangleParent(p);
            auto ti = p.isTemplateInstance();
            if (ti && !ti.isTemplateMixin())
            {
                localNum = ti.tempdecl.localNum;
                mangleTemplateInstance(ti);
            }
            else if (p.getIdent())
            {
                mangleIdentifier(p.ident, s);
                if (FuncDeclaration f = p.isFuncDeclaration())
                    mangleFunc(f, true);
            }
            else
                buf.writeByte('0');

            /* There can be multiple different declarations in the same
             * function that have the same mangled name.
             * This results in localNum having a non-zero number, which
             * is used to add a fake parent of the form `__Sddd` to make
             * the mangled names unique.
             * https://issues.dlang.org/show_bug.cgi?id=20565
             */
            if (localNum)
            {
                uint ndigits = 1;
                auto n = localNum;
                while (n >= 10)
                {
                    n /= 10;
                    ++ndigits;
                }
                buf.printf("%u__S%u", ndigits + 3, localNum);
            }
        }
    }

    void mangleFunc(FuncDeclaration fd, bool inParent)
    {
        //printf("deco = '%s'\n", fd.type.deco ? fd.type.deco : "null");
        //printf("fd.type = %s\n", fd.type.toChars());
        if (fd.needThis() || fd.isNested())
            buf.writeByte('M');

        if (!fd.type || fd.type.ty == Terror)
        {
            // never should have gotten here, but could be the result of
            // failed speculative compilation
            buf.writestring("9__error__FZ");

            //printf("[%s] %s no type\n", fd.loc.toChars(), fd.toChars());
            //assert(0); // don't mangle function until semantic3 done.
        }
        else if (inParent)
        {
            TypeFunction tf = fd.type.isTypeFunction();
            TypeFunction tfo = fd.originalType.isTypeFunction();
            mangleFuncType(tf, tfo, 0, null);
        }
        else
        {
            visitWithMask(fd.type, 0);
        }
    }

    /************************************************************
     * Write length prefixed string to buf.
     */
    extern (D) void toBuffer(const(char)[] id, Dsymbol s)
    {
        const len = id.length;
        if (buf.length + len >= 8 * 1024 * 1024) // 8 megs ought be enough for anyone
            s.error("excessive length %llu for symbol, possible recursive expansion?", cast(ulong)(buf.length + len));
        else
        {
            buf.print(len);
            buf.writestring(id);
        }
    }

    /************************************************************
     * Try to obtain an externally mangled identifier from a declaration.
     * If the declaration is at global scope or mixed in at global scope,
     * the user might want to call it externally, so an externally mangled
     * name is returned. Member functions or nested functions can't be called
     * externally in C, so in that case null is returned. C++ does support
     * namespaces, so extern(C++) always gives a C++ mangled name.
     *
     * See also: https://issues.dlang.org/show_bug.cgi?id=20012
     *
     * Params:
     *     d = declaration to mangle
     *
     * Returns:
     *     an externally mangled name or null if the declaration cannot be called externally
     */
    extern (D) static const(char)[] externallyMangledIdentifier(Declaration d)
    {
        const par = d.toParent(); //toParent() skips over mixin templates
        if (!par || par.isModule() || d.linkage == LINK.cpp)
        {
            if (d.linkage != LINK.d && d.localNum)
                d.error("the same declaration cannot be in multiple scopes with non-D linkage");
            final switch (d.linkage)
            {
                case LINK.d:
                    break;
                case LINK.c:
                case LINK.windows:
                case LINK.objc:
                    return d.ident.toString();
                case LINK.cpp:
                {
                    const p = target.cpp.toMangle(d);
                    return p.toDString();
                }
                case LINK.default_:
                case LINK.system:
                    d.error("forward declaration");
                    return d.ident.toString();
            }
        }
        return null;
    }

    override void visit(Declaration d)
    {
        //printf("Declaration.mangle(this = %p, '%s', parent = '%s', linkage = %d)\n",
        //        d, d.toChars(), d.parent ? d.parent.toChars() : "null", d.linkage);
        if (const id = externallyMangledIdentifier(d))
        {
            buf.writestring(id);
            return;
        }
        buf.writestring("_D");
        mangleDecl(d);
        debug
        {
            const slice = (*buf)[];
            assert(slice.length);
            for (size_t pos; pos < slice.length; )
            {
                dchar c;
                auto ppos = pos;
                const s = utf_decodeChar(slice, pos, c);
                assert(s is null, s);
                assert(c.isValidMangling, "The mangled name '" ~ slice ~ "' " ~
                    "contains an invalid character: " ~ slice[ppos..pos]);
            }
        }
    }

    /******************************************************************************
     * Normally FuncDeclaration and FuncAliasDeclaration have overloads.
     * If and only if there is no overloads, mangle() could return
     * exact mangled name.
     *
     *      module test;
     *      void foo(long) {}           // _D4test3fooFlZv
     *      void foo(string) {}         // _D4test3fooFAyaZv
     *
     *      // from FuncDeclaration.mangle().
     *      pragma(msg, foo.mangleof);  // prints unexact mangled name "4test3foo"
     *                                  // by calling Dsymbol.mangle()
     *
     *      // from FuncAliasDeclaration.mangle()
     *      pragma(msg, __traits(getOverloads, test, "foo")[0].mangleof);  // "_D4test3fooFlZv"
     *      pragma(msg, __traits(getOverloads, test, "foo")[1].mangleof);  // "_D4test3fooFAyaZv"
     *
     * If a function has no overloads, .mangleof property still returns exact mangled name.
     *
     *      void bar() {}
     *      pragma(msg, bar.mangleof);  // still prints "_D4test3barFZv"
     *                                  // by calling FuncDeclaration.mangleExact().
     */
    override void visit(FuncDeclaration fd)
    {
        if (fd.isUnique())
            mangleExact(fd);
        else
            visit(cast(Dsymbol)fd);
    }

    // ditto
    override void visit(FuncAliasDeclaration fd)
    {
        FuncDeclaration f = fd.toAliasFunc();
        FuncAliasDeclaration fa = f.isFuncAliasDeclaration();
        if (!fd.hasOverloads && !fa)
        {
            mangleExact(f);
            return;
        }
        if (fa)
        {
            mangleSymbol(fa);
            return;
        }
        visit(cast(Dsymbol)fd);
    }

    override void visit(OverDeclaration od)
    {
        if (od.overnext)
        {
            visit(cast(Dsymbol)od);
            return;
        }
        if (FuncDeclaration fd = od.aliassym.isFuncDeclaration())
        {
            if (fd.isUnique())
            {
                mangleExact(fd);
                return;
            }
        }
        if (TemplateDeclaration td = od.aliassym.isTemplateDeclaration())
        {
            if (td.overnext is null)
            {
                mangleSymbol(td);
                return;
            }
        }
        visit(cast(Dsymbol)od);
    }

    void mangleExact(FuncDeclaration fd)
    {
        assert(!fd.isFuncAliasDeclaration());
        if (fd.mangleOverride)
        {
            buf.writestring(fd.mangleOverride);
            return;
        }
        if (fd.isMain())
        {
            buf.writestring("_Dmain");
            return;
        }
        if (fd.isWinMain() || fd.isDllMain())
        {
            buf.writestring(fd.ident.toString());
            return;
        }
        visit(cast(Declaration)fd);
    }

    override void visit(VarDeclaration vd)
    {
        if (vd.mangleOverride)
        {
            buf.writestring(vd.mangleOverride);
            return;
        }
        visit(cast(Declaration)vd);
    }

    override void visit(AggregateDeclaration ad)
    {
        ClassDeclaration cd = ad.isClassDeclaration();
        Dsymbol parentsave = ad.parent;
        if (cd)
        {
            /* These are reserved to the compiler, so keep simple
             * names for them.
             */
            if (cd.ident == Id.Exception && cd.parent.ident == Id.object || cd.ident == Id.TypeInfo || cd.ident == Id.TypeInfo_Struct || cd.ident == Id.TypeInfo_Class || cd.ident == Id.TypeInfo_Tuple || cd == ClassDeclaration.object || cd == Type.typeinfoclass || cd == Module.moduleinfo || strncmp(cd.ident.toChars(), "TypeInfo_", 9) == 0)
            {
                // Don't mangle parent
                ad.parent = null;
            }
        }
        visit(cast(Dsymbol)ad);
        ad.parent = parentsave;
    }

    override void visit(TemplateInstance ti)
    {
        version (none)
        {
            printf("TemplateInstance.mangle() %p %s", ti, ti.toChars());
            if (ti.parent)
                printf("  parent = %s %s", ti.parent.kind(), ti.parent.toChars());
            printf("\n");
        }
        if (!ti.tempdecl)
            ti.error("is not defined");
        else
            mangleParent(ti);

        if (ti.isTemplateMixin() && ti.ident)
            mangleIdentifier(ti.ident, ti);
        else
            mangleTemplateInstance(ti);
    }

    void mangleTemplateInstance(TemplateInstance ti)
    {
        TemplateDeclaration tempdecl = ti.tempdecl.isTemplateDeclaration();
        assert(tempdecl);

        // Use "__U" for the symbols declared inside template constraint.
        const char T = ti.members ? 'T' : 'U';
        buf.printf("__%c", T);
        mangleIdentifier(tempdecl.ident, tempdecl);

        auto args = ti.tiargs;
        size_t nparams = tempdecl.parameters.dim - (tempdecl.isVariadic() ? 1 : 0);
        for (size_t i = 0; i < args.dim; i++)
        {
            auto o = (*args)[i];
            Type ta = isType(o);
            Expression ea = isExpression(o);
            Dsymbol sa = isDsymbol(o);
            Tuple va = isTuple(o);
            //printf("\to [%d] %p ta %p ea %p sa %p va %p\n", i, o, ta, ea, sa, va);
            if (i < nparams && (*tempdecl.parameters)[i].specialization())
                buf.writeByte('H'); // https://issues.dlang.org/show_bug.cgi?id=6574
            if (ta)
            {
                buf.writeByte('T');
                visitWithMask(ta, 0);
            }
            else if (ea)
            {
                // Don't interpret it yet, it might actually be an alias template parameter.
                // Only constfold manifest constants, not const/immutable lvalues, see https://issues.dlang.org/show_bug.cgi?id=17339.
                enum keepLvalue = true;
                ea = ea.optimize(WANTvalue, keepLvalue);
                if (auto ev = ea.isVarExp())
                {
                    sa = ev.var;
                    ea = null;
                    goto Lsa;
                }
                if (auto et = ea.isThisExp())
                {
                    sa = et.var;
                    ea = null;
                    goto Lsa;
                }
                if (auto ef = ea.isFuncExp())
                {
                    if (ef.td)
                        sa = ef.td;
                    else
                        sa = ef.fd;
                    ea = null;
                    goto Lsa;
                }
                buf.writeByte('V');
                if (ea.op == TOK.tuple)
                {
                    ea.error("tuple is not a valid template value argument");
                    continue;
                }
                // Now that we know it is not an alias, we MUST obtain a value
                uint olderr = global.errors;
                ea = ea.ctfeInterpret();
                if (ea.op == TOK.error || olderr != global.errors)
                    continue;

                /* Use type mangling that matches what it would be for a function parameter
                */
                visitWithMask(ea.type, 0);
                ea.accept(this);
            }
            else if (sa)
            {
            Lsa:
                sa = sa.toAlias();
                if (sa.isDeclaration() && !sa.isOverDeclaration())
                {
                    Declaration d = sa.isDeclaration();

                    if (auto fad = d.isFuncAliasDeclaration())
                        d = fad.toAliasFunc();
                    if (d.mangleOverride)
                    {
                        buf.writeByte('X');
                        toBuffer(d.mangleOverride, d);
                        continue;
                    }
                    if (const id = externallyMangledIdentifier(d))
                    {
                        buf.writeByte('X');
                        toBuffer(id, d);
                        continue;
                    }
                    if (!d.type || !d.type.deco)
                    {
                        ti.error("forward reference of %s `%s`", d.kind(), d.toChars());
                        continue;
                    }
                }
                buf.writeByte('S');
                mangleSymbol(sa);
            }
            else if (va)
            {
                assert(i + 1 == args.dim); // must be last one
                args = &va.objects;
                i = -cast(size_t)1;
            }
            else
                assert(0);
        }
        buf.writeByte('Z');
    }

    override void visit(Dsymbol s)
    {
        version (none)
        {
            printf("Dsymbol.mangle() '%s'", s.toChars());
            if (s.parent)
                printf("  parent = %s %s", s.parent.kind(), s.parent.toChars());
            printf("\n");
        }
        mangleParent(s);
        if (s.ident)
            mangleIdentifier(s.ident, s);
        else
            toBuffer(s.toString(), s);
        //printf("Dsymbol.mangle() %s = %s\n", s.toChars(), id);
    }

    ////////////////////////////////////////////////////////////////////////////
    override void visit(Expression e)
    {
        e.error("expression `%s` is not a valid template value argument", e.toChars());
    }

    override void visit(IntegerExp e)
    {
        const v = e.toInteger();
        if (cast(sinteger_t)v < 0)
        {
            buf.writeByte('N');
            buf.print(-v);
        }
        else
        {
            buf.writeByte('i');
            buf.print(v);
        }
    }

    override void visit(RealExp e)
    {
        buf.writeByte('e');
        realToMangleBuffer(e.value);
    }

    void realToMangleBuffer(real_t value)
    {
        /* Rely on %A to get portable mangling.
         * Must munge result to get only identifier characters.
         *
         * Possible values from %A  => mangled result
         * NAN                      => NAN
         * -INF                     => NINF
         * INF                      => INF
         * -0X1.1BC18BA997B95P+79   => N11BC18BA997B95P79
         * 0X1.9P+2                 => 19P2
         */
        if (CTFloat.isNaN(value))
        {
            buf.writestring("NAN"); // no -NAN bugs
            return;
        }

        if (value < CTFloat.zero)
        {
            buf.writeByte('N');
            value = -value;
        }

        if (CTFloat.isInfinity(value))
        {
            buf.writestring("INF");
            return;
        }

        char[36] buffer = void;
        // 'A' format yields [-]0xh.hhhhp+-d
        const n = CTFloat.sprint(buffer.ptr, 'A', value);
        assert(n < buffer.length);
        foreach (const c; buffer[2 .. n])
        {
            switch (c)
            {
                case '-':
                    buf.writeByte('N');
                    break;

                case '+':
                case '.':
                    break;

                default:
                    buf.writeByte(c);
                    break;
            }
        }
    }

    override void visit(ComplexExp e)
    {
        buf.writeByte('c');
        realToMangleBuffer(e.toReal());
        buf.writeByte('c'); // separate the two
        realToMangleBuffer(e.toImaginary());
    }

    override void visit(NullExp e)
    {
        buf.writeByte('n');
    }

    override void visit(StringExp e)
    {
        char m;
        OutBuffer tmp;
        const(char)[] q;
        /* Write string in UTF-8 format
         */
        switch (e.sz)
        {
        case 1:
            m = 'a';
            q = e.peekString();
            break;
        case 2:
        {
            m = 'w';
            const slice = e.peekWstring();
            for (size_t u = 0; u < e.len;)
            {
                dchar c;
                if (const s = utf_decodeWchar(slice, u, c))
                    e.error("%.*s", cast(int)s.length, s.ptr);
                else
                    tmp.writeUTF8(c);
            }
            q = tmp[];
            break;
        }
        case 4:
        {
            m = 'd';
            const slice = e.peekDstring();
            foreach (c; slice)
            {
                if (!utf_isValidDchar(c))
                    e.error("invalid UCS-32 char \\U%08x", c);
                else
                    tmp.writeUTF8(c);
            }
            q = tmp[];
            break;
        }

        default:
            assert(0);
        }
        buf.reserve(1 + 11 + 2 * q.length);
        buf.writeByte(m);
        buf.print(q.length);
        buf.writeByte('_');    // nbytes <= 11
        auto slice = buf.allocate(2 * q.length);
        foreach (i, c; q)
        {
            char hi = (c >> 4) & 0xF;
            slice[i * 2] = cast(char)(hi < 10 ? hi + '0' : hi - 10 + 'a');
            char lo = c & 0xF;
            slice[i * 2 + 1] = cast(char)(lo < 10 ? lo + '0' : lo - 10 + 'a');
        }
    }

    override void visit(ArrayLiteralExp e)
    {
        const dim = e.elements ? e.elements.dim : 0;
        buf.writeByte('A');
        buf.print(dim);
        foreach (i; 0 .. dim)
        {
            e[i].accept(this);
        }
    }

    override void visit(AssocArrayLiteralExp e)
    {
        const dim = e.keys.dim;
        buf.writeByte('A');
        buf.print(dim);
        foreach (i; 0 .. dim)
        {
            (*e.keys)[i].accept(this);
            (*e.values)[i].accept(this);
        }
    }

    override void visit(StructLiteralExp e)
    {
        const dim = e.elements ? e.elements.dim : 0;
        buf.writeByte('S');
        buf.print(dim);
        foreach (i; 0 .. dim)
        {
            Expression ex = (*e.elements)[i];
            if (ex)
                ex.accept(this);
            else
                buf.writeByte('v'); // 'v' for void
        }
    }

    override void visit(FuncExp e)
    {
        buf.writeByte('f');
        if (e.td)
            mangleSymbol(e.td);
        else
            mangleSymbol(e.fd);
    }

    ////////////////////////////////////////////////////////////////////////////

    override void visit(Parameter p)
    {
        if (p.storageClass & STC.scope_ && !(p.storageClass & STC.scopeinferred))
            buf.writeByte('M');

        // 'return inout ref' is the same as 'inout ref'
        if ((p.storageClass & (STC.return_ | STC.wild)) == STC.return_ &&
            !(p.storageClass & STC.returninferred))
            buf.writestring("Nk");
        switch (p.storageClass & (STC.IOR | STC.lazy_))
        {
        case 0:
            break;
        case STC.in_:
            buf.writeByte('I');
            break;
        case STC.in_ | STC.ref_:
            buf.writestring("IK");
            break;
        case STC.out_:
            buf.writeByte('J');
            break;
        case STC.ref_:
            buf.writeByte('K');
            break;
        case STC.lazy_:
            buf.writeByte('L');
            break;
        default:
            debug
            {
                printf("storageClass = x%llx\n", p.storageClass & (STC.IOR | STC.lazy_));
            }
            assert(0);
        }
        visitWithMask(p.type, (p.storageClass & STC.in_) ? MODFlags.const_ : 0);
    }
}
