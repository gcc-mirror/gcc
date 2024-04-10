/**
 * Do mangling for C++ linkage.
 *
 * This is the POSIX side of the implementation.
 * It exports two functions to C++, `toCppMangleItanium` and `cppTypeInfoMangleItanium`.
 *
 * Copyright: Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors: Walter Bright, https://www.digitalmars.com
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/cppmangle.d, _cppmangle.d)
 * Documentation:  https://dlang.org/phobos/dmd_cppmangle.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/cppmangle.d
 *
 * References:
 *  Follows Itanium C++ ABI 1.86 section 5.1
 *  http://refspecs.linux-foundation.org/cxxabi-1.86.html#mangling
 *  which is where the grammar comments come from.
 *
 * Bugs:
 *  https://issues.dlang.org/query.cgi
 *  enter `C++, mangling` as the keywords.
 */

module dmd.cppmangle;

import core.stdc.stdio;

import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.declaration;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.nspace;
import dmd.root.array;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.root.string;
import dmd.target;
import dmd.typesem;
import dmd.visitor;


// helper to check if an identifier is a C++ operator
enum CppOperator { Cast, Assign, Eq, Index, Call, Unary, Binary, OpAssign, Unknown }
package CppOperator isCppOperator(Identifier id)
{
    __gshared const(Identifier)[] operators = null;
    if (!operators)
        operators = [Id._cast, Id.assign, Id.eq, Id.index, Id.call, Id.opUnary, Id.opBinary, Id.opOpAssign];
    foreach (i, op; operators)
    {
        if (op == id)
            return cast(CppOperator)i;
    }
    return CppOperator.Unknown;
}

///
const(char)* toCppMangleItanium(Dsymbol s)
{
    //printf("toCppMangleItanium(%s)\n", s.toChars());
    OutBuffer buf;
    scope CppMangleVisitor v = new CppMangleVisitor(&buf, s.loc);
    v.mangleOf(s);
    return buf.extractChars();
}

///
const(char)* cppTypeInfoMangleItanium(Dsymbol s)
{
    //printf("cppTypeInfoMangle(%s)\n", s.toChars());
    OutBuffer buf;
    buf.writestring("_ZTI");    // "TI" means typeinfo structure
    scope CppMangleVisitor v = new CppMangleVisitor(&buf, s.loc);
    v.cpp_mangle_name(s, false);
    return buf.extractChars();
}

///
const(char)* cppThunkMangleItanium(FuncDeclaration fd, int offset)
{
    //printf("cppThunkMangleItanium(%s)\n", fd.toChars());
    OutBuffer buf;
    buf.printf("_ZThn%u_", offset);  // "Th" means thunk, "n%u" is the call offset
    scope CppMangleVisitor v = new CppMangleVisitor(&buf, fd.loc);
    v.mangle_function_encoding(fd);
    return buf.extractChars();
}

/******************************
 * Determine if sym is a full aggregate destructor.
 * Params:
 *      sym = Dsymbol
 * Returns:
 *      true if sym is an aggregate destructor
 */
bool isAggregateDtor(const Dsymbol sym)
{
    const dtor = sym.isDtorDeclaration();
    if (!dtor)
        return false;
    const ad = dtor.isMember();
    assert(ad);
    return dtor == ad.aggrDtor;
}

/// Context used when processing pre-semantic AST
private struct Context
{
    /// Template instance of the function being mangled
    TemplateInstance ti;
    /// Function declaration we're mangling
    FuncDeclaration fd;
    /// Current type / expression being processed (semantically analyzed)
    RootObject res;

    @disable ref Context opAssign(ref Context other);
    @disable ref Context opAssign(Context other);

    /**
     * Helper function to track `res`
     *
     * Params:
     *   next = Value to set `this.res` to.
     *          If `this.res` is `null`, the expression is not evalutated.
     *          This allow this code to be used even when no context is needed.
     *
     * Returns:
     *   The previous state of this `Context` object
     */
    private Context push(lazy RootObject next) @safe
    {
        auto r = this.res;
        if (r !is null)
            this.res = next;
        return Context(this.ti, this.fd, r);
    }

    /**
     * Reset the context to a previous one, making any adjustment necessary
     */
    private void pop(ref Context prev) @safe
    {
        this.res = prev.res;
    }
}

private final class CppMangleVisitor : Visitor
{
    /// Context used when processing pre-semantic AST
    private Context context;

    ABITagContainer abiTags;    /// Container for already-written ABI tags
    Objects components;         /// array of components available for substitution
    OutBuffer* buf;             /// append the mangling to buf[]
    Loc loc;                    /// location for use in error messages

    /**
     * Constructor
     *
     * Params:
     *   buf = `OutBuffer` to write the mangling to
     *   loc = `Loc` of the symbol being mangled
     */
    this(OutBuffer* buf, Loc loc) scope
    {
        this.buf = buf;
        this.loc = loc;
    }

    /*****
     * Entry point. Append mangling to buf[]
     * Params:
     *  s = symbol to mangle
     */
    void mangleOf(Dsymbol s)
    {
        if (VarDeclaration vd = s.isVarDeclaration())
        {
            mangle_variable(vd, vd.cppnamespace !is null);
        }
        else if (FuncDeclaration fd = s.isFuncDeclaration())
        {
            mangle_function(fd);
        }
        else
        {
            assert(0);
        }
    }

    /**
     * Mangle the return type of a function
     *
     * This is called on a templated function type.
     * Context is set to the `FuncDeclaration`.
     *
     * Params:
     *   preSemantic = the `FuncDeclaration`'s `originalType`
     */
    void mangleReturnType(TypeFunction preSemantic)
    {
        auto tf = this.context.res.asFuncDecl().type.isTypeFunction();
        Type rt = preSemantic.nextOf();
        // https://issues.dlang.org/show_bug.cgi?id=22739
        // auto return type means that rt is null.
        // if so, just pick up the type from the instance
        if (!rt)
            rt = tf.nextOf();
        if (tf.isref)
            rt = rt.referenceTo();
        auto prev = this.context.push(tf.nextOf());
        scope (exit) this.context.pop(prev);
        this.headOfType(rt);
    }

    /**
     * Write a seq-id from an index number, excluding the terminating '_'
     *
     * Params:
     *   idx = the index in a substitution list.
     *         Note that index 0 has no value, and `S0_` would be the
     *         substitution at index 1 in the list.
     *
     * See-Also:
     *  https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.seq-id
     */
    private void writeSequenceFromIndex(size_t idx) @safe
    {
        if (idx)
        {
            void write_seq_id(size_t i)
            {
                if (i >= 36)
                {
                    write_seq_id(i / 36);
                    i %= 36;
                }
                i += (i < 10) ? '0' : 'A' - 10;
                buf.writeByte(cast(char)i);
            }

            write_seq_id(idx - 1);
        }
    }

    /**
     * Attempt to perform substitution on `p`
     *
     * If `p` already appeared in the mangling, it is stored as
     * a 'part', and short references in the form of `SX_` can be used.
     * Note that `p` can be anything: template declaration, struct declaration,
     * class declaration, namespace...
     *
     * Params:
     *   p = The object to attempt to substitute
     *   nested = Whether or not `p` is to be considered nested.
     *            When `true`, `N` will be prepended before the substitution.
     *
     * Returns:
     *   Whether `p` already appeared in the mangling,
     *   and substitution has been written to `this.buf`.
     */
    bool substitute(RootObject p, bool nested = false)
    {
        //printf("substitute %s\n", p ? p.toChars() : null);
        auto i = find(p);
        if (i < 0)
            return false;

        //printf("\tmatch\n");
        /* Sequence is S_, S0_, .., S9_, SA_, ..., SZ_, S10_, ...
         */
        if (nested)
            buf.writeByte('N');
        buf.writeByte('S');
        writeSequenceFromIndex(i);
        buf.writeByte('_');
        return true;
    }

    /******
     * See if `p` exists in components[]
     *
     * Note that components can contain `null` entries,
     * as the index used in mangling is based on the index in the array.
     *
     * If called with an object whose dynamic type is `Nspace`,
     * calls the `find(Nspace)` overload.
     *
     * Returns:
     *  index if found, -1 if not
     */
    int find(RootObject p)
    {
        //printf("find %p %d %s\n", p, p.dyncast(), p ? p.toChars() : null);
        scope v = new ComponentVisitor(p);
        foreach (i, component; components)
        {
            if (component)
                component.visitObject(v);
            if (v.result)
                return cast(int)i;
        }
        return -1;
    }

    /*********************
     * Append p to components[]
     */
    void append(RootObject p)
    {
        //printf("append %p %d %s\n", p, p.dyncast(), p ? p.toChars() : "null");
        components.push(p);
    }

    /**
     * Write an identifier preceded by its length
     *
     * Params:
     *   ident = `Identifier` to write to `this.buf`
     */
    void writeIdentifier(const ref Identifier ident)
    {
        const name = ident.toString();
        this.buf.print(name.length);
        this.buf.writestring(name);
    }

    /**
     * Insert the leftover ABI tags to the buffer
     *
     * This inset ABI tags that hasn't already been written
     * after the mangled name of the function.
     * For more details, see the `abiTags` variable.
     *
     * Params:
     *   off  = Offset to insert at
     *   tf   = Type of the function to mangle the return type of
     */
    void writeRemainingTags(size_t off, TypeFunction tf)
    {
        Array!StringExp toWrite;
        leftOver(tf, &this.abiTags.written, &toWrite);
        OutBuffer b2;
        foreach (se; toWrite)
        {
            auto tag = se.peekString();
            // We can only insert a slice, and each insert is a memmove,
            // so use a temporary buffer to keep it efficient.
            b2.reset();
            b2.writestring("B");
            b2.print(tag.length);
            b2.writestring(tag);
            this.buf.insert(off, b2[]);
            off += b2.length;
        }
    }

    /************************
     * Determine if symbol is indeed the global ::std namespace.
     * Params:
     *  s = symbol to check
     * Returns:
     *  true if it is ::std
     */
    static bool isStd(Dsymbol s)
    {
        if (!s)
            return false;

        if (auto cnd = s.isCPPNamespaceDeclaration())
            return isStd(cnd);

        return (s.ident == Id.std &&    // the right name
                s.isNspace() &&         // g++ disallows global "std" for other than a namespace
                !getQualifier(s));      // at global level
    }

    /// Ditto
    static bool isStd(CPPNamespaceDeclaration s)
    {
        return s && s.cppnamespace is null && s.ident == Id.std;
    }

    /************************
     * Determine if type is a C++ fundamental type.
     * Params:
     *  t = type to check
     * Returns:
     *  true if it is a fundamental type
     */
    static bool isFundamentalType(Type t)
    {
        // First check the target whether some specific ABI is being followed.
        bool isFundamental = void;
        if (target.cpp.fundamentalType(t, isFundamental))
            return isFundamental;

        if (auto te = t.isTypeEnum())
        {
            // Peel off enum type from special types.
            if (te.sym.isSpecial())
                t = te.memType();
        }

        // Fundamental arithmetic types:
        // 1. integral types: bool, char, int, ...
        // 2. floating point types: float, double, real
        // 3. void
        // 4. null pointer: std::nullptr_t (since C++11)
        if (t.ty == Tvoid || t.ty == Tbool)
            return true;
        else if (t.ty == Tnull && global.params.cplusplus >= CppStdRevision.cpp11)
            return true;
        else
            return t.isTypeBasic() && (t.isintegral() || t.isreal());
    }

    /******************************
     * Write the mangled representation of a template argument.
     * Params:
     *  ti  = the template instance
     *  arg = the template argument index
     */
    void template_arg(TemplateInstance ti, size_t arg)
    {
        TemplateDeclaration td = ti.tempdecl.isTemplateDeclaration();
        assert(td);
        TemplateParameter tp = (*td.parameters)[arg];
        RootObject o = (*ti.tiargs)[arg];

        auto prev = this.context.push({
                TemplateInstance parentti;
                if (this.context.res.dyncast() == DYNCAST.dsymbol)
                    parentti = this.context.res.asFuncDecl().parent.isTemplateInstance();
                else
                {
                    auto parent = this.context.res.asType().toDsymbol(null).parent;
                    parentti = parent.isTemplateInstance();
                    // https://issues.dlang.org/show_bug.cgi?id=22760
                    // The template instance may sometimes have the form
                    // S1!int.S1, therefore the above instruction might yield null
                    if (parentti is null && parent.parent)
                        parentti = parent.parent.isTemplateInstance();
                }
                return (*parentti.tiargs)[arg];
            }());
        scope (exit) this.context.pop(prev);

        if (tp.isTemplateTypeParameter())
        {
            Type t = isType(o);
            assert(t);
            t.accept(this);
        }
        else if (TemplateValueParameter tv = tp.isTemplateValueParameter())
        {
            // <expr-primary> ::= L <type> <value number> E  # integer literal
            if (tv.valType.isintegral())
            {
                Expression e = isExpression(o);
                assert(e);
                buf.writeByte('L');
                tv.valType.accept(this);
                auto val = e.toUInteger();
                if (!tv.valType.isunsigned() && cast(sinteger_t)val < 0)
                {
                    val = -val;
                    buf.writeByte('n');
                }
                buf.print(val);
                buf.writeByte('E');
            }
            else
            {
                .error(ti.loc, "%s `%s` internal compiler error: C++ `%s` template value parameter is not supported", ti.kind, ti.toPrettyChars, tv.valType.toChars());
                fatal();
            }
        }
        else if (tp.isTemplateAliasParameter())
        {
            // Passing a function as alias parameter is the same as passing
            // `&function`
            Dsymbol d = isDsymbol(o);
            Expression e = isExpression(o);
            if (d && d.isFuncDeclaration())
            {
                // X .. E => template parameter is an expression
                // 'ad'   => unary operator ('&')
                // L .. E => is a <expr-primary>
                buf.writestring("XadL");
                mangle_function(d.isFuncDeclaration());
                buf.writestring("EE");
            }
            else if (e && e.isVarExp() && e.isVarExp().var.isVarDeclaration())
            {
                VarDeclaration vd = e.isVarExp().var.isVarDeclaration();
                buf.writeByte('L');
                mangle_variable(vd, true);
                buf.writeByte('E');
            }
            else if (d && d.isTemplateDeclaration() && d.isTemplateDeclaration().onemember)
            {
                if (!substitute(d))
                {
                    cpp_mangle_name(d, false);
                }
            }
            else
            {
                .error(ti.loc, "%s `%s` internal compiler error: C++ `%s` template alias parameter is not supported", ti.kind, ti.toPrettyChars, o.toChars());
                fatal();
            }
        }
        else if (tp.isTemplateThisParameter())
        {
            .error(ti.loc, "%s `%s` internal compiler error: C++ `%s` template this parameter is not supported", ti.kind, ti.toPrettyChars, o.toChars());
            fatal();
        }
        else
        {
            assert(0);
        }
    }

    /******************************
     * Write the mangled representation of the template arguments.
     * Params:
     *  ti = the template instance
     *  firstArg = index of the first template argument to mangle
     *             (used for operator overloading)
     * Returns:
     *  true if any arguments were written
     */
    bool template_args(TemplateInstance ti, int firstArg = 0)
    {
        /* <template-args> ::= I <template-arg>+ E
         */
        if (!ti || ti.tiargs.length <= firstArg)   // could happen if std::basic_string is not a template
            return false;
        buf.writeByte('I');
        foreach (i; firstArg .. ti.tiargs.length)
        {
            TemplateDeclaration td = ti.tempdecl.isTemplateDeclaration();
            assert(td);
            TemplateParameter tp = (*td.parameters)[i];

            /*
             * <template-arg> ::= <type>               # type or template
             *                ::= X <expression> E     # expression
             *                ::= <expr-primary>       # simple expressions
             *                ::= J <template-arg>* E  # argument pack
             *
             * Reference: https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.template-arg
             */
            if (TemplateTupleParameter tt = tp.isTemplateTupleParameter())
            {
                buf.writeByte('J');     // argument pack

                // mangle the rest of the arguments as types
                foreach (j; i .. (*ti.tiargs).length)
                {
                    Type t = isType((*ti.tiargs)[j]);
                    if (t is null)
                    {
                        .error(ti.loc, "%s `%s` internal compiler error: C++ `%s` template value parameter is not supported", ti.kind, ti.toPrettyChars, (*ti.tiargs)[j].toChars());
                        fatal();
                    }
                    t.accept(this);
                }

                buf.writeByte('E');
                break;
            }

            template_arg(ti, i);
        }
        buf.writeByte('E');
        return true;
    }

    /**
     * Write the symbol `p` if not null, then execute the delegate
     *
     * Params:
     *   p = Symbol to write
     *   dg = Delegate to execute
     */
    void writeChained(Dsymbol p, scope void delegate() dg)
    {
        if (p && !p.isModule())
        {
            buf.writestring("N");
            source_name(p, true);
            dg();
            buf.writestring("E");
        }
        else
            dg();
    }

    /**
     * Write the name of `s` to the buffer
     *
     * Params:
     *   s = Symbol to write the name of
     *   haveNE = Whether `N..E` is already part of the mangling
     *            Because `Nspace` and `CPPNamespaceAttribute` can be
     *            mixed, this is a mandatory hack.
     */
    void source_name(Dsymbol s, bool haveNE = false)
    {
        version (none)
        {
            printf("source_name(%s)\n", s.toChars());
            auto sl = this.buf.peekSlice();
            assert(sl.length == 0 || haveNE || s.cppnamespace is null || sl != "_ZN");
        }
        auto ti = s.isTemplateInstance();

        if (!ti)
        {
            auto ag = s.isAggregateDeclaration();
            const ident = (ag && ag.pMangleOverride) ? ag.pMangleOverride.id : s.ident;
            this.writeNamespace(s.cppnamespace, () {
                this.writeIdentifier(ident);
                this.abiTags.writeSymbol(s, this);
                },
                haveNE);
            return;
        }

        bool needsTa = false;

        // https://issues.dlang.org/show_bug.cgi?id=20413
        // N..E is not needed when substituting members of the std namespace.
        // This is observed in the GCC and Clang implementations.
        // The Itanium specification is not clear enough on this specific case.
        // References:
        //   https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.name
        //   https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling-compression
        Dsymbol q = getQualifier(ti.tempdecl);
        Dsymbol ns = ti.tempdecl.cppnamespace;
        const inStd = ns && isStd(ns) || q && isStd(q);
        const isNested = !inStd && (ns || q);

        if (substitute(ti.tempdecl, !haveNE && isNested))
        {
            template_args(ti);
            if (!haveNE && isNested)
                buf.writeByte('E');
            return;
        }
        else if (this.writeStdSubstitution(ti, needsTa))
        {
            this.abiTags.writeSymbol(ti, this);
            if (needsTa)
                template_args(ti);
            return;
        }

        auto ag = ti.aliasdecl ? ti.aliasdecl.isAggregateDeclaration() : null;
        if (ag && ag.pMangleOverride)
        {
            this.writeNamespace(
                ti.toAlias().cppnamespace, () {
                    this.writeIdentifier(ag.pMangleOverride.id);
                    if (ag.pMangleOverride.agg && ag.pMangleOverride.agg.isInstantiated())
                    {
                        auto to = ag.pMangleOverride.agg.isInstantiated();
                        append(to);
                        this.abiTags.writeSymbol(to.tempdecl, this);
                        template_args(to);
                    }
              }, haveNE);
        }
        else
        {
            this.writeNamespace(
                s.cppnamespace, () {
                    this.writeIdentifier(ti.tempdecl.toAlias().ident);
                    append(ti.tempdecl);
                    this.abiTags.writeSymbol(ti.tempdecl, this);
                    template_args(ti);
                }, haveNE);
        }
    }

    /********
     * See if s is actually an instance of a template
     * Params:
     *  s = symbol
     * Returns:
     *  if s is instance of a template, return the instance, otherwise return s
     */
    static Dsymbol getInstance(Dsymbol s)
    {
        Dsymbol p = s.toParent();
        if (p)
        {
            if (TemplateInstance ti = p.isTemplateInstance())
                return ti;
        }
        return s;
    }

    /// Get the namespace of a template instance
    CPPNamespaceDeclaration getTiNamespace(TemplateInstance ti)
    {
        // If we receive a pre-semantic `TemplateInstance`,
        // `cppnamespace` is always `null`
        return ti.tempdecl ? ti.cppnamespace
            : this.context.res.asType().toDsymbol(null).cppnamespace;
    }

    /********
     * Get qualifier for `s`, meaning the symbol
     * that s is in the symbol table of.
     * The module does not count as a qualifier, because C++
     * does not have modules.
     * Params:
     *  s = symbol that may have a qualifier
     *      s is rewritten to be TemplateInstance if s is one
     * Returns:
     *  qualifier, null if none
     */
    static Dsymbol getQualifier(Dsymbol s)
    {
        Dsymbol p = s.toParent();
        return (p && !p.isModule()) ? p : null;
    }

    // Detect type char
    static bool isChar(RootObject o)
    {
        Type t = isType(o);
        return (t && t.equals(Type.tchar));
    }

    // Detect type ::std::char_traits<char>
    bool isChar_traits_char(RootObject o)
    {
        return isIdent_char(Id.char_traits, o);
    }

    // Detect type ::std::allocator<char>
    bool isAllocator_char(RootObject o)
    {
        return isIdent_char(Id.allocator, o);
    }

    // Detect type ::std::ident<char>
    bool isIdent_char(Identifier ident, RootObject o)
    {
        Type t = isType(o);
        if (!t || !t.isTypeStruct())
            return false;
        Dsymbol s = t.toDsymbol(null);
        if (s.ident != ident)
            return false;
        Dsymbol p = s.toParent();
        if (!p)
            return false;
        TemplateInstance ti = p.isTemplateInstance();
        if (!ti)
            return false;
        Dsymbol q = getQualifier(ti);
        const bool inStd = isStd(q) || isStd(this.getTiNamespace(ti));
        return inStd && ti.tiargs.length == 1 && isChar((*ti.tiargs)[0]);
    }

    /***
     * Detect template args <char, ::std::char_traits<char>>
     * and write st if found.
     * Returns:
     *  true if found
     */
    bool char_std_char_traits_char(TemplateInstance ti, string st)
    {
        if (ti.tiargs.length == 2 &&
            isChar((*ti.tiargs)[0]) &&
            isChar_traits_char((*ti.tiargs)[1]))
        {
            buf.writestring(st.ptr);
            return true;
        }
        return false;
    }


    void prefix_name(Dsymbol s)
    {
        //printf("prefix_name(%s)\n", s.toChars());
        if (substitute(s))
            return;
        if (isStd(s))
            return buf.writestring("St");

        auto si = getInstance(s);
        Dsymbol p = getQualifier(si);
        if (p)
        {
            if (isStd(p))
            {
                bool needsTa;
                auto ti = si.isTemplateInstance();
                if (this.writeStdSubstitution(ti, needsTa))
                {
                    this.abiTags.writeSymbol(ti, this);
                    if (needsTa)
                    {
                        template_args(ti);
                        append(ti);
                    }
                    return;
                }
                buf.writestring("St");
            }
            else
                prefix_name(p);
        }
        source_name(si, true);
        if (!isStd(si))
            /* Do this after the source_name() call to keep components[]
             * in the right order.
             * https://issues.dlang.org/show_bug.cgi?id=17947
             */
            append(si);
    }

    /**
     * Write common substitution for standard types, such as std::allocator
     *
     * This function assumes that the symbol `ti` is in the namespace `std`.
     *
     * Params:
     *   ti = Template instance to consider
     *   needsTa = If this function returns `true`, this value indicates
     *             if additional template argument mangling is needed
     *
     * Returns:
     *   `true` if a special std symbol was found
     */
    bool writeStdSubstitution(TemplateInstance ti, out bool needsTa)
    {
        if (!ti)
            return false;
        if (!isStd(this.getTiNamespace(ti)) && !isStd(getQualifier(ti)))
            return false;

        if (ti.name == Id.allocator)
        {
            buf.writestring("Sa");
            needsTa = true;
            return true;
        }
        if (ti.name == Id.basic_string)
        {
            // ::std::basic_string<char, ::std::char_traits<char>, ::std::allocator<char>>
            if (ti.tiargs.length == 3 &&
                isChar((*ti.tiargs)[0]) &&
                isChar_traits_char((*ti.tiargs)[1]) &&
                isAllocator_char((*ti.tiargs)[2]))

            {
                buf.writestring("Ss");
                return true;
            }
            buf.writestring("Sb");      // ::std::basic_string
            needsTa = true;
            return true;
        }

        // ::std::basic_istream<char, ::std::char_traits<char>>
        if (ti.name == Id.basic_istream &&
            char_std_char_traits_char(ti, "Si"))
            return true;

        // ::std::basic_ostream<char, ::std::char_traits<char>>
        if (ti.name == Id.basic_ostream &&
            char_std_char_traits_char(ti, "So"))
            return true;

        // ::std::basic_iostream<char, ::std::char_traits<char>>
        if (ti.name == Id.basic_iostream &&
            char_std_char_traits_char(ti, "Sd"))
            return true;

        return false;
    }

    void cpp_mangle_name(Dsymbol s, bool qualified)
    {
        //printf("cpp_mangle_name(%s, %d)\n", s.toChars(), qualified);
        Dsymbol p = s.toParent();
        Dsymbol se = s;
        bool write_prefix = true;
        if (p && p.isTemplateInstance())
        {
            se = p;
            if (find(p.isTemplateInstance().tempdecl) >= 0)
                write_prefix = false;
            p = p.toParent();
        }
        if (!p || p.isModule())
        {
            source_name(se, false);
            append(s);
            return;
        }

        if (!isStd(p) || qualified)
        {
            buf.writeByte('N');
            if (write_prefix)
            {
                if (isStd(p))
                    buf.writestring("St");
                else
                    prefix_name(p);
            }
            source_name(se, true);
            buf.writeByte('E');
            append(s);
            return;
        }
        /* The N..E is not required if:
         * 1. the parent is 'std'
         * 2. 'std' is the initial qualifier
         * 3. there is no CV-qualifier or a ref-qualifier for a member function
         * ABI 5.1.8
         */
        TemplateInstance ti = se.isTemplateInstance();
        if (s.ident == Id.allocator)
        {
            buf.writestring("Sa"); // "Sa" is short for ::std::allocator
            template_args(ti);
        }
        else if (s.ident == Id.basic_string)
        {
            // ::std::basic_string<char, ::std::char_traits<char>, ::std::allocator<char>>
            if (ti.tiargs.length == 3 &&
                isChar((*ti.tiargs)[0]) &&
                isChar_traits_char((*ti.tiargs)[1]) &&
                isAllocator_char((*ti.tiargs)[2]))
            {
                buf.writestring("Ss");
                return;
            }
            buf.writestring("Sb");      // ::std::basic_string
            template_args(ti);
        }
        else
        {
            // ::std::basic_istream<char, ::std::char_traits<char>>
            if (s.ident == Id.basic_istream)
            {
                if (char_std_char_traits_char(ti, "Si"))
                    return;
            }
            else if (s.ident == Id.basic_ostream)
            {
                if (char_std_char_traits_char(ti, "So"))
                    return;
            }
            else if (s.ident == Id.basic_iostream)
            {
                if (char_std_char_traits_char(ti, "Sd"))
                    return;
            }
            buf.writestring("St");
            source_name(se, true);
        }
        append(s);
    }

    /**
     * Write CV-qualifiers to the buffer
     *
     * CV-qualifiers are 'r': restrict (unused in D), 'V': volatile, 'K': const
     *
     * See_Also:
     *   https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.CV-qualifiers
     */
    void CV_qualifiers(const Type t)
    {
        if (t.isConst())
            buf.writeByte('K');
    }

    /**
     * Mangles a variable
     *
     * Params:
     *   d = Variable declaration to mangle
     *   isNested = Whether this variable is nested, e.g. a template parameter
     *              or within a namespace
     */
    void mangle_variable(VarDeclaration d, bool isNested)
    {
        // fake mangling for fields to fix https://issues.dlang.org/show_bug.cgi?id=16525
        if (!(d.storage_class & (STC.extern_ | STC.field | STC.gshared)))
        {
            .error(d.loc, "%s `%s` internal compiler error: C++ static non-`__gshared` non-`extern` variables not supported", d.kind, d.toPrettyChars);
            fatal();
        }
        Dsymbol p = d.toParent();
        if (p && !p.isModule()) //for example: char Namespace1::beta[6] should be mangled as "_ZN10Namespace14betaE"
        {
            buf.writestring("_ZN");
            prefix_name(p);
            source_name(d, true);
            buf.writeByte('E');
        }
        else if (isNested)
        {
            buf.writestring("_Z");
            source_name(d, false);
        }
        else
        {
            if (auto varTags = ABITagContainer.forSymbol(d))
            {
                buf.writestring("_Z");
                source_name(d, false);
                return;
            }
            if (auto typeTags = ABITagContainer.forSymbol(d.type.toDsymbol(null)))
            {
                buf.writestring("_Z");
                source_name(d, false);
                this.abiTags.write(*this.buf, typeTags);
                return;
            }
            //char beta[6] should mangle as "beta"
            buf.writestring(d.ident.toString());
        }
    }

    void mangle_function(FuncDeclaration d)
    {
        //printf("mangle_function(%s)\n", d.toChars());
        /*
         * <mangled-name> ::= _Z <encoding>
         */
        buf.writestring("_Z");
        this.mangle_function_encoding(d);
    }

    void mangle_function_encoding(FuncDeclaration d)
    {
        //printf("mangle_function_encoding(%s)\n", d.toChars());
        /*
         * <encoding> ::= <function name> <bare-function-type>
         *            ::= <data name>
         *            ::= <special-name>
         */
        TypeFunction tf = d.type.isTypeFunction();

        if (TemplateDeclaration ftd = getFuncTemplateDecl(d))
        {
            /* It's an instance of a function template
             */
            TemplateInstance ti = d.parent.isTemplateInstance();
            assert(ti);
            this.mangleTemplatedFunction(d, tf, ftd, ti);
            return;
        }

        Dsymbol p = d.toParent();
        if (p && !p.isModule() && tf.linkage == LINK.cpp)
        {
            this.mangleNestedFuncPrefix(tf, p);

            if (auto ctor = d.isCtorDeclaration())
                buf.writestring(ctor.isCpCtor ? "C2" : "C1");
            else if (d.isAggregateDtor())
                buf.writestring("D1");
            else if (d.ident && d.ident == Id.assign)
                buf.writestring("aS");
            else if (d.ident && d.ident == Id.eq)
                buf.writestring("eq");
            else if (d.ident && d.ident == Id.index)
                buf.writestring("ix");
            else if (d.ident && d.ident == Id.call)
                buf.writestring("cl");
            else
                source_name(d, true);
            buf.writeByte('E');
        }
        else
        {
            source_name(d, false);
        }

        // Save offset for potentially writing tags
        const size_t off = this.buf.length();

        // Template args accept extern "C" symbols with special mangling
        if (tf.linkage == LINK.cpp)
            mangleFunctionParameters(tf.parameterList);

        if (!tf.next.isTypeBasic())
            this.writeRemainingTags(off, tf);
    }

    /**
     * Recursively mangles a non-scoped namespace
     *
     * Parameters:
     *   ns = Namespace to mangle
     *   dg = A delegate to write the identifier in this namespace
     *   haveNE = When `false` (the default), surround the namespace / dg
     *            call with nested name qualifier (`N..E`).
     *            Otherwise, they are already present (e.g. `Nspace` was used).
     */
    void writeNamespace(CPPNamespaceDeclaration ns, scope void delegate() dg,
                        bool haveNE = false)
    {
        void runDg () { if (dg !is null) dg(); }

        if (ns is null || ns.ident is null)
            return runDg();

        if (isStd(ns))
        {
            if (!substitute(ns))
                buf.writestring("St");
            runDg();
        }
        else if (dg !is null)
        {
            if (!haveNE)
                buf.writestring("N");
            if (!substitute(ns))
            {
                this.writeNamespace(ns.cppnamespace, null);
                this.writeIdentifier(ns.ident);
                append(ns);
            }
            dg();
            if (!haveNE)
                buf.writestring("E");
        }
        else if (!substitute(ns))
        {
            this.writeNamespace(ns.cppnamespace, null);
            this.writeIdentifier(ns.ident);
            append(ns);
        }
    }

    /**
     * Mangles a function template to C++
     *
     * Params:
     *   d = Function declaration
     *   tf = Function type (casted d.type)
     *   ftd = Template declaration (ti.templdecl)
     *   ti = Template instance (d.parent)
     */
    void mangleTemplatedFunction(FuncDeclaration d, TypeFunction tf,
                                 TemplateDeclaration ftd, TemplateInstance ti)
    {
        Dsymbol p = ti.toParent();
        // Check if this function is *not* nested
        if (!p || p.isModule() || tf.linkage != LINK.cpp)
        {
            this.context.ti = ti;
            this.context.fd = d;
            this.context.res = d;
            TypeFunction preSemantic = d.originalType.isTypeFunction();
            auto nspace = ti.toParent();
            if (nspace && nspace.isNspace())
                this.writeChained(ti.toParent(), () => source_name(ti, true));
            else
                source_name(ti, false);
            this.mangleReturnType(preSemantic);
            this.mangleFunctionParameters(ParameterList(preSemantic.parameterList.parameters, tf.parameterList.varargs));
            return;
        }

        // It's a nested function (e.g. a member of an aggregate)
        this.mangleNestedFuncPrefix(tf, p);

        if (d.isCtorDeclaration())
        {
            buf.writestring("C1");
            mangleFunctionParameters(tf.parameterList);
            return;
        }
        else if (d.isAggregateDtor())
        {
            buf.writestring("D1");
            mangleFunctionParameters(tf.parameterList);
            return;
        }

        int firstTemplateArg = 0;
        bool appendReturnType = true;
        bool isConvertFunc = false;
        string symName;

        // test for special symbols
        CppOperator whichOp = isCppOperator(ti.name);
        final switch (whichOp)
        {
        case CppOperator.Unknown:
            break;
        case CppOperator.Cast:
            symName = "cv";
            firstTemplateArg = 1;
            isConvertFunc = true;
            appendReturnType = false;
            break;
        case CppOperator.Assign:
            symName = "aS";
            break;
        case CppOperator.Eq:
            symName = "eq";
            break;
        case CppOperator.Index:
            symName = "ix";
            break;
        case CppOperator.Call:
            symName = "cl";
            break;
        case CppOperator.Unary:
        case CppOperator.Binary:
        case CppOperator.OpAssign:
            TemplateDeclaration td = ti.tempdecl.isTemplateDeclaration();
            assert(td);
            assert(ti.tiargs.length >= 1);
            TemplateParameter tp = (*td.parameters)[0];
            TemplateValueParameter tv = tp.isTemplateValueParameter();
            if (!tv || !tv.valType.isString())
                break; // expecting a string argument to operators!
            Expression exp = (*ti.tiargs)[0].isExpression();
            StringExp str = exp.toStringExp();
            switch (whichOp)
            {
            case CppOperator.Unary:
                switch (str.peekString())
                {
                case "*":   symName = "de"; goto continue_template;
                case "++":  symName = "pp"; goto continue_template;
                case "--":  symName = "mm"; goto continue_template;
                case "-":   symName = "ng"; goto continue_template;
                case "+":   symName = "ps"; goto continue_template;
                case "~":   symName = "co"; goto continue_template;
                default:    break;
                }
                break;
            case CppOperator.Binary:
                switch (str.peekString())
                {
                case ">>":  symName = "rs"; goto continue_template;
                case "<<":  symName = "ls"; goto continue_template;
                case "*":   symName = "ml"; goto continue_template;
                case "-":   symName = "mi"; goto continue_template;
                case "+":   symName = "pl"; goto continue_template;
                case "&":   symName = "an"; goto continue_template;
                case "/":   symName = "dv"; goto continue_template;
                case "%":   symName = "rm"; goto continue_template;
                case "^":   symName = "eo"; goto continue_template;
                case "|":   symName = "or"; goto continue_template;
                default:    break;
                }
                break;
            case CppOperator.OpAssign:
                switch (str.peekString())
                {
                case "*":   symName = "mL"; goto continue_template;
                case "+":   symName = "pL"; goto continue_template;
                case "-":   symName = "mI"; goto continue_template;
                case "/":   symName = "dV"; goto continue_template;
                case "%":   symName = "rM"; goto continue_template;
                case ">>":  symName = "rS"; goto continue_template;
                case "<<":  symName = "lS"; goto continue_template;
                case "&":   symName = "aN"; goto continue_template;
                case "|":   symName = "oR"; goto continue_template;
                case "^":   symName = "eO"; goto continue_template;
                default:    break;
                }
                break;
            default:
                assert(0);
            continue_template:
                firstTemplateArg = 1;
                break;
            }
            break;
        }
        if (symName.length == 0)
            source_name(ti, true);
        else
        {
            buf.writestring(symName);
            if (isConvertFunc)
                template_arg(ti, 0);
            appendReturnType = template_args(ti, firstTemplateArg) && appendReturnType;
        }
        buf.writeByte('E');
        if (appendReturnType)
            headOfType(tf.nextOf());  // mangle return type
        mangleFunctionParameters(tf.parameterList);
    }

    /**
     * Mangle the parameters of a function
     *
     * For templated functions, `context.res` is set to the `FuncDeclaration`
     *
     * Params:
     *   parameters = Array of `Parameter` to mangle
     *   varargs = if != 0, this function has varargs parameters
     */
    void mangleFunctionParameters(ParameterList parameterList)
    {
        int numparams = 0;

        foreach (n, fparam; parameterList)
        {
            Type t = fparam.type.merge2();
            if (fparam.isReference())
                t = t.referenceTo();
            else if (fparam.isLazy())
            {
                // Mangle as delegate
                auto tf = new TypeFunction(ParameterList(), t, LINK.d);
                auto td = new TypeDelegate(tf);
                t = td.merge();
            }
            else if (Type cpptype = target.cpp.parameterType(t))
                t = cpptype;
            if (t.ty == Tsarray)
            {
                // Static arrays in D are passed by value; no counterpart in C++
                .error(loc, "internal compiler error: unable to pass static array `%s` to extern(C++) function, use pointer instead",
                    t.toChars());
                fatal();
            }
            auto prev = this.context.push({
                    TypeFunction tf;
                    if (isDsymbol(this.context.res))
                        tf = this.context.res.asFuncDecl().type.isTypeFunction();
                    else
                        tf = this.context.res.asType().isTypeFunction();
                    assert(tf);
                    return (*tf.parameterList.parameters)[n].type;
                }());
            scope (exit) this.context.pop(prev);

            if (this.context.ti && global.params.cplusplus >= CppStdRevision.cpp11)
                handleParamPack(t, this.context.ti.tempdecl.isTemplateDeclaration().parameters);

            headOfType(t);
            ++numparams;
        }

        if (parameterList.varargs == VarArg.variadic)
            buf.writeByte('z');
        else if (!numparams)
            buf.writeByte('v'); // encode (void) parameters
    }

    /****** The rest is type mangling ************/

    void error(Type t)
    {
        const(char)* p;
        if (t.isImmutable())
            p = "`immutable` ";
        else if (t.isShared())
            p = "`shared` ";
        else
            p = "";
        .error(loc, "internal compiler error: %stype `%s` cannot be mapped to C++\n", p, t.toChars());
        fatal(); //Fatal, because this error should be handled in frontend
    }

    /****************************
     * Mangle a type,
     * treating it as a Head followed by a Tail.
     * Params:
     *  t = Head of a type
     */
    void headOfType(Type t)
    {
        if (auto tc = t.isTypeClass())
        {
            mangleTypeClass(tc, true);
        }
        else
        {
            // For value types, strip const/immutable/shared from the head of the type
            auto prev = this.context.push(this.context.res.asType().mutableOf().unSharedOf());
            scope (exit) this.context.pop(prev);
            t.mutableOf().unSharedOf().accept(this);
        }
    }

    /******
     * Write out 1 or 2 character basic type mangling.
     * Handle const and substitutions.
     * Params:
     *  t = type to mangle
     *  p = if not 0, then character prefix
     *  c = mangling character
     */
    void writeBasicType(Type t, char p, char c)
    {
        // Only do substitutions for non-fundamental types.
        if (!isFundamentalType(t) || t.isConst())
        {
            if (substitute(t))
                return;
            else
                append(t);
        }
        CV_qualifiers(t);
        if (p)
            buf.writeByte(p);
        buf.writeByte(c);
    }


    /****************
     * Write structs and enums.
     * Params:
     *  t = TypeStruct or TypeEnum
     */
    void doSymbol(Type t)
    {
        if (substitute(t))
            return;
        CV_qualifiers(t);

        // Handle any target-specific struct types.
        if (auto tm = target.cpp.typeMangle(t))
        {
            buf.writestring(tm);
        }
        else
        {
            Dsymbol s = t.toDsymbol(null);
            Dsymbol p = s.toParent();
            if (p && p.isTemplateInstance())
            {
                 /* https://issues.dlang.org/show_bug.cgi?id=17947
                  * Substitute the template instance symbol, not the struct/enum symbol
                  */
                if (substitute(p))
                    return;
            }
            if (!substitute(s))
                cpp_mangle_name(s, false);
        }
        if (t.isConst())
            append(t);
    }



    /************************
     * Mangle a class type.
     * If it's the head, treat the initial pointer as a value type.
     * Params:
     *  t = class type
     *  head = true for head of a type
     */
    void mangleTypeClass(TypeClass t, bool head)
    {
        if (t.isImmutable() || t.isShared())
            return error(t);

        /* Mangle as a <pointer to><struct>
         */
        if (substitute(t))
            return;
        if (!head)
            CV_qualifiers(t);
        buf.writeByte('P');

        CV_qualifiers(t);

        {
            Dsymbol s = t.toDsymbol(null);
            Dsymbol p = s.toParent();
            if (p && p.isTemplateInstance())
            {
                 /* https://issues.dlang.org/show_bug.cgi?id=17947
                  * Substitute the template instance symbol, not the class symbol
                  */
                if (substitute(p))
                    return;
            }
        }

        if (!substitute(t.sym))
        {
            cpp_mangle_name(t.sym, false);
        }
        if (t.isConst())
            append(null);  // C++ would have an extra type here
        append(t);
    }

    /**
     * Mangle the prefix of a nested (e.g. member) function
     *
     * Params:
     *   tf = Type of the nested function
     *   parent = Parent in which the function is nested
     */
    void mangleNestedFuncPrefix(TypeFunction tf, Dsymbol parent)
    {
        /* <nested-name> ::= N [<CV-qualifiers>] <prefix> <unqualified-name> E
         *               ::= N [<CV-qualifiers>] <template-prefix> <template-args> E
         */
        buf.writeByte('N');
        CV_qualifiers(tf);

        /* <prefix> ::= <prefix> <unqualified-name>
         *          ::= <template-prefix> <template-args>
         *          ::= <template-param>
         *          ::= # empty
         *          ::= <substitution>
         *          ::= <prefix> <data-member-prefix>
         */
        prefix_name(parent);
    }

    /**
     * Write `Dp` (C++11 function parameter pack prefix) if 't' is a TemplateSequenceParameter (T...).
     *
     * Params:
     *   t      = Parameter type
     *   params = Template parameters of the function
     */
    private void handleParamPack(Type t, TemplateParameters* params)
    {
        if (t.isTypeReference())
            t = t.nextOf();
        auto ti = t.isTypeIdentifier();
        if (!ti)
            return;

        auto idx = templateParamIndex(ti.ident, params);
        if (idx < params.length && (*params)[idx].isTemplateTupleParameter())
            buf.writestring("Dp");
    }

    /**
     * Helper function to write a `T..._` template index.
     *
     * Params:
     *   idx   = Index of `param` in the template argument list
     *   param = Template parameter to mangle
     */
    private void writeTemplateArgIndex(size_t idx, TemplateParameter param)
    {
        // expressions are mangled in <X..E>
        if (param.isTemplateValueParameter())
            buf.writeByte('X');
        buf.writeByte('T');
        writeSequenceFromIndex(idx);
        buf.writeByte('_');
        if (param.isTemplateValueParameter())
            buf.writeByte('E');
    }

    /**
     * Given an array of template parameters and an identifier,
     * returns the index of the identifier in that array.
     *
     * Params:
     *   ident = Identifier for which substitution is attempted
     *           (e.g. `void func(T)(T param)` => `T` from `T param`)
     *   params = `TemplateParameters` of the enclosing symbol
     *           (in the previous example, `func`'s template parameters)
     *
     * Returns:
     *   The index of the identifier match in `params`,
     *   or `params.length` if there wasn't any match.
     */
    private static size_t templateParamIndex(
        const ref Identifier ident, TemplateParameters* params) @safe
    {
        foreach (idx, param; *params)
            if (param.ident == ident)
                return idx;
        return params.length;
    }

    /**
     * Given a template instance `t`, write its qualified name
     * without the template parameter list
     *
     * Params:
     *   t = Post-parsing `TemplateInstance` pointing to the symbol
     *       to mangle (one level deep)
     *   dg = Delegate to execute after writing the qualified symbol
     *
     */
    private void writeQualified(TemplateInstance t, scope void delegate() dg)
    {
        auto type = isType(this.context.res);
        if (!type)
        {
            this.writeIdentifier(t.name);
            return dg();
        }
        auto sym1 = type.toDsymbol(null);
        if (!sym1)
        {
            this.writeIdentifier(t.name);
            return dg();
        }
        // Get the template instance
        auto sym = getQualifier(sym1);
        auto sym2 = getQualifier(sym);
        if (sym2 && isStd(sym2)) // Nspace path
        {
            bool unused;
            assert(sym.isTemplateInstance());
            if (this.writeStdSubstitution(sym.isTemplateInstance(), unused))
                return dg();
            // std names don't require `N..E`
            buf.writestring("St");
            this.writeIdentifier(t.name);
            this.append(t);
            return dg();
        }
        else if (sym2)
        {
            buf.writestring("N");
            if (!this.substitute(sym2))
                sym2.accept(this);
        }
        this.writeNamespace(
            sym1.cppnamespace, () {
                this.writeIdentifier(t.name);
                this.append(t);
                dg();
            });
        if (sym2)
            buf.writestring("E");
    }

extern(C++):

    alias visit = Visitor.visit;

    override void visit(TypeNull t)
    {
        if (t.isImmutable() || t.isShared())
            return error(t);

        writeBasicType(t, 'D', 'n');
    }

    override void visit(TypeNoreturn t)
    {
        if (t.isImmutable() || t.isShared())
            return error(t);

        writeBasicType(t, 0, 'v');      // mangle like `void`
    }

    override void visit(TypeBasic t)
    {
        if (t.isImmutable() || t.isShared())
            return error(t);

        // Handle any target-specific basic types.
        if (auto tm = target.cpp.typeMangle(t))
        {
            // Only do substitutions for non-fundamental types.
            if (!isFundamentalType(t) || t.isConst())
            {
                if (substitute(t))
                    return;
                else
                    append(t);
            }
            CV_qualifiers(t);
            buf.writestring(tm);
            return;
        }

        /* <builtin-type>:
         * v        void
         * w        wchar_t
         * b        bool
         * c        char
         * a        signed char
         * h        unsigned char
         * s        short
         * t        unsigned short
         * i        int
         * j        unsigned int
         * l        long
         * m        unsigned long
         * x        long long, __int64
         * y        unsigned long long, __int64
         * n        __int128
         * o        unsigned __int128
         * f        float
         * d        double
         * e        long double, __float80
         * g        __float128
         * z        ellipsis
         * Dd       64 bit IEEE 754r decimal floating point
         * De       128 bit IEEE 754r decimal floating point
         * Df       32 bit IEEE 754r decimal floating point
         * Dh       16 bit IEEE 754r half-precision floating point
         * Di       char32_t
         * Ds       char16_t
         * u <source-name>  # vendor extended type
         */
        if (t.isimaginary() || t.iscomplex())
        {
            // https://issues.dlang.org/show_bug.cgi?id=22806
            // Complex and imaginary types are represented in the same way as
            // arrays or vectors in C++.  First substitute the outer type, then
            // write out the mangle string of the underlying type.
            if (substitute(t))
                return;
            append(t);
            CV_qualifiers(t);

            if (t.isimaginary())
                buf.writeByte('G'); // 'G' means imaginary
            else
                buf.writeByte('C'); // 'C' means complex

            switch (t.ty)
            {
                case Timaginary32:
                case Tcomplex32:
                    return Type.tfloat32.accept(this);
                case Timaginary64:
                case Tcomplex64:
                    return Type.tfloat64.accept(this);
                case Timaginary80:
                case Tcomplex80:
                    return Type.tfloat80.accept(this);
                default:
                    assert(0);
            }
        }

        char c;
        char p = 0;
        switch (t.ty)
        {
            case Tvoid:                 c = 'v';        break;
            case Tint8:                 c = 'a';        break;
            case Tuns8:                 c = 'h';        break;
            case Tint16:                c = 's';        break;
            case Tuns16:                c = 't';        break;
            case Tint32:                c = 'i';        break;
            case Tuns32:                c = 'j';        break;
            case Tfloat32:              c = 'f';        break;
            case Tint64:
                c = target.c.longsize == 8 ? 'l' : 'x';
                break;
            case Tuns64:
                c = target.c.longsize == 8 ? 'm' : 'y';
                break;
            case Tint128:                c = 'n';       break;
            case Tuns128:                c = 'o';       break;
            case Tfloat64:               c = 'd';       break;
            case Tfloat80:               c = 'e';       break;
            case Tbool:                  c = 'b';       break;
            case Tchar:                  c = 'c';       break;
            case Twchar:        p = 'D'; c = 's';       break;  // since C++11
            case Tdchar:        p = 'D'; c = 'i';       break;  // since C++11

            default:
                return error(t);
        }
        writeBasicType(t, p, c);
    }

    override void visit(TypeVector t)
    {
        if (t.isImmutable() || t.isShared())
            return error(t);

        if (substitute(t))
            return;
        append(t);
        CV_qualifiers(t);

        // Handle any target-specific vector types.
        if (auto tm = target.cpp.typeMangle(t))
        {
            buf.writestring(tm);
        }
        else
        {
            assert(t.basetype && t.basetype.ty == Tsarray);
            auto tsa = t.basetype.isTypeSArray();
            assert(tsa.dim);
            buf.writestring("Dv");          // -- Gnu ABI v.4
            buf.print(tsa.dim.toInteger());
            buf.writeByte('_');
            t.basetype.nextOf().accept(this);
        }
    }

    override void visit(TypeSArray t)
    {
        if (t.isImmutable() || t.isShared())
            return error(t);

        if (!substitute(t))
            append(t);
        CV_qualifiers(t);
        buf.writeByte('A');
        buf.print(t.dim ? t.dim.toInteger() : 0);
        buf.writeByte('_');
        t.next.accept(this);
    }

    override void visit(TypePointer t)
    {
        if (t.isImmutable() || t.isShared())
            return error(t);

        // Check for const - Since we cannot represent C++'s `char* const`,
        // and `const char* const` (a.k.a `const(char*)` in D) is mangled
        // the same as `const char*` (`const(char)*` in D), we need to add
        // an extra `K` if `nextOf()` is `const`, before substitution
        CV_qualifiers(t);
        if (substitute(t))
            return;
        buf.writeByte('P');
        auto prev = this.context.push(this.context.res.asType().nextOf());
        scope (exit) this.context.pop(prev);
        t.next.accept(this);
        append(t);
    }

    override void visit(TypeReference t)
    {
        if (substitute(t))
            return;
        buf.writeByte('R');
        CV_qualifiers(t.nextOf());
        headOfType(t.nextOf());
        if (t.nextOf().isConst())
            append(t.nextOf());
        append(t);
    }

    override void visit(TypeFunction t)
    {
        /*
         *  <function-type> ::= F [Y] <bare-function-type> E
         *  <bare-function-type> ::= <signature type>+
         *  # types are possible return type, then parameter types
         */
        /* ABI says:
            "The type of a non-static member function is considered to be different,
            for the purposes of substitution, from the type of a namespace-scope or
            static member function whose type appears similar. The types of two
            non-static member functions are considered to be different, for the
            purposes of substitution, if the functions are members of different
            classes. In other words, for the purposes of substitution, the class of
            which the function is a member is considered part of the type of
            function."

            BUG: Right now, types of functions are never merged, so our simplistic
            component matcher always finds them to be different.
            We should use Type.equals on these, and use different
            TypeFunctions for non-static member functions, and non-static
            member functions of different classes.
         */
        if (substitute(t))
            return;
        buf.writeByte('F');
        if (t.linkage == LINK.c)
            buf.writeByte('Y');
        Type tn = t.next;
        if (t.isref)
            tn = tn.referenceTo();
        tn.accept(this);
        mangleFunctionParameters(t.parameterList);
        buf.writeByte('E');
        append(t);
    }

    override void visit(TypeStruct t)
    {
        if (t.isImmutable() || t.isShared())
            return error(t);
        //printf("TypeStruct %s\n", t.toChars());
        doSymbol(t);
    }

    override void visit(TypeEnum t)
    {
        if (t.isImmutable() || t.isShared())
            return error(t);

        /* __c_(u)long(long) and others get special mangling
         */
        const id = t.sym.ident;
        //printf("enum id = '%s'\n", id.toChars());
        if (id == Id.__c_long)
            return writeBasicType(t, 0, 'l');
        else if (id == Id.__c_ulong)
            return writeBasicType(t, 0, 'm');
        else if (id == Id.__c_char)
            return writeBasicType(t, 0, 'c');
        else if (id == Id.__c_wchar_t)
            return writeBasicType(t, 0, 'w');
        else if (id == Id.__c_longlong)
            return writeBasicType(t, 0, 'x');
        else if (id == Id.__c_ulonglong)
            return writeBasicType(t, 0, 'y');
        else if (id == Id.__c_complex_float)
            return Type.tcomplex32.accept(this);
        else if (id == Id.__c_complex_double)
            return Type.tcomplex64.accept(this);
        else if (id == Id.__c_complex_real)
            return Type.tcomplex80.accept(this);

        doSymbol(t);
    }

    override void visit(TypeClass t)
    {
        mangleTypeClass(t, false);
    }

    /**
     * Performs template parameter substitution
     *
     * Mangling is performed on a copy of the post-parsing AST before
     * any semantic pass is run.
     * There is no easy way to link a type to the template parameters
     * once semantic has run, because:
     * - the `TemplateInstance` installs aliases in its scope to its params
     * - `AliasDeclaration`s are resolved in many places
     * - semantic passes are destructive, so the `TypeIdentifier` gets lost
     *
     * As a result, the best approach with the current architecture is to:
     * - Run the visitor on the `originalType` of the function,
     *   looking up any `TypeIdentifier` at the template scope when found.
     * - Fallback to the post-semantic `TypeFunction` when the identifier is
     *   not a template parameter.
     */
    override void visit(TypeIdentifier t)
    {
        auto decl = this.context.ti.tempdecl.isTemplateDeclaration();
        assert(decl.parameters !is null);
        auto idx = templateParamIndex(t.ident, decl.parameters);
        // If not found, default to the post-semantic type
        if (idx >= decl.parameters.length)
            return this.context.res.visitObject(this);

        auto param = (*decl.parameters)[idx];
        if (auto type = this.context.res.isType())
            CV_qualifiers(type);
        // Otherwise, attempt substitution (`S_` takes precedence on `T_`)
        if (this.substitute(param))
            return;

        // If substitution failed, write `TX_` where `X` is the index
        this.writeTemplateArgIndex(idx, param);
        this.append(param);
        // Write the ABI tags, if any
        if (auto sym = this.context.res.isDsymbol())
            this.abiTags.writeSymbol(sym, this);
    }

    /// Ditto
    override void visit(TypeInstance t)
    {
        assert(t.tempinst !is null);
        t.tempinst.accept(this);
    }

    /**
     * Mangles a `TemplateInstance`
     *
     * A `TemplateInstance` can be found either in the parameter,
     * or the return value.
     * Arguments to the template instance needs to be mangled but the template
     * can be partially substituted, so for example the following:
     * `Container!(T, Val) func16479_12 (alias Container, T, int Val) ()`
     * will mangle the return value part to "T_IT0_XT1_EE"
     */
    override void visit(TemplateInstance t)
    {
        // Template names are substituted, but args still need to be written
        void writeArgs ()
        {
            buf.writeByte('I');
            // When visiting the arguments, the context will be set to the
            // resolved type
            auto analyzed_ti = this.context.res.asType().toDsymbol(null).isInstantiated();
            auto prev = this.context;
            scope (exit) this.context.pop(prev);
            foreach (idx, RootObject o; *t.tiargs)
            {
                this.context.res = (*analyzed_ti.tiargs)[idx];
                o.visitObject(this);
            }
            if (analyzed_ti.tiargs.length > t.tiargs.length)
            {
                // If the resolved AST has more args than the parse one,
                // we have default arguments
                auto oparams = analyzed_ti.tempdecl.isTemplateDeclaration().origParameters;
                foreach (idx, arg; (*oparams)[t.tiargs.length .. $])
                {
                    this.context.res = (*analyzed_ti.tiargs)[idx + t.tiargs.length];

                    if (auto ttp = arg.isTemplateTypeParameter())
                        ttp.defaultType.accept(this);
                    else if (auto tvp = arg.isTemplateValueParameter())
                        tvp.defaultValue.accept(this);
                    else if (auto tvp = arg.isTemplateThisParameter())
                        tvp.defaultType.accept(this);
                    else if (auto tvp = arg.isTemplateAliasParameter())
                        tvp.defaultAlias.visitObject(this);
                    else
                        assert(0, arg.toString());
                }
            }
            buf.writeByte('E');
        }

        // `name` is used, not `ident`
        assert(t.name !is null);
        assert(t.tiargs !is null);

        bool needsTa;
        auto decl = this.context.ti.tempdecl.isTemplateDeclaration();
        // Attempt to substitute the template itself
        auto idx = templateParamIndex(t.name, decl.parameters);
        if (idx < decl.parameters.length)
        {
            auto param = (*decl.parameters)[idx];
            if (auto type = t.getType())
                CV_qualifiers(type);
            if (this.substitute(param))
                return;
            this.writeTemplateArgIndex(idx, param);
            this.append(param);
            writeArgs();
        }
        else if (this.writeStdSubstitution(t, needsTa))
        {
            if (needsTa)
                writeArgs();
        }
        else if (!this.substitute(t))
            this.writeQualified(t, &writeArgs);
    }

    /// Ditto
    override void visit(IntegerExp t)
    {
        this.buf.writeByte('L');
        t.type.accept(this);
        this.buf.print(t.getInteger());
        this.buf.writeByte('E');
    }

    override void visit(Nspace t)
    {
        if (auto p = getQualifier(t))
            p.accept(this);

        if (isStd(t))
            buf.writestring("St");
        else
        {
            this.writeIdentifier(t.ident);
            this.append(t);
        }
    }

    override void visit(Type t)
    {
        error(t);
    }

    void visit(Tuple t)
    {
        assert(0);
    }
}

/// Helper code to visit `RootObject`, as it doesn't define `accept`,
/// only its direct subtypes do.
private void visitObject(V : Visitor)(RootObject o, V this_)
{
    assert(o !is null);
    if (Type ta = isType(o))
        ta.accept(this_);
    else if (Expression ea = isExpression(o))
        ea.accept(this_);
    else if (Dsymbol sa = isDsymbol(o))
        sa.accept(this_);
    else if (TemplateParameter t = isTemplateParameter(o))
        t.accept(this_);
    else if (Tuple t = isTuple(o))
        // `Tuple` inherits `RootObject` and does not define accept
        // For this reason, this uses static dispatch on the visitor
        this_.visit(t);
    else
        assert(0, o.toString());
}

/// Helper function to safely get a type out of a `RootObject`
private Type asType(RootObject o) @safe
{
    if (Type ta = isType(o))
        return ta;

    // When called with context.res as argument, it can be `FuncDeclaration`
    if (auto fd = o.asFuncDecl())
        return fd.type;
    assert(0);
}

/// Helper function to safely get a `FuncDeclaration` out of a `RootObject`
private FuncDeclaration asFuncDecl(RootObject o) @safe
{
    Dsymbol d = isDsymbol(o);
    assert(d !is null);
    auto fd = d.isFuncDeclaration();
    assert(fd !is null);
    return fd;
}

/// Helper class to compare entries in components
private extern(C++) final class ComponentVisitor : Visitor
{
    /// Only one of the following is not `null`, it's always
    /// the most specialized type, set from the ctor
    private Nspace namespace;

    /// Ditto
    private CPPNamespaceDeclaration namespace2;

    /// Ditto
    private TypePointer tpointer;

    /// Ditto
    private TypeReference tref;

    /// Ditto
    private TypeIdentifier tident;

    /// Least specialized type
    private RootObject object;

    /// Set to the result of the comparison
    private bool result;

    public this(RootObject base) @safe
    {
        switch (base.dyncast())
        {
        case DYNCAST.dsymbol:
            if (auto ns = (cast(Dsymbol)base).isNspace())
                this.namespace = ns;
            else if (auto ns = (cast(Dsymbol)base).isCPPNamespaceDeclaration())
                this.namespace2 = ns;
            else
                goto default;
            break;

        case DYNCAST.type:
            auto t = cast(Type)base;
            if (auto tp = t.isTypePointer())
                this.tpointer = tp;
            else if (auto tr = t.isTypeReference())
                this.tref = tr;
            else if (auto ti = t.isTypeIdentifier())
                this.tident = ti;
            else
                goto default;
            break;

        // Note: ABI tags are also handled here (they are TupleExp of StringExp)
        default:
            this.object = base;
        }
    }

    /// Introduce base class overloads
    alias visit = Visitor.visit;

    /// Least specialized overload of each direct child of `RootObject`
    public override void visit(Dsymbol o)
    {
        this.result = this.object && this.object == o;
    }

    /// Ditto
    public override void visit(Expression o)
    {
        this.result = this.object && this.object == o;
    }

    /// Ditto
    public void visit(Tuple o)
    {
        this.result = this.object && this.object == o;
    }

    /// Ditto
    public override void visit(Type o)
    {
        this.result = this.object && this.object == o;
    }

    /// Ditto
    public override void visit(TemplateParameter o)
    {
        this.result = this.object && this.object == o;
    }

    /**
     * This overload handles composed types including template parameters
     *
     * Components for substitutions include "next" type.
     * For example, if `ref T` is present, `ref T` and `T` will be present
     * in the substitution array.
     * But since we don't have the final/merged type, we cannot rely on
     * object comparison, and need to recurse instead.
     */
    public override void visit(TypeReference o)
    {
        if (!this.tref)
            return;
        if (this.tref == o)
            this.result = true;
        else
        {
            // It might be a reference to a template parameter that we already
            // saw, so we need to recurse
            scope v = new ComponentVisitor(this.tref.next);
            o.next.visitObject(v);
            this.result = v.result;
        }
    }

    /// Ditto
    public override void visit(TypePointer o)
    {
        if (!this.tpointer)
            return;
        if (this.tpointer == o)
            this.result = true;
        else
        {
            // It might be a pointer to a template parameter that we already
            // saw, so we need to recurse
            scope v = new ComponentVisitor(this.tpointer.next);
            o.next.visitObject(v);
            this.result = v.result;
        }
    }

    /// Ditto
    public override void visit(TypeIdentifier o)
    {
        /// Since we know they are at the same level, scope resolution will
        /// give us the same symbol, thus we can just compare ident.
        this.result = (this.tident && (this.tident.ident == o.ident));
    }

    /**
     * Overload which accepts a Namespace
     *
     * It is very common for large C++ projects to have multiple files sharing
     * the same `namespace`. If any D project adopts the same approach
     * (e.g. separating data structures from functions), it will lead to two
     * `Nspace` objects being instantiated, with different addresses.
     * At the same time, we cannot compare just any Dsymbol via identifier,
     * because it messes with templates.
     *
     * See_Also:
     *  https://issues.dlang.org/show_bug.cgi?id=18922
     *
     * Params:
     *   ns = C++ namespace to do substitution for
     */
    public override void visit(Nspace ns)
    {
        this.result = isNamespaceEqual(this.namespace, ns)
            || isNamespaceEqual(this.namespace2, ns);
    }

    /// Ditto
    public override void visit(CPPNamespaceDeclaration ns)
    {
        this.result = isNamespaceEqual(this.namespace, ns)
            || isNamespaceEqual(this.namespace2, ns);
    }
}

/// Transitional functions for `CPPNamespaceDeclaration` / `Nspace`
/// Remove when `Nspace` is removed.
private bool isNamespaceEqual (Nspace a, Nspace b)
{
    if (a is null || b is null)
        return false;
    return a.equals(b);
}

/// Ditto
private bool isNamespaceEqual (Nspace a, CPPNamespaceDeclaration b)
{
    return isNamespaceEqual(b, a);
}

/// Ditto
private bool isNamespaceEqual (CPPNamespaceDeclaration a, Nspace b, size_t idx = 0)
{
    if ((a is null) != (b is null))
        return false;
    if (!a.ident.equals(b.ident))
        return false;

    // We need to see if there's more ident enclosing
    if (auto pb = b.toParent().isNspace())
        return isNamespaceEqual(a.cppnamespace, pb);
    else
        return a.cppnamespace is null;
}

/// Returns:
///   Whether  two `CPPNamespaceDeclaration` are equals
private bool isNamespaceEqual (CPPNamespaceDeclaration a, CPPNamespaceDeclaration b) @safe
{
    if (a is null || b is null)
        return false;

    if ((a.cppnamespace is null) != (b.cppnamespace is null))
        return false;
    if (a.ident != b.ident)
        return false;
    return a.cppnamespace is null ? true : isNamespaceEqual(a.cppnamespace, b.cppnamespace);
}

/**
 * A container for ABI tags
 *
 * At its hearth, there is a sorted array of ABI tags having been written
 * already. ABI tags can be present on parameters, template parameters,
 * return value, and varaible. ABI tags for a given type needs to be written
 * sorted. When a function returns a type that has ABI tags, only the tags that
 * haven't been printed as part of the mangling (e.g. arguments) are written
 * directly after the function name.
 *
 * This means that:
 * ---
 * /++ C++ type definitions:
 * struct [[gnu::abi_tag("tag1")]] Struct1 {};
 * struct [[gnu::abi_tag("tag2")]] Struct2 {};
 * // Can also be: "tag2", "tag1", since tags are sorted.
 * struct [[gnu::abi_tag("tag1", "tag2")]] Struct3 {};
 * +/
 * // Functions definitions:
 * Struct3 func1 (Struct1);
 * Struct3 func2 (Struct2);
 * Struct3 func3 (Struct2, Struct1);
 * ---
 * Will be respectively pseudo-mangled (part of interest between stars) as:
 * "_Z4 func1 *B4tag2* ParamsMangling" (ParamsMangling includes tag1),
 * "_Z4 func2 *B4tag1* ParamsMangling" (ParamsMangling includes tag2),
 * "_Z4 func2 *B4tag1* ParamsMangling" (ParamsMangling includes both).
 *
 * This is why why need to keep a list of tags that were written,
 * and insert the missing one after parameter mangling has been written.
 * Since there's a lot of operations that are not easily doable in DMD
 * (since we can't use Phobos), this special container is implemented.
 */
private struct ABITagContainer
{
    private Array!StringExp written;

    static ArrayLiteralExp forSymbol (Dsymbol s)
    {
        if (!s)
            return null;
        // If this is a template instance, we want the declaration,
        // as that's where the UDAs are
        if (auto ti = s.isTemplateInstance())
            s = ti.tempdecl;
        if (!s.userAttribDecl || !s.userAttribDecl.atts)
            return null;

        foreach (exp; *s.userAttribDecl.atts)
        {
            if (UserAttributeDeclaration.isGNUABITag(exp))
                return (*exp.isStructLiteralExp().elements)[0]
                    .isArrayLiteralExp();
        }
        return null;
    }

    void writeSymbol(Dsymbol s, CppMangleVisitor self)
    {
        auto tale = forSymbol(s);
        if (!tale) return;
        if (self.substitute(tale))
            return;
        this.write(*self.buf, tale);
    }

    /**
     * Write an ArrayLiteralExp (expected to be an ABI tag) to the buffer
     *
     * Params:
     *   buf = Buffer to write mangling to
     *   ale = GNU ABI tag array literal expression, semantically analyzed
     */
    void write (ref OutBuffer buf, ArrayLiteralExp ale, bool skipKnown = false)
    {
        void writeElem (StringExp exp)
        {
            const tag = exp.peekString();
            buf.writestring("B");
            buf.print(tag.length);
            buf.writestring(tag);
        }

        bool match;
        foreach (exp; *ale.elements)
        {
            auto elem = exp.toStringExp();
            auto idx = closestIndex(this.written[], elem, match);
            if (!match)
            {
                writeElem(elem);
                this.written.insert(idx, elem);
            }
            else if (!skipKnown)
                writeElem(elem);
        }
    }
}

/**
 * Returns the closest index to to `exp` in `slice`
 *
 * Performs a binary search on `slice` (assumes `slice` is sorted),
 * and returns either `exp`'s index in `slice` if `exact` is `true`,
 * or the index at which `exp` can be inserted in `slice` if `exact is `false`.
 * Inserting `exp` at the return value will keep the array sorted.
 *
 * Params:
 *   slice = The sorted slice to search into
 *   exp   = The string expression to search for
 *   exact = If `true` on return, `exp` was found in `slice`
 *
 * Returns:
 *   Either the index to insert `exp` at (if `exact == false`),
 *   or the index of `exp` in `slice`.
 */
private size_t closestIndex (const(StringExp)[] slice, StringExp exp, out bool exact)
{
    if (!slice.length) return 0;

    const StringExp* first = slice.ptr;
    while (true)
    {
        int res = dstrcmp(exp.peekString(), slice[$ / 2].peekString());
        if (res == 0)
        {
            exact = true;
            return (&slice[$/2] - first);
        }

        if (slice.length == 1)
            return (slice.ptr - first) + (res > 0);
        slice = slice[(res > 0 ? $ / 2 : 0) .. (res > 0 ? $ : $ / 2)];
    }
}

//
unittest
{
    bool match;
    auto s1 = new StringExp(Loc.initial, "Amande");
    auto s2 = new StringExp(Loc.initial, "Baguette");
    auto s3 = new StringExp(Loc.initial, "Croissant");
    auto s4 = new StringExp(Loc.initial, "Framboises");
    auto s5 = new StringExp(Loc.initial, "Proscuitto");

    // Found, odd size
    assert(closestIndex([s1, s2, s3, s4, s5], s1, match) == 0 && match);
    assert(closestIndex([s1, s2, s3, s4, s5], s2, match) == 1 && match);
    assert(closestIndex([s1, s2, s3, s4, s5], s3, match) == 2 && match);
    assert(closestIndex([s1, s2, s3, s4, s5], s4, match) == 3 && match);
    assert(closestIndex([s1, s2, s3, s4, s5], s5, match) == 4 && match);

    // Not found, even size
    assert(closestIndex([s2, s3, s4, s5], s1, match) == 0 && !match);
    assert(closestIndex([s1, s3, s4, s5], s2, match) == 1 && !match);
    assert(closestIndex([s1, s2, s4, s5], s3, match) == 2 && !match);
    assert(closestIndex([s1, s2, s3, s5], s4, match) == 3 && !match);
    assert(closestIndex([s1, s2, s3, s4], s5, match) == 4 && !match);

    // Found, even size
    assert(closestIndex([s1, s2, s3, s4], s1, match) == 0 && match);
    assert(closestIndex([s1, s2, s3, s4], s2, match) == 1 && match);
    assert(closestIndex([s1, s2, s3, s4], s3, match) == 2 && match);
    assert(closestIndex([s1, s2, s3, s4], s4, match) == 3 && match);
    assert(closestIndex([s1, s3, s4, s5], s5, match) == 3 && match);

    // Not found, odd size
    assert(closestIndex([s2, s4, s5], s1, match) == 0 && !match);
    assert(closestIndex([s1, s4, s5], s2, match) == 1 && !match);
    assert(closestIndex([s1, s2, s4], s3, match) == 2 && !match);
    assert(closestIndex([s1, s3, s5], s4, match) == 2 && !match);
    assert(closestIndex([s1, s2, s4], s5, match) == 3 && !match);
}

/***
 * Visits the return type of a function and writes leftover ABI tags
 * Params:
 *   tf = Type of the function to mangle the return type of
 *   previous = already written ones
 *   toWrite = where to put StringExp's to be written
 */
private
void leftOver(TypeFunction tf, const(Array!StringExp)* previous, Array!StringExp* toWrite)
{
    extern(C++) final class LeftoverVisitor : Visitor
    {
        /// List of tags to write
        private Array!StringExp* toWrite;
        /// List of tags to ignore
        private const(Array!StringExp)* ignore;

        ///
        public this(const(Array!StringExp)* previous, Array!StringExp* toWrite) @safe
        {
            this.ignore = previous;
            this.toWrite = toWrite;
        }

        /// Reintroduce base class overloads
        public alias visit = Visitor.visit;

        /// Least specialized overload of each direct child of `RootObject`
        public override void visit(Dsymbol o)
        {
            auto ale = ABITagContainer.forSymbol(o);
            if (!ale) return;

            bool match;
            foreach (elem; *ale.elements)
            {
                auto se = elem.toStringExp();
                closestIndex((*this.ignore)[], se, match);
                if (match) continue;
                auto idx = closestIndex((*this.toWrite)[], se, match);
                if (!match)
                    (*this.toWrite).insert(idx, se);
            }
        }

        /// Ditto
        public override void visit(Type o)
        {
            if (auto sym = o.toDsymbol(null))
                sym.accept(this);
        }

        /// Composite type
        public override void visit(TypePointer o)
        {
            o.next.accept(this);
        }

        public override void visit(TypeReference o)
        {
            o.next.accept(this);
        }
    }

    scope remainingVisitor = new LeftoverVisitor(previous, toWrite);
    tf.next.accept(remainingVisitor);
}
