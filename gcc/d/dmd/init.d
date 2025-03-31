/**
 * Defines initializers of variables, e.g. the array literal in `int[3] x = [0, 1, 2]`.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/init.d, _init.d)
 * Documentation:  https://dlang.org/phobos/dmd_init.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/init.d
 */

module dmd.init;

import core.stdc.stdio;

import dmd.arraytypes;
import dmd.astenums;
import dmd.ast_node;
import dmd.expression;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.rootobject;
import dmd.visitor;

enum NeedInterpret : int
{
    INITnointerpret,
    INITinterpret,
}

alias INITnointerpret = NeedInterpret.INITnointerpret;
alias INITinterpret = NeedInterpret.INITinterpret;

/***********************************************************
 */
extern (C++) class Initializer : ASTNode
{
    Loc loc;
    InitKind kind;
    bool semanticDone = false; /// initializerSemantic has been run on this

    override DYNCAST dyncast() const
    {
        return DYNCAST.initializer;
    }


    extern (D) this(Loc loc, InitKind kind) @safe
    {
        this.loc = loc;
        this.kind = kind;
    }

    final inout(ErrorInitializer) isErrorInitializer() inout @nogc nothrow pure @trusted
    {
        // Use void* cast to skip dynamic casting call
        return kind == InitKind.error ? cast(inout ErrorInitializer)cast(void*)this : null;
    }

    final inout(VoidInitializer) isVoidInitializer() inout @nogc nothrow pure @trusted
    {
        return kind == InitKind.void_ ? cast(inout VoidInitializer)cast(void*)this : null;
    }

    final inout(DefaultInitializer) isDefaultInitializer() inout @nogc nothrow pure @trusted
    {
        return kind == InitKind.default_ ? cast(inout DefaultInitializer)cast(void*)this : null;
    }

    final inout(StructInitializer) isStructInitializer() inout @nogc nothrow pure @trusted
    {
        return kind == InitKind.struct_ ? cast(inout StructInitializer)cast(void*)this : null;
    }

    final inout(ArrayInitializer) isArrayInitializer() inout @nogc nothrow pure @trusted
    {
        return kind == InitKind.array ? cast(inout ArrayInitializer)cast(void*)this : null;
    }

    final inout(ExpInitializer) isExpInitializer() inout @nogc nothrow pure @trusted
    {
        return kind == InitKind.exp ? cast(inout ExpInitializer)cast(void*)this : null;
    }

    final inout(CInitializer) isCInitializer() inout @nogc nothrow pure @trusted
    {
        return kind == InitKind.C_ ? cast(inout CInitializer)cast(void*)this : null;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class VoidInitializer : Initializer
{
    Type type;      // type that this will initialize to

    extern (D) this(Loc loc) @safe
    {
        super(loc, InitKind.void_);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The C23 default initializer `{ }`
 */
extern (C++) final class DefaultInitializer : Initializer
{
    Type type;      // type that this will initialize to

    extern (D) this(Loc loc) @safe
    {
        super(loc, InitKind.default_);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class ErrorInitializer : Initializer
{
    extern (D) this() @safe
    {
        super(Loc.initial, InitKind.error);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class StructInitializer : Initializer
{
    Identifiers field;      // of Identifier *'s
    Initializers value;     // parallel array of Initializer *'s

    extern (D) this(Loc loc)
    {
        super(loc, InitKind.struct_);
    }

    extern (D) void addInit(Identifier field, Initializer value)
    {
        //printf("StructInitializer::addInit(field = %p, value = %p)\n", field, value);
        this.field.push(field);
        this.value.push(value);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class ArrayInitializer : Initializer
{
    Expressions index;      // indices
    Initializers value;     // of Initializer *'s
    uint dim;               // length of array being initialized
    Type type;              // type that array will be used to initialize
    bool isCarray;          // C array semantics

    extern (D) this(Loc loc)
    {
        super(loc, InitKind.array);
    }

    extern (D) void addInit(Expression index, Initializer value)
    {
        this.index.push(index);
        this.value.push(value);
        dim = 0;
        type = null;
    }

    bool isAssociativeArray() const pure
    {
        foreach (idx; index)
        {
            if (idx)
                return true;
        }
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class ExpInitializer : Initializer
{
    bool expandTuples;
    Expression exp;

    extern (D) this(Loc loc, Expression exp) @safe
    {
        super(loc, InitKind.exp);
        this.exp = exp;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/*********************************************
 * Holds the `designator` for C initializers
 */
struct Designator
{
    Expression exp;     /// [ constant-expression ]
    Identifier ident;   /// . identifier

    this(Expression exp) @safe { this.exp = exp; }
    this(Identifier ident) @safe  { this.ident = ident; }
}

/*********************************************
 * Holds the `designation (opt) initializer` for C initializers
 */
struct DesigInit
{
    Designators* designatorList; /// designation (opt)
    Initializer initializer;     /// initializer
}

/********************************
 * C11 6.7.9 Initialization
 * Represents the C initializer-list
 */
extern (C++) final class CInitializer : Initializer
{
    DesigInits initializerList; /// initializer-list
    Type type;              /// type that array will be used to initialize

    extern (D) this(Loc loc)
    {
        super(loc, InitKind.C_);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/****************************************
 * Copy the AST for Initializer.
 * Params:
 *      inx = Initializer AST to copy
 * Returns:
 *      the copy
 */
Initializer syntaxCopy(Initializer inx)
{
    static Initializer visitVoid(VoidInitializer vi)
    {
        return new VoidInitializer(vi.loc);
    }

    static Initializer visitDefault(DefaultInitializer vi)
    {
        return new DefaultInitializer(vi.loc);
    }

    static Initializer visitError(ErrorInitializer vi)
    {
        return vi;
    }

    static Initializer visitExp(ExpInitializer vi)
    {
        return new ExpInitializer(vi.loc, vi.exp.syntaxCopy());
    }

    static Initializer visitStruct(StructInitializer vi)
    {
        auto si = new StructInitializer(vi.loc);
        assert(vi.field.length == vi.value.length);
        si.field.setDim(vi.field.length);
        si.value.setDim(vi.value.length);
        foreach (const i; 0 .. vi.field.length)
        {
            si.field[i] = vi.field[i];
            si.value[i] = vi.value[i].syntaxCopy();
        }
        return si;
    }

    static Initializer visitArray(ArrayInitializer vi)
    {
        auto ai = new ArrayInitializer(vi.loc);
        assert(vi.index.length == vi.value.length);
        ai.index.setDim(vi.index.length);
        ai.value.setDim(vi.value.length);
        foreach (const i; 0 .. vi.value.length)
        {
            ai.index[i] = vi.index[i] ? vi.index[i].syntaxCopy() : null;
            ai.value[i] = vi.value[i].syntaxCopy();
        }
        return ai;
    }

    static Initializer visitC(CInitializer vi)
    {
        auto ci = new CInitializer(vi.loc);
        ci.initializerList.setDim(vi.initializerList.length);
        foreach (const i; 0 .. vi.initializerList.length)
        {
            DesigInit* cdi = &ci.initializerList[i];
            DesigInit* vdi = &ci.initializerList[i];
            cdi.initializer = vdi.initializer.syntaxCopy();
            if (vdi.designatorList)
            {
                cdi.designatorList = new Designators();
                cdi.designatorList.setDim(vdi.designatorList.length);
                foreach (const j; 0 .. vdi.designatorList.length)
                {
                    Designator* cdid = &(*cdi.designatorList)[j];
                    Designator* vdid = &(*vdi.designatorList)[j];
                    cdid.exp = vdid.exp ? vdid.exp.syntaxCopy() : null;
                    cdid.ident = vdid.ident;
                }
            }
        }
        return ci;
    }

    mixin VisitInitializer!Initializer visit;
    return visit.VisitInitializer(inx);
}

/***********************************************************
 * Visit each Initializer in init. Call a function visit%s(init) for
 * each node, where %s is the op of the node. Otherwise call visitDefault(init)
 * for that node. If the visit function returns R.init, continue
 * visiting each node, otherwise return the value of R.
 * Params:
 *      Result = return type
 *      init = Initializer tree to traverse
 * Returns:
 *      Result.init for continue, value of type Result for early exit
 */

mixin template VisitInitializer(Result)
{
    Result VisitInitializer(Initializer init)
    {
        final switch (init.kind)
        {
            case InitKind.void_:    mixin(visitCase("Void"));    break;
            case InitKind.default_: mixin(visitCase("Default")); break;
            case InitKind.error:    mixin(visitCase("Error"));   break;
            case InitKind.struct_:  mixin(visitCase("Struct"));  break;
            case InitKind.array:    mixin(visitCase("Array"));   break;
            case InitKind.exp:      mixin(visitCase("Exp"));     break;
            case InitKind.C_:       mixin(visitCase("C"));       break;
        }
        static if (is(Result == void)) { } else
            return Result.init;
    }
}

/****************************************
 * CTFE-only helper function for VisitInitializer.
 * Params:
 *      handler = string for the name of the visit handler
 * Returns: boilerplate code for a case
 */
string visitCase(string handler) pure @safe
{
    if (__ctfe)
    {
        return
            "
            auto ix = init.is"~handler~"Initializer();
            static if (is(Result == void))
                visit"~handler~"(ix);
            else
            {
                Result r = visit"~handler~"(ix);
                if (r !is Result.init)
                    return r;
            }
            ";
    }
    assert(0);
}
