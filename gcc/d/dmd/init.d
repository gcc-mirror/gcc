/**
 * Defines initializers of variables, e.g. the array literal in `int[3] x = [0, 1, 2]`.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/init.d, _init.d)
 * Documentation:  https://dlang.org/phobos/dmd_init.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/init.d
 */

module dmd.init;

import core.stdc.stdio;
import core.checkedint;

import dmd.arraytypes;
import dmd.astenums;
import dmd.ast_node;
import dmd.dsymbol;
import dmd.expression;
import dmd.globals;
import dmd.hdrgen;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.common.outbuffer;
import dmd.root.rootobject;
import dmd.tokens;
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

    override DYNCAST dyncast() const
    {
        return DYNCAST.initializer;
    }


    extern (D) this(const ref Loc loc, InitKind kind)
    {
        this.loc = loc;
        this.kind = kind;
    }

    override final const(char)* toChars() const
    {
        OutBuffer buf;
        HdrGenState hgs;
        .toCBuffer(this, &buf, &hgs);
        return buf.extractChars();
    }

    final inout(ErrorInitializer) isErrorInitializer() inout @nogc nothrow pure
    {
        // Use void* cast to skip dynamic casting call
        return kind == InitKind.error ? cast(inout ErrorInitializer)cast(void*)this : null;
    }

    final inout(VoidInitializer) isVoidInitializer() inout @nogc nothrow pure
    {
        return kind == InitKind.void_ ? cast(inout VoidInitializer)cast(void*)this : null;
    }

    final inout(StructInitializer) isStructInitializer() inout @nogc nothrow pure
    {
        return kind == InitKind.struct_ ? cast(inout StructInitializer)cast(void*)this : null;
    }

    final inout(ArrayInitializer) isArrayInitializer() inout @nogc nothrow pure
    {
        return kind == InitKind.array ? cast(inout ArrayInitializer)cast(void*)this : null;
    }

    final inout(ExpInitializer) isExpInitializer() inout @nogc nothrow pure
    {
        return kind == InitKind.exp ? cast(inout ExpInitializer)cast(void*)this : null;
    }

    final inout(CInitializer) isCInitializer() inout @nogc nothrow pure
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

    extern (D) this(const ref Loc loc)
    {
        super(loc, InitKind.void_);
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
    extern (D) this()
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

    extern (D) this(const ref Loc loc)
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
    bool sem;               // true if semantic() is run
    bool isCarray;          // C array semantics

    extern (D) this(const ref Loc loc)
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

    extern (D) this(const ref Loc loc, Expression exp)
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

    this(Expression exp) { this.exp = exp; }
    this(Identifier ident) { this.ident = ident; }
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
    bool sem;               /// true if semantic() is run

    extern (D) this(const ref Loc loc)
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
    static Initializer copyStruct(StructInitializer vi)
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

    static Initializer copyArray(ArrayInitializer vi)
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

    static Initializer copyC(CInitializer vi)
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

    final switch (inx.kind)
    {
        case InitKind.void_:   return new VoidInitializer(inx.loc);
        case InitKind.error:   return inx;
        case InitKind.struct_: return copyStruct(cast(StructInitializer)inx);
        case InitKind.array:   return copyArray(cast(ArrayInitializer)inx);
        case InitKind.exp:     return new ExpInitializer(inx.loc, (cast(ExpInitializer)inx).exp.syntaxCopy());
        case InitKind.C_:      return copyC(cast(CInitializer)inx);
    }
}
