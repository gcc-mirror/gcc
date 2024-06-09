/**
 * Provides a visitor class visiting all AST nodes present in the compiler.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/visitor.d, _visitor.d)
 * Documentation:  https://dlang.org/phobos/dmd_visitor.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/visitor.d
 */

module dmd.visitor;

import dmd.astcodegen;
import dmd.astenums;
import dmd.parsetimevisitor;
import dmd.tokens;
import dmd.transitivevisitor;
import dmd.expression;
import dmd.rootobject;

/**
 * Classic Visitor class which implements visit methods for all the AST
 * nodes present in the compiler. The visit methods for AST nodes
 * created at parse time are inherited while the visiting methods
 * for AST nodes created at semantic time are implemented.
 */
extern (C++) class Visitor : ParseTimeVisitor!ASTCodegen
{
    alias visit = ParseTimeVisitor!ASTCodegen.visit;
public:
    void visit(ASTCodegen.ErrorStatement s) { visit(cast(ASTCodegen.Statement)s); }
    void visit(ASTCodegen.PeelStatement s) { visit(cast(ASTCodegen.Statement)s); }
    void visit(ASTCodegen.UnrolledLoopStatement s) { visit(cast(ASTCodegen.Statement)s); }
    void visit(ASTCodegen.SwitchErrorStatement s) { visit(cast(ASTCodegen.Statement)s); }
    void visit(ASTCodegen.DebugStatement s) { visit(cast(ASTCodegen.Statement)s); }
    void visit(ASTCodegen.DtorExpStatement s) { visit(cast(ASTCodegen.ExpStatement)s); }
    void visit(ASTCodegen.ForwardingStatement s) { visit(cast(ASTCodegen.Statement)s); }
    void visit(ASTCodegen.OverloadSet s) { visit(cast(ASTCodegen.Dsymbol)s); }
    void visit(ASTCodegen.LabelDsymbol s) { visit(cast(ASTCodegen.Dsymbol)s); }
    void visit(ASTCodegen.WithScopeSymbol s) { visit(cast(ASTCodegen.ScopeDsymbol)s); }
    void visit(ASTCodegen.ArrayScopeSymbol s) { visit(cast(ASTCodegen.ScopeDsymbol)s); }
    void visit(ASTCodegen.OverDeclaration s) { visit(cast(ASTCodegen.Declaration)s); }
    void visit(ASTCodegen.SymbolDeclaration s) { visit(cast(ASTCodegen.Declaration)s); }
    void visit(ASTCodegen.ForwardingAttribDeclaration s) { visit(cast(ASTCodegen.AttribDeclaration)s); }
    void visit(ASTCodegen.ThisDeclaration s) { visit(cast(ASTCodegen.VarDeclaration)s); }
    void visit(ASTCodegen.TypeInfoDeclaration s) { visit(cast(ASTCodegen.VarDeclaration)s); }
    void visit(ASTCodegen.TypeInfoStructDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoClassDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoInterfaceDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoPointerDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoArrayDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoStaticArrayDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoAssociativeArrayDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoEnumDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoFunctionDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoDelegateDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoTupleDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoConstDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoInvariantDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoSharedDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoWildDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.TypeInfoVectorDeclaration s) { visit(cast(ASTCodegen.TypeInfoDeclaration)s); }
    void visit(ASTCodegen.FuncAliasDeclaration s) { visit(cast(ASTCodegen.FuncDeclaration)s); }
    void visit(ASTCodegen.ErrorInitializer i) { visit(cast(ASTCodegen.Initializer)i); }
    void visit(ASTCodegen.ErrorExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.ComplexExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.StructLiteralExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.CompoundLiteralExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.ObjcClassReferenceExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.SymOffExp e) { visit(cast(ASTCodegen.SymbolExp)e); }
    void visit(ASTCodegen.OverExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.HaltExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.DotTemplateExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.DotVarExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.DelegateExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.DotTypeExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.VectorExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.VectorArrayExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.SliceExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.ArrayLengthExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.DelegatePtrExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.DelegateFuncptrExp e) { visit(cast(ASTCodegen.UnaExp)e); }
    void visit(ASTCodegen.DotExp e) { visit(cast(ASTCodegen.BinExp)e); }
    void visit(ASTCodegen.IndexExp e) { visit(cast(ASTCodegen.BinExp)e); }
    void visit(ASTCodegen.ConstructExp e) { visit(cast(ASTCodegen.AssignExp)e); }
    void visit(ASTCodegen.BlitExp e) { visit(cast(ASTCodegen.AssignExp)e); }
    void visit(ASTCodegen.RemoveExp e) { visit(cast(ASTCodegen.BinExp)e); }
    void visit(ASTCodegen.ClassReferenceExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.VoidInitExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.ThrownExceptionExp e) { visit(cast(ASTCodegen.Expression)e); }
    void visit(ASTCodegen.LoweredAssignExp e) { visit(cast(ASTCodegen.AssignExp)e); }
}

/**
 * The PermissiveVisitor overrides the root AST nodes with
 * empty visiting methods.
 */
extern (C++) class SemanticTimePermissiveVisitor : Visitor
{
    alias visit = Visitor.visit;

    override void visit(ASTCodegen.Dsymbol){}
    override void visit(ASTCodegen.Parameter){}
    override void visit(ASTCodegen.Statement){}
    override void visit(ASTCodegen.Type){}
    override void visit(ASTCodegen.Expression){}
    override void visit(ASTCodegen.TemplateParameter){}
    override void visit(ASTCodegen.Condition){}
    override void visit(ASTCodegen.Initializer){}
}

/**
 * The TransitiveVisitor implements the AST traversal logic for all AST nodes.
 */
extern (C++) class SemanticTimeTransitiveVisitor : SemanticTimePermissiveVisitor
{
    alias visit = SemanticTimePermissiveVisitor.visit;

    mixin ParseVisitMethods!ASTCodegen __methods;
    alias visit = __methods.visit;

    override void visit(ASTCodegen.PeelStatement s)
    {
        if (s.s)
            s.s.accept(this);
    }

    override void visit(ASTCodegen.UnrolledLoopStatement s)
    {
        foreach(sx; *s.statements)
        {
            if (sx)
                sx.accept(this);
        }
    }

    override void visit(ASTCodegen.DebugStatement s)
    {
        if (s.statement)
            s.statement.accept(this);
    }

    override void visit(ASTCodegen.ForwardingStatement s)
    {
        if (s.statement)
            s.statement.accept(this);
    }

    override void visit(ASTCodegen.StructLiteralExp e)
    {
        // CTFE can generate struct literals that contain an AddrExp pointing to themselves,
        // need to avoid infinite recursion.
        if (!(e.stageflags & stageToCBuffer))
        {
            const old = e.stageflags;
            e.stageflags |= stageToCBuffer;
            foreach (el; *e.elements)
                if (el)
                    el.accept(this);
            e.stageflags = old;
        }
    }

    override void visit(ASTCodegen.CompoundLiteralExp e)
    {
        if (e.initializer)
            e.initializer.accept(this);
    }

    override void visit(ASTCodegen.DotTemplateExp e)
    {
        e.e1.accept(this);
    }

    override void visit(ASTCodegen.DotVarExp e)
    {
        e.e1.accept(this);
    }

    override void visit(ASTCodegen.DelegateExp e)
    {
        if (!e.func.isNested() || e.func.needThis())
            e.e1.accept(this);
    }

    override void visit(ASTCodegen.DotTypeExp e)
    {
        e.e1.accept(this);
    }

    override void visit(ASTCodegen.VectorExp e)
    {
        visitType(e.to);
        e.e1.accept(this);
    }

    override void visit(ASTCodegen.VectorArrayExp e)
    {
        e.e1.accept(this);
    }

    override void visit(ASTCodegen.SliceExp e)
    {
        e.e1.accept(this);
        if (e.upr)
            e.upr.accept(this);
        if (e.lwr)
            e.lwr.accept(this);
    }

    override void visit(ASTCodegen.ArrayLengthExp e)
    {
        e.e1.accept(this);
    }

    override void visit(ASTCodegen.DelegatePtrExp e)
    {
        e.e1.accept(this);
    }

    override void visit(ASTCodegen.DelegateFuncptrExp e)
    {
        e.e1.accept(this);
    }

    override void visit(ASTCodegen.DotExp e)
    {
        e.e1.accept(this);
        e.e2.accept(this);
    }

    override void visit(ASTCodegen.IndexExp e)
    {
        e.e1.accept(this);
        e.e2.accept(this);
    }

    override void visit(ASTCodegen.RemoveExp e)
    {
        e.e1.accept(this);
        e.e2.accept(this);
    }

    override void visit(ASTCodegen.LoweredAssignExp e)
    {
        e.lowering.accept(this);
        visit(cast(AssignExp)e);
    }
}

extern (C++) class StoppableVisitor : Visitor
{
    alias visit = Visitor.visit;
public:
    bool stop;

    final extern (D) this() scope @safe
    {
    }
}
