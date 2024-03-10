/**
 * Generate $(LINK2 https://dlang.org/dmd-windows.html#interface-files, D interface files).
 *
 * Also used to convert AST nodes to D code in general, e.g. for error messages or `printf` debugging.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/hdrgen.d, _hdrgen.d)
 * Documentation:  https://dlang.org/phobos/dmd_hdrgen.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/hdrgen.d
 */

module dmd.hdrgen;

import core.stdc.ctype;
import core.stdc.stdio;
import core.stdc.string;
import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.cond;
import dmd.ctfeexpr;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dimport;
import dmd.dmodule;
import dmd.doc;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.dversion;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.mtype;
import dmd.nspace;
import dmd.parse;
import dmd.root.complex;
import dmd.root.ctfloat;
import dmd.common.outbuffer;
import dmd.root.rootobject;
import dmd.root.string;
import dmd.statement;
import dmd.staticassert;
import dmd.target;
import dmd.tokens;
import dmd.utils;
import dmd.visitor;

struct HdrGenState
{
    bool hdrgen;        /// true if generating header file
    bool ddoc;          /// true if generating Ddoc file
    bool fullDump;      /// true if generating a full AST dump file

    bool fullQual;      /// fully qualify types when printing
    int tpltMember;
    int autoMember;
    int forStmtInit;
    int insideFuncBody;
    int insideAggregate;

    bool declstring; // set while declaring alias for string,wstring or dstring
    EnumDeclaration inEnumDecl;
}

enum TEST_EMIT_ALL = 0;

extern (C++) void genhdrfile(Module m)
{
    OutBuffer buf;
    buf.doindent = 1;
    buf.printf("// D import file generated from '%s'", m.srcfile.toChars());
    buf.writenl();
    HdrGenState hgs;
    hgs.hdrgen = true;
    toCBuffer(m, &buf, &hgs);
    writeFile(m.loc, m.hdrfile.toString(), buf[]);
}

/**
 * Dumps the full contents of module `m` to `buf`.
 * Params:
 *   buf = buffer to write to.
 *   m = module to visit all members of.
 */
extern (C++) void moduleToBuffer(OutBuffer* buf, Module m)
{
    HdrGenState hgs;
    hgs.fullDump = true;
    toCBuffer(m, buf, &hgs);
}

void moduleToBuffer2(Module m, OutBuffer* buf, HdrGenState* hgs)
{
    if (m.md)
    {
        if (m.userAttribDecl)
        {
            buf.writestring("@(");
            argsToBuffer(m.userAttribDecl.atts, buf, hgs);
            buf.writeByte(')');
            buf.writenl();
        }
        if (m.md.isdeprecated)
        {
            if (m.md.msg)
            {
                buf.writestring("deprecated(");
                m.md.msg.expressionToBuffer(buf, hgs);
                buf.writestring(") ");
            }
            else
                buf.writestring("deprecated ");
        }
        buf.writestring("module ");
        buf.writestring(m.md.toChars());
        buf.writeByte(';');
        buf.writenl();
    }

    foreach (s; *m.members)
    {
        s.dsymbolToBuffer(buf, hgs);
    }
}

private void statementToBuffer(Statement s, OutBuffer* buf, HdrGenState* hgs)
{
    scope v = new StatementPrettyPrintVisitor(buf, hgs);
    s.accept(v);
}

private extern (C++) final class StatementPrettyPrintVisitor : Visitor
{
    alias visit = Visitor.visit;
public:
    OutBuffer* buf;
    HdrGenState* hgs;

    extern (D) this(OutBuffer* buf, HdrGenState* hgs) scope
    {
        this.buf = buf;
        this.hgs = hgs;
    }

    override void visit(Statement s)
    {
        buf.writestring("Statement::toCBuffer()");
        buf.writenl();
        assert(0);
    }

    override void visit(ErrorStatement s)
    {
        buf.writestring("__error__");
        buf.writenl();
    }

    override void visit(ExpStatement s)
    {
        if (s.exp && s.exp.op == EXP.declaration &&
            (cast(DeclarationExp)s.exp).declaration)
        {
            // bypass visit(DeclarationExp)
            (cast(DeclarationExp)s.exp).declaration.dsymbolToBuffer(buf, hgs);
            return;
        }
        if (s.exp)
            s.exp.expressionToBuffer(buf, hgs);
        buf.writeByte(';');
        if (!hgs.forStmtInit)
            buf.writenl();
    }

    override void visit(CompileStatement s)
    {
        buf.writestring("mixin(");
        argsToBuffer(s.exps, buf, hgs, null);
        buf.writestring(");");
        if (!hgs.forStmtInit)
            buf.writenl();
    }

    override void visit(CompoundStatement s)
    {
        foreach (sx; *s.statements)
        {
            if (sx)
                sx.accept(this);
        }
    }

    override void visit(CompoundDeclarationStatement s)
    {
        bool anywritten = false;
        foreach (sx; *s.statements)
        {
            auto ds = sx ? sx.isExpStatement() : null;
            if (ds && ds.exp.op == EXP.declaration)
            {
                auto d = (cast(DeclarationExp)ds.exp).declaration;
                assert(d.isDeclaration());
                if (auto v = d.isVarDeclaration())
                {
                    scope ppv = new DsymbolPrettyPrintVisitor(buf, hgs);
                    ppv.visitVarDecl(v, anywritten);
                }
                else
                    d.dsymbolToBuffer(buf, hgs);
                anywritten = true;
            }
        }
        buf.writeByte(';');
        if (!hgs.forStmtInit)
            buf.writenl();
    }

    override void visit(UnrolledLoopStatement s)
    {
        buf.writestring("/*unrolled*/ {");
        buf.writenl();
        buf.level++;
        foreach (sx; *s.statements)
        {
            if (sx)
                sx.accept(this);
        }
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
    }

    override void visit(ScopeStatement s)
    {
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        if (s.statement)
            s.statement.accept(this);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
    }

    override void visit(WhileStatement s)
    {
        buf.writestring("while (");
        if (auto p = s.param)
        {
            // Print condition assignment
            StorageClass stc = p.storageClass;
            if (!p.type && !stc)
                stc = STC.auto_;
            if (stcToBuffer(buf, stc))
                buf.writeByte(' ');
            if (p.type)
                typeToBuffer(p.type, p.ident, buf, hgs);
            else
                buf.writestring(p.ident.toString());
            buf.writestring(" = ");
        }
        s.condition.expressionToBuffer(buf, hgs);
        buf.writeByte(')');
        buf.writenl();
        if (s._body)
            s._body.accept(this);
    }

    override void visit(DoStatement s)
    {
        buf.writestring("do");
        buf.writenl();
        if (s._body)
            s._body.accept(this);
        buf.writestring("while (");
        s.condition.expressionToBuffer(buf, hgs);
        buf.writestring(");");
        buf.writenl();
    }

    override void visit(ForStatement s)
    {
        buf.writestring("for (");
        if (s._init)
        {
            hgs.forStmtInit++;
            s._init.accept(this);
            hgs.forStmtInit--;
        }
        else
            buf.writeByte(';');
        if (s.condition)
        {
            buf.writeByte(' ');
            s.condition.expressionToBuffer(buf, hgs);
        }
        buf.writeByte(';');
        if (s.increment)
        {
            buf.writeByte(' ');
            s.increment.expressionToBuffer(buf, hgs);
        }
        buf.writeByte(')');
        buf.writenl();
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        if (s._body)
            s._body.accept(this);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
    }

    private void foreachWithoutBody(ForeachStatement s)
    {
        buf.writestring(Token.toString(s.op));
        buf.writestring(" (");
        foreach (i, p; *s.parameters)
        {
            if (i)
                buf.writestring(", ");
            if (stcToBuffer(buf, p.storageClass))
                buf.writeByte(' ');
            if (p.type)
                typeToBuffer(p.type, p.ident, buf, hgs);
            else
                buf.writestring(p.ident.toString());
        }
        buf.writestring("; ");
        s.aggr.expressionToBuffer(buf, hgs);
        buf.writeByte(')');
        buf.writenl();
    }

    override void visit(ForeachStatement s)
    {
        foreachWithoutBody(s);
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        if (s._body)
            s._body.accept(this);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
    }

    private void foreachRangeWithoutBody(ForeachRangeStatement s)
    {
        buf.writestring(Token.toString(s.op));
        buf.writestring(" (");
        if (s.prm.type)
            typeToBuffer(s.prm.type, s.prm.ident, buf, hgs);
        else
            buf.writestring(s.prm.ident.toString());
        buf.writestring("; ");
        s.lwr.expressionToBuffer(buf, hgs);
        buf.writestring(" .. ");
        s.upr.expressionToBuffer(buf, hgs);
        buf.writeByte(')');
        buf.writenl();
    }

    override void visit(ForeachRangeStatement s)
    {
        foreachRangeWithoutBody(s);
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        if (s._body)
            s._body.accept(this);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
    }

    override void visit(StaticForeachStatement s)
    {
        buf.writestring("static ");
        if (s.sfe.aggrfe)
        {
            visit(s.sfe.aggrfe);
        }
        else
        {
            assert(s.sfe.rangefe);
            visit(s.sfe.rangefe);
        }
    }

    override void visit(ForwardingStatement s)
    {
        s.statement.accept(this);
    }

    override void visit(IfStatement s)
    {
        buf.writestring("if (");
        if (Parameter p = s.prm)
        {
            StorageClass stc = p.storageClass;
            if (!p.type && !stc)
                stc = STC.auto_;
            if (stcToBuffer(buf, stc))
                buf.writeByte(' ');
            if (p.type)
                typeToBuffer(p.type, p.ident, buf, hgs);
            else
                buf.writestring(p.ident.toString());
            buf.writestring(" = ");
        }
        s.condition.expressionToBuffer(buf, hgs);
        buf.writeByte(')');
        buf.writenl();
        if (s.ifbody.isScopeStatement())
        {
            s.ifbody.accept(this);
        }
        else
        {
            buf.level++;
            s.ifbody.accept(this);
            buf.level--;
        }
        if (s.elsebody)
        {
            buf.writestring("else");
            if (!s.elsebody.isIfStatement())
            {
                buf.writenl();
            }
            else
            {
                buf.writeByte(' ');
            }
            if (s.elsebody.isScopeStatement() || s.elsebody.isIfStatement())
            {
                s.elsebody.accept(this);
            }
            else
            {
                buf.level++;
                s.elsebody.accept(this);
                buf.level--;
            }
        }
    }

    override void visit(ConditionalStatement s)
    {
        s.condition.conditionToBuffer(buf, hgs);
        buf.writenl();
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        if (s.ifbody)
            s.ifbody.accept(this);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
        if (s.elsebody)
        {
            buf.writestring("else");
            buf.writenl();
            buf.writeByte('{');
            buf.level++;
            buf.writenl();
            s.elsebody.accept(this);
            buf.level--;
            buf.writeByte('}');
        }
        buf.writenl();
    }

    override void visit(PragmaStatement s)
    {
        buf.writestring("pragma (");
        buf.writestring(s.ident.toString());
        if (s.args && s.args.length)
        {
            buf.writestring(", ");
            argsToBuffer(s.args, buf, hgs);
        }
        buf.writeByte(')');
        if (s._body)
        {
            buf.writenl();
            buf.writeByte('{');
            buf.writenl();
            buf.level++;
            s._body.accept(this);
            buf.level--;
            buf.writeByte('}');
            buf.writenl();
        }
        else
        {
            buf.writeByte(';');
            buf.writenl();
        }
    }

    override void visit(StaticAssertStatement s)
    {
        s.sa.dsymbolToBuffer(buf, hgs);
    }

    override void visit(SwitchStatement s)
    {
        buf.writestring(s.isFinal ? "final switch (" : "switch (");
        s.condition.expressionToBuffer(buf, hgs);
        buf.writeByte(')');
        buf.writenl();
        if (s._body)
        {
            if (!s._body.isScopeStatement())
            {
                buf.writeByte('{');
                buf.writenl();
                buf.level++;
                s._body.accept(this);
                buf.level--;
                buf.writeByte('}');
                buf.writenl();
            }
            else
            {
                s._body.accept(this);
            }
        }
    }

    override void visit(CaseStatement s)
    {
        buf.writestring("case ");
        s.exp.expressionToBuffer(buf, hgs);
        buf.writeByte(':');
        buf.writenl();
        s.statement.accept(this);
    }

    override void visit(CaseRangeStatement s)
    {
        buf.writestring("case ");
        s.first.expressionToBuffer(buf, hgs);
        buf.writestring(": .. case ");
        s.last.expressionToBuffer(buf, hgs);
        buf.writeByte(':');
        buf.writenl();
        s.statement.accept(this);
    }

    override void visit(DefaultStatement s)
    {
        buf.writestring("default:");
        buf.writenl();
        s.statement.accept(this);
    }

    override void visit(GotoDefaultStatement s)
    {
        buf.writestring("goto default;");
        buf.writenl();
    }

    override void visit(GotoCaseStatement s)
    {
        buf.writestring("goto case");
        if (s.exp)
        {
            buf.writeByte(' ');
            s.exp.expressionToBuffer(buf, hgs);
        }
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(SwitchErrorStatement s)
    {
        buf.writestring("SwitchErrorStatement::toCBuffer()");
        buf.writenl();
    }

    override void visit(ReturnStatement s)
    {
        buf.writestring("return ");
        if (s.exp)
            s.exp.expressionToBuffer(buf, hgs);
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(BreakStatement s)
    {
        buf.writestring("break");
        if (s.ident)
        {
            buf.writeByte(' ');
            buf.writestring(s.ident.toString());
        }
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(ContinueStatement s)
    {
        buf.writestring("continue");
        if (s.ident)
        {
            buf.writeByte(' ');
            buf.writestring(s.ident.toString());
        }
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(SynchronizedStatement s)
    {
        buf.writestring("synchronized");
        if (s.exp)
        {
            buf.writeByte('(');
            s.exp.expressionToBuffer(buf, hgs);
            buf.writeByte(')');
        }
        if (s._body)
        {
            buf.writeByte(' ');
            s._body.accept(this);
        }
    }

    override void visit(WithStatement s)
    {
        buf.writestring("with (");
        s.exp.expressionToBuffer(buf, hgs);
        buf.writestring(")");
        buf.writenl();
        if (s._body)
            s._body.accept(this);
    }

    override void visit(TryCatchStatement s)
    {
        buf.writestring("try");
        buf.writenl();
        if (s._body)
        {
            if (s._body.isScopeStatement())
            {
                s._body.accept(this);
            }
            else
            {
                buf.level++;
                s._body.accept(this);
                buf.level--;
            }
        }
        foreach (c; *s.catches)
        {
            visit(c);
        }
    }

    override void visit(TryFinallyStatement s)
    {
        buf.writestring("try");
        buf.writenl();
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        s._body.accept(this);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
        buf.writestring("finally");
        buf.writenl();
        if (s.finalbody.isScopeStatement())
        {
            s.finalbody.accept(this);
        }
        else
        {
            buf.level++;
            s.finalbody.accept(this);
            buf.level--;
        }
    }

    override void visit(ScopeGuardStatement s)
    {
        buf.writestring(Token.toString(s.tok));
        buf.writeByte(' ');
        if (s.statement)
            s.statement.accept(this);
    }

    override void visit(ThrowStatement s)
    {
        buf.writestring("throw ");
        s.exp.expressionToBuffer(buf, hgs);
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(DebugStatement s)
    {
        if (s.statement)
        {
            s.statement.accept(this);
        }
    }

    override void visit(GotoStatement s)
    {
        buf.writestring("goto ");
        buf.writestring(s.ident.toString());
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(LabelStatement s)
    {
        buf.writestring(s.ident.toString());
        buf.writeByte(':');
        buf.writenl();
        if (s.statement)
            s.statement.accept(this);
    }

    override void visit(AsmStatement s)
    {
        buf.writestring("asm { ");
        Token* t = s.tokens;
        buf.level++;
        while (t)
        {
            buf.writestring(t.toChars());
            if (t.next &&
                t.value != TOK.min      &&
                t.value != TOK.comma    && t.next.value != TOK.comma    &&
                t.value != TOK.leftBracket && t.next.value != TOK.leftBracket &&
                                          t.next.value != TOK.rightBracket &&
                t.value != TOK.leftParenthesis   && t.next.value != TOK.leftParenthesis   &&
                                          t.next.value != TOK.rightParenthesis   &&
                t.value != TOK.dot      && t.next.value != TOK.dot)
            {
                buf.writeByte(' ');
            }
            t = t.next;
        }
        buf.level--;
        buf.writestring("; }");
        buf.writenl();
    }

    override void visit(ImportStatement s)
    {
        foreach (imp; *s.imports)
        {
            imp.dsymbolToBuffer(buf, hgs);
        }
    }

    void visit(Catch c)
    {
        buf.writestring("catch");
        if (c.type)
        {
            buf.writeByte('(');
            typeToBuffer(c.type, c.ident, buf, hgs);
            buf.writeByte(')');
        }
        buf.writenl();
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        if (c.handler)
            c.handler.accept(this);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
    }
}

private void dsymbolToBuffer(Dsymbol s, OutBuffer* buf, HdrGenState* hgs)
{
    scope v = new DsymbolPrettyPrintVisitor(buf, hgs);
    s.accept(v);
}

private extern (C++) final class DsymbolPrettyPrintVisitor : Visitor
{
    alias visit = Visitor.visit;
public:
    OutBuffer* buf;
    HdrGenState* hgs;

    extern (D) this(OutBuffer* buf, HdrGenState* hgs) scope
    {
        this.buf = buf;
        this.hgs = hgs;
    }

    ////////////////////////////////////////////////////////////////////////////

    override void visit(Dsymbol s)
    {
        buf.writestring(s.toChars());
    }

    override void visit(StaticAssert s)
    {
        buf.writestring(s.kind());
        buf.writeByte('(');
        s.exp.expressionToBuffer(buf, hgs);
        if (s.msgs)
        {
            foreach (m; (*s.msgs)[])
            {
                buf.writestring(", ");
                m.expressionToBuffer(buf, hgs);
            }
        }
        buf.writestring(");");
        buf.writenl();
    }

    override void visit(DebugSymbol s)
    {
        buf.writestring("debug = ");
        if (s.ident)
            buf.writestring(s.ident.toString());
        else
            buf.print(s.level);
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(VersionSymbol s)
    {
        buf.writestring("version = ");
        if (s.ident)
            buf.writestring(s.ident.toString());
        else
            buf.print(s.level);
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(EnumMember em)
    {
        if (em.type)
            typeToBuffer(em.type, em.ident, buf, hgs);
        else
            buf.writestring(em.ident.toString());
        if (em.value)
        {
            buf.writestring(" = ");
            em.value.expressionToBuffer(buf, hgs);
        }
    }

    override void visit(Import imp)
    {
        if (hgs.hdrgen && imp.id == Id.object)
            return; // object is imported by default
        if (imp.isstatic)
            buf.writestring("static ");
        buf.writestring("import ");
        if (imp.aliasId)
        {
            buf.printf("%s = ", imp.aliasId.toChars());
        }
        foreach (const pid; imp.packages)
        {
            buf.printf("%s.", pid.toChars());
        }
        buf.writestring(imp.id.toString());
        if (imp.names.length)
        {
            buf.writestring(" : ");
            foreach (const i, const name; imp.names)
            {
                if (i)
                    buf.writestring(", ");
                const _alias = imp.aliases[i];
                if (_alias)
                    buf.printf("%s = %s", _alias.toChars(), name.toChars());
                else
                    buf.writestring(name.toChars());
            }
        }
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(AliasThis d)
    {
        buf.writestring("alias ");
        buf.writestring(d.ident.toString());
        buf.writestring(" this;\n");
    }

    override void visit(AttribDeclaration d)
    {
        bool hasSTC;
        if (auto stcd = d.isStorageClassDeclaration)
        {
            hasSTC = stcToBuffer(buf, stcd.stc);
        }

        if (!d.decl)
        {
            buf.writeByte(';');
            buf.writenl();
            return;
        }
        if (d.decl.length == 0 || (hgs.hdrgen && d.decl.length == 1 && (*d.decl)[0].isUnitTestDeclaration()))
        {
            // hack for bugzilla 8081
            if (hasSTC) buf.writeByte(' ');
            buf.writestring("{}");
        }
        else if (d.decl.length == 1)
        {
            if (hasSTC) buf.writeByte(' ');
            (*d.decl)[0].accept(this);
            return;
        }
        else
        {
            buf.writenl();
            buf.writeByte('{');
            buf.writenl();
            buf.level++;
            foreach (de; *d.decl)
                de.accept(this);
            buf.level--;
            buf.writeByte('}');
        }
        buf.writenl();
    }

    override void visit(StorageClassDeclaration d)
    {
        visit(cast(AttribDeclaration)d);
    }

    override void visit(DeprecatedDeclaration d)
    {
        buf.writestring("deprecated(");
        d.msg.expressionToBuffer(buf, hgs);
        buf.writestring(") ");
        visit(cast(AttribDeclaration)d);
    }

    override void visit(LinkDeclaration d)
    {
        buf.writestring("extern (");
        buf.writestring(linkageToString(d.linkage));
        buf.writestring(") ");
        visit(cast(AttribDeclaration)d);
    }

    override void visit(CPPMangleDeclaration d)
    {
        string s;
        final switch (d.cppmangle)
        {
        case CPPMANGLE.asClass:
            s = "class";
            break;
        case CPPMANGLE.asStruct:
            s = "struct";
            break;
        case CPPMANGLE.def:
            break;
        }
        buf.writestring("extern (C++, ");
        buf.writestring(s);
        buf.writestring(") ");
        visit(cast(AttribDeclaration)d);
    }

    override void visit(VisibilityDeclaration d)
    {
        visibilityToBuffer(buf, d.visibility);
        AttribDeclaration ad = cast(AttribDeclaration)d;
        if (ad.decl.length <= 1)
            buf.writeByte(' ');
        if (ad.decl.length == 1 && (*ad.decl)[0].isVisibilityDeclaration)
            visit(cast(AttribDeclaration)(*ad.decl)[0]);
        else
            visit(cast(AttribDeclaration)d);
    }

    override void visit(AlignDeclaration d)
    {
        if (d.exps)
        {
            foreach (i, exp; (*d.exps)[])
            {
                if (i)
                    buf.writeByte(' ');
                buf.printf("align (%s)", exp.toChars());
            }
            if (d.decl && d.decl.length < 2)
                buf.writeByte(' ');
        }
        else
            buf.writestring("align ");

        visit(d.isAttribDeclaration());
    }

    override void visit(AnonDeclaration d)
    {
        buf.writestring(d.isunion ? "union" : "struct");
        buf.writenl();
        buf.writestring("{");
        buf.writenl();
        buf.level++;
        if (d.decl)
        {
            foreach (de; *d.decl)
                de.accept(this);
        }
        buf.level--;
        buf.writestring("}");
        buf.writenl();
    }

    override void visit(PragmaDeclaration d)
    {
        buf.writestring("pragma (");
        buf.writestring(d.ident.toString());
        if (d.args && d.args.length)
        {
            buf.writestring(", ");
            argsToBuffer(d.args, buf, hgs);
        }

        buf.writeByte(')');

        // https://issues.dlang.org/show_bug.cgi?id=14690
        // Unconditionally perform a full output dump
        // for `pragma(inline)` declarations.
        bool savedFullDump = global.params.dihdr.fullOutput;
        if (d.ident == Id.Pinline)
            global.params.dihdr.fullOutput = true;

        visit(cast(AttribDeclaration)d);
        global.params.dihdr.fullOutput = savedFullDump;
    }

    override void visit(ConditionalDeclaration d)
    {
        d.condition.conditionToBuffer(buf, hgs);
        if (d.decl || d.elsedecl)
        {
            buf.writenl();
            buf.writeByte('{');
            buf.writenl();
            buf.level++;
            if (d.decl)
            {
                foreach (de; *d.decl)
                    de.accept(this);
            }
            buf.level--;
            buf.writeByte('}');
            if (d.elsedecl)
            {
                buf.writenl();
                buf.writestring("else");
                buf.writenl();
                buf.writeByte('{');
                buf.writenl();
                buf.level++;
                foreach (de; *d.elsedecl)
                    de.accept(this);
                buf.level--;
                buf.writeByte('}');
            }
        }
        else
            buf.writeByte(':');
        buf.writenl();
    }

    override void visit(StaticForeachDeclaration s)
    {
        void foreachWithoutBody(ForeachStatement s)
        {
            buf.writestring(Token.toString(s.op));
            buf.writestring(" (");
            foreach (i, p; *s.parameters)
            {
                if (i)
                    buf.writestring(", ");
                if (stcToBuffer(buf, p.storageClass))
                    buf.writeByte(' ');
                if (p.type)
                    typeToBuffer(p.type, p.ident, buf, hgs);
                else
                    buf.writestring(p.ident.toString());
            }
            buf.writestring("; ");
            s.aggr.expressionToBuffer(buf, hgs);
            buf.writeByte(')');
            buf.writenl();
        }

        void foreachRangeWithoutBody(ForeachRangeStatement s)
        {
            /* s.op ( prm ; lwr .. upr )
             */
            buf.writestring(Token.toString(s.op));
            buf.writestring(" (");
            if (s.prm.type)
                typeToBuffer(s.prm.type, s.prm.ident, buf, hgs);
            else
                buf.writestring(s.prm.ident.toString());
            buf.writestring("; ");
            s.lwr.expressionToBuffer(buf, hgs);
            buf.writestring(" .. ");
            s.upr.expressionToBuffer(buf, hgs);
            buf.writeByte(')');
            buf.writenl();
        }

        buf.writestring("static ");
        if (s.sfe.aggrfe)
        {
            foreachWithoutBody(s.sfe.aggrfe);
        }
        else
        {
            assert(s.sfe.rangefe);
            foreachRangeWithoutBody(s.sfe.rangefe);
        }
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        visit(cast(AttribDeclaration)s);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();

    }

    override void visit(CompileDeclaration d)
    {
        buf.writestring("mixin(");
        argsToBuffer(d.exps, buf, hgs, null);
        buf.writestring(");");
        buf.writenl();
    }

    override void visit(UserAttributeDeclaration d)
    {
        buf.writestring("@(");
        argsToBuffer(d.atts, buf, hgs);
        buf.writeByte(')');
        visit(cast(AttribDeclaration)d);
    }

    override void visit(TemplateDeclaration d)
    {
        version (none)
        {
            // Should handle template functions for doc generation
            if (onemember && onemember.isFuncDeclaration())
                buf.writestring("foo ");
        }
        if ((hgs.hdrgen || hgs.fullDump) && visitEponymousMember(d))
            return;
        if (hgs.ddoc)
            buf.writestring(d.kind());
        else
            buf.writestring("template");
        buf.writeByte(' ');
        buf.writestring(d.ident.toString());
        buf.writeByte('(');
        visitTemplateParameters(hgs.ddoc ? d.origParameters : d.parameters);
        buf.writeByte(')');
        visitTemplateConstraint(d.constraint);
        if (hgs.hdrgen || hgs.fullDump)
        {
            hgs.tpltMember++;
            buf.writenl();
            buf.writeByte('{');
            buf.writenl();
            buf.level++;
            foreach (s; *d.members)
                s.accept(this);
            buf.level--;
            buf.writeByte('}');
            buf.writenl();
            hgs.tpltMember--;
        }
    }

    bool visitEponymousMember(TemplateDeclaration d)
    {
        if (!d.members || d.members.length != 1)
            return false;
        Dsymbol onemember = (*d.members)[0];
        if (onemember.ident != d.ident)
            return false;
        if (FuncDeclaration fd = onemember.isFuncDeclaration())
        {
            assert(fd.type);
            if (stcToBuffer(buf, fd.storage_class))
                buf.writeByte(' ');
            functionToBufferFull(cast(TypeFunction)fd.type, buf, d.ident, hgs, d);
            visitTemplateConstraint(d.constraint);
            hgs.tpltMember++;
            bodyToBuffer(fd);
            hgs.tpltMember--;
            return true;
        }
        if (AggregateDeclaration ad = onemember.isAggregateDeclaration())
        {
            buf.writestring(ad.kind());
            buf.writeByte(' ');
            buf.writestring(ad.ident.toString());
            buf.writeByte('(');
            visitTemplateParameters(hgs.ddoc ? d.origParameters : d.parameters);
            buf.writeByte(')');
            visitTemplateConstraint(d.constraint);
            visitBaseClasses(ad.isClassDeclaration());
            hgs.tpltMember++;
            if (ad.members)
            {
                buf.writenl();
                buf.writeByte('{');
                buf.writenl();
                buf.level++;
                foreach (s; *ad.members)
                    s.accept(this);
                buf.level--;
                buf.writeByte('}');
            }
            else
                buf.writeByte(';');
            buf.writenl();
            hgs.tpltMember--;
            return true;
        }
        if (VarDeclaration vd = onemember.isVarDeclaration())
        {
            if (d.constraint)
                return false;
            if (stcToBuffer(buf, vd.storage_class))
                buf.writeByte(' ');
            if (vd.type)
                typeToBuffer(vd.type, vd.ident, buf, hgs);
            else
                buf.writestring(vd.ident.toString());
            buf.writeByte('(');
            visitTemplateParameters(hgs.ddoc ? d.origParameters : d.parameters);
            buf.writeByte(')');
            if (vd._init)
            {
                buf.writestring(" = ");
                ExpInitializer ie = vd._init.isExpInitializer();
                if (ie && (ie.exp.op == EXP.construct || ie.exp.op == EXP.blit))
                    (cast(AssignExp)ie.exp).e2.expressionToBuffer(buf, hgs);
                else
                    vd._init.initializerToBuffer(buf, hgs);
            }
            buf.writeByte(';');
            buf.writenl();
            return true;
        }
        return false;
    }

    void visitTemplateParameters(TemplateParameters* parameters)
    {
        if (!parameters || !parameters.length)
            return;
        foreach (i, p; *parameters)
        {
            if (i)
                buf.writestring(", ");
            p.templateParameterToBuffer(buf, hgs);
        }
    }

    void visitTemplateConstraint(Expression constraint)
    {
        if (!constraint)
            return;
        buf.writestring(" if (");
        constraint.expressionToBuffer(buf, hgs);
        buf.writeByte(')');
    }

    override void visit(TemplateInstance ti)
    {
        buf.writestring(ti.name.toChars());
        tiargsToBuffer(ti, buf, hgs);

        if (hgs.fullDump)
        {
            buf.writenl();
            dumpTemplateInstance(ti, buf, hgs);
        }
    }

    override void visit(TemplateMixin tm)
    {
        buf.writestring("mixin ");
        typeToBuffer(tm.tqual, null, buf, hgs);
        tiargsToBuffer(tm, buf, hgs);
        if (tm.ident && memcmp(tm.ident.toChars(), cast(const(char)*)"__mixin", 7) != 0)
        {
            buf.writeByte(' ');
            buf.writestring(tm.ident.toString());
        }
        buf.writeByte(';');
        buf.writenl();
        if (hgs.fullDump)
            dumpTemplateInstance(tm, buf, hgs);
    }

    override void visit(EnumDeclaration d)
    {
        auto oldInEnumDecl = hgs.inEnumDecl;
        scope(exit) hgs.inEnumDecl = oldInEnumDecl;
        hgs.inEnumDecl = d;
        buf.writestring("enum ");
        if (d.ident)
        {
            buf.writestring(d.ident.toString());
        }
        if (d.memtype)
        {
            buf.writestring(" : ");
            typeToBuffer(d.memtype, null, buf, hgs);
        }
        if (!d.members)
        {
            buf.writeByte(';');
            buf.writenl();
            return;
        }
        buf.writenl();
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        foreach (em; *d.members)
        {
            if (!em)
                continue;
            em.accept(this);
            buf.writeByte(',');
            buf.writenl();
        }
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
    }

    override void visit(Nspace d)
    {
        buf.writestring("extern (C++, ");
        buf.writestring(d.ident.toString());
        buf.writeByte(')');
        buf.writenl();
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        foreach (s; *d.members)
            s.accept(this);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
    }

    override void visit(StructDeclaration d)
    {
        buf.writestring(d.kind());
        buf.writeByte(' ');
        if (!d.isAnonymous())
            buf.writestring(d.toChars());
        if (!d.members)
        {
            buf.writeByte(';');
            buf.writenl();
            return;
        }
        buf.writenl();
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        hgs.insideAggregate++;
        foreach (s; *d.members)
            s.accept(this);
        hgs.insideAggregate--;
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
    }

    override void visit(ClassDeclaration d)
    {
        if (!d.isAnonymous())
        {
            buf.writestring(d.kind());
            buf.writeByte(' ');
            buf.writestring(d.ident.toString());
        }
        visitBaseClasses(d);
        if (d.members)
        {
            buf.writenl();
            buf.writeByte('{');
            buf.writenl();
            buf.level++;
            hgs.insideAggregate++;
            foreach (s; *d.members)
                s.accept(this);
            hgs.insideAggregate--;
            buf.level--;
            buf.writeByte('}');
        }
        else
            buf.writeByte(';');
        buf.writenl();
    }

    void visitBaseClasses(ClassDeclaration d)
    {
        if (!d || !d.baseclasses.length)
            return;
        if (!d.isAnonymous())
            buf.writestring(" : ");
        foreach (i, b; *d.baseclasses)
        {
            if (i)
                buf.writestring(", ");
            typeToBuffer(b.type, null, buf, hgs);
        }
    }

    override void visit(AliasDeclaration d)
    {
        if (d.storage_class & STC.local)
            return;
        buf.writestring("alias ");
        if (d.aliassym)
        {
            buf.writestring(d.ident.toString());
            buf.writestring(" = ");
            if (stcToBuffer(buf, d.storage_class))
                buf.writeByte(' ');
            /*
                https://issues.dlang.org/show_bug.cgi?id=23223
                https://issues.dlang.org/show_bug.cgi?id=23222
                This special case (initially just for modules) avoids some segfaults
                and nicer -vcg-ast output.
            */
            if (d.aliassym.isModule())
            {
                buf.writestring(d.aliassym.ident.toString());
            }
            else
            {
                d.aliassym.accept(this);
            }
        }
        else if (d.type.ty == Tfunction)
        {
            if (stcToBuffer(buf, d.storage_class))
                buf.writeByte(' ');
            typeToBuffer(d.type, d.ident, buf, hgs);
        }
        else if (d.ident)
        {
            hgs.declstring = (d.ident == Id.string || d.ident == Id.wstring || d.ident == Id.dstring);
            buf.writestring(d.ident.toString());
            buf.writestring(" = ");
            if (stcToBuffer(buf, d.storage_class))
                buf.writeByte(' ');
            typeToBuffer(d.type, null, buf, hgs);
            hgs.declstring = false;
        }
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(AliasAssign d)
    {
        buf.writestring(d.ident.toString());
        buf.writestring(" = ");
        if (d.aliassym)
            d.aliassym.accept(this);
        else // d.type
            typeToBuffer(d.type, null, buf, hgs);
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(VarDeclaration d)
    {
        if (d.storage_class & STC.local)
            return;
        visitVarDecl(d, false);
        buf.writeByte(';');
        buf.writenl();
    }

    void visitVarDecl(VarDeclaration v, bool anywritten)
    {
        const bool isextern = hgs.hdrgen &&
            !hgs.insideFuncBody &&
            !hgs.tpltMember &&
            !hgs.insideAggregate &&
            !(v.storage_class & STC.manifest);

        void vinit(VarDeclaration v)
        {
            auto ie = v._init.isExpInitializer();
            if (ie && (ie.exp.op == EXP.construct || ie.exp.op == EXP.blit))
                (cast(AssignExp)ie.exp).e2.expressionToBuffer(buf, hgs);
            else
                v._init.initializerToBuffer(buf, hgs);
        }

        if (anywritten)
        {
            buf.writestring(", ");
            buf.writestring(v.ident.toString());
        }
        else
        {
            const bool useTypeof = isextern && v._init && !v.type;
            auto stc = v.storage_class;
            if (isextern)
                stc |= STC.extern_;
            if (useTypeof)
                stc &= ~STC.auto_;
            if (stcToBuffer(buf, stc))
                buf.writeByte(' ');
            if (v.type)
                typeToBuffer(v.type, v.ident, buf, hgs);
            else if (useTypeof)
            {
                buf.writestring("typeof(");
                vinit(v);
                buf.writestring(") ");
                buf.writestring(v.ident.toString());
            }
            else
                buf.writestring(v.ident.toString());
        }
        if (v._init && !isextern)
        {
            buf.writestring(" = ");
            vinit(v);
        }
    }

    override void visit(FuncDeclaration f)
    {
        //printf("FuncDeclaration::toCBuffer() '%s'\n", f.toChars());
        if (stcToBuffer(buf, f.storage_class))
            buf.writeByte(' ');
        auto tf = cast(TypeFunction)f.type;
        typeToBuffer(tf, f.ident, buf, hgs);

        if (hgs.hdrgen)
        {
            // if the return type is missing (e.g. ref functions or auto)
            // https://issues.dlang.org/show_bug.cgi?id=20090
            // constructors are an exception: they don't have an explicit return
            // type but we still don't output the body.
            if ((!f.isCtorDeclaration() && !tf.next) || f.storage_class & STC.auto_)
            {
                hgs.autoMember++;
                bodyToBuffer(f);
                hgs.autoMember--;
            }
            else if (hgs.tpltMember == 0 && global.params.dihdr.fullOutput == false && !hgs.insideFuncBody)
            {
                if (!f.fbody)
                {
                    // this can happen on interfaces / abstract functions, see `allowsContractWithoutBody`
                    if (f.fensures || f.frequires)
                        buf.writenl();
                    contractsToBuffer(f);
                }
                buf.writeByte(';');
                buf.writenl();
            }
            else
                bodyToBuffer(f);
        }
        else
            bodyToBuffer(f);
    }

    /// Returns: whether `do` is needed to write the function body
    bool contractsToBuffer(FuncDeclaration f)
    {
        bool requireDo = false;
        // in{}
        if (f.frequires)
        {
            foreach (frequire; *f.frequires)
            {
                buf.writestring("in");
                if (auto es = frequire.isExpStatement())
                {
                    assert(es.exp && es.exp.op == EXP.assert_);
                    buf.writestring(" (");
                    (cast(AssertExp)es.exp).e1.expressionToBuffer(buf, hgs);
                    buf.writeByte(')');
                    buf.writenl();
                    requireDo = false;
                }
                else
                {
                    buf.writenl();
                    frequire.statementToBuffer(buf, hgs);
                    requireDo = true;
                }
            }
        }
        // out{}
        if (f.fensures)
        {
            foreach (fensure; *f.fensures)
            {
                buf.writestring("out");
                if (auto es = fensure.ensure.isExpStatement())
                {
                    assert(es.exp && es.exp.op == EXP.assert_);
                    buf.writestring(" (");
                    if (fensure.id)
                    {
                        buf.writestring(fensure.id.toString());
                    }
                    buf.writestring("; ");
                    (cast(AssertExp)es.exp).e1.expressionToBuffer(buf, hgs);
                    buf.writeByte(')');
                    buf.writenl();
                    requireDo = false;
                }
                else
                {
                    if (fensure.id)
                    {
                        buf.writeByte('(');
                        buf.writestring(fensure.id.toString());
                        buf.writeByte(')');
                    }
                    buf.writenl();
                    fensure.ensure.statementToBuffer(buf, hgs);
                    requireDo = true;
                }
            }
        }
        return requireDo;
    }

    void bodyToBuffer(FuncDeclaration f)
    {
        if (!f.fbody || (hgs.hdrgen && global.params.dihdr.fullOutput == false && !hgs.autoMember && !hgs.tpltMember && !hgs.insideFuncBody))
        {
            if (!f.fbody && (f.fensures || f.frequires))
            {
                buf.writenl();
                contractsToBuffer(f);
            }
            buf.writeByte(';');
            buf.writenl();
            return;
        }

        // there is no way to know if a function is nested
        // or not after parsing. We need scope information
        // for that, which is avaible during semantic
        // analysis. To overcome that, a simple mechanism
        // is implemented: everytime we print a function
        // body (templated or not) we increment a counter.
        // We decredement the counter when we stop
        // printing the function body.
        ++hgs.insideFuncBody;
        scope(exit) { --hgs.insideFuncBody; }

        const savetlpt = hgs.tpltMember;
        const saveauto = hgs.autoMember;
        hgs.tpltMember = 0;
        hgs.autoMember = 0;
        buf.writenl();
        bool requireDo = contractsToBuffer(f);

        if (requireDo)
        {
            buf.writestring("do");
            buf.writenl();
        }
        buf.writeByte('{');
        buf.writenl();
        buf.level++;
        f.fbody.statementToBuffer(buf, hgs);
        buf.level--;
        buf.writeByte('}');
        buf.writenl();
        hgs.tpltMember = savetlpt;
        hgs.autoMember = saveauto;
    }

    override void visit(FuncLiteralDeclaration f)
    {
        if (f.type.ty == Terror)
        {
            buf.writestring("__error");
            return;
        }
        if (f.tok != TOK.reserved)
        {
            buf.writestring(f.kind());
            buf.writeByte(' ');
        }
        TypeFunction tf = cast(TypeFunction)f.type;

        if (!f.inferRetType && tf.next)
            typeToBuffer(tf.next, null, buf, hgs);
        parametersToBuffer(tf.parameterList, buf, hgs);

        // https://issues.dlang.org/show_bug.cgi?id=20074
        void printAttribute(string str)
        {
            buf.writeByte(' ');
            buf.writestring(str);
        }
        tf.attributesApply(&printAttribute);


        CompoundStatement cs = f.fbody.isCompoundStatement();
        Statement s1;
        if (f.semanticRun >= PASS.semantic3done && cs)
        {
            s1 = (*cs.statements)[cs.statements.length - 1];
        }
        else
            s1 = !cs ? f.fbody : null;
        ReturnStatement rs = s1 ? s1.endsWithReturnStatement() : null;
        if (rs && rs.exp)
        {
            buf.writestring(" => ");
            rs.exp.expressionToBuffer(buf, hgs);
        }
        else
        {
            hgs.tpltMember++;
            bodyToBuffer(f);
            hgs.tpltMember--;
        }
    }

    override void visit(PostBlitDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class))
            buf.writeByte(' ');
        buf.writestring("this(this)");
        bodyToBuffer(d);
    }

    override void visit(DtorDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class))
            buf.writeByte(' ');
        buf.writestring("~this()");
        bodyToBuffer(d);
    }

    override void visit(StaticCtorDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class & ~STC.static_))
            buf.writeByte(' ');
        if (d.isSharedStaticCtorDeclaration())
            buf.writestring("shared ");
        buf.writestring("static this()");
        if (hgs.hdrgen && !hgs.tpltMember)
        {
            buf.writeByte(';');
            buf.writenl();
        }
        else
            bodyToBuffer(d);
    }

    override void visit(StaticDtorDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class & ~STC.static_))
            buf.writeByte(' ');
        if (d.isSharedStaticDtorDeclaration())
            buf.writestring("shared ");
        buf.writestring("static ~this()");
        if (hgs.hdrgen && !hgs.tpltMember)
        {
            buf.writeByte(';');
            buf.writenl();
        }
        else
            bodyToBuffer(d);
    }

    override void visit(InvariantDeclaration d)
    {
        if (hgs.hdrgen)
            return;
        if (stcToBuffer(buf, d.storage_class))
            buf.writeByte(' ');
        buf.writestring("invariant");
        if(auto es = d.fbody.isExpStatement())
        {
            assert(es.exp && es.exp.op == EXP.assert_);
            buf.writestring(" (");
            (cast(AssertExp)es.exp).e1.expressionToBuffer(buf, hgs);
            buf.writestring(");");
            buf.writenl();
        }
        else
        {
            bodyToBuffer(d);
        }
    }

    override void visit(UnitTestDeclaration d)
    {
        if (hgs.hdrgen)
            return;
        if (stcToBuffer(buf, d.storage_class))
            buf.writeByte(' ');
        buf.writestring("unittest");
        bodyToBuffer(d);
    }

    override void visit(BitFieldDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class))
            buf.writeByte(' ');
        Identifier id = d.isAnonymous() ? null : d.ident;
        typeToBuffer(d.type, id, buf, hgs);
        buf.writestring(" : ");
        d.width.expressionToBuffer(buf, hgs);
        buf.writeByte(';');
        buf.writenl();
    }

    override void visit(NewDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class & ~STC.static_))
            buf.writeByte(' ');
        buf.writestring("new();");
    }

    override void visit(Module m)
    {
        moduleToBuffer2(m, buf, hgs);
    }
}

/*********************************************
 * Print expression to buffer.
 */
private void expressionPrettyPrint(Expression e, OutBuffer* buf, HdrGenState* hgs)
{
    void visit(Expression e)
    {
        buf.writestring(EXPtoString(e.op));
    }

    void visitInteger(IntegerExp e)
    {
        const dinteger_t v = e.toInteger();
        if (e.type)
        {
            Type t = e.type;
        L1:
            switch (t.ty)
            {
            case Tenum:
                {
                    TypeEnum te = cast(TypeEnum)t;
                    auto sym = te.sym;
                    if (sym && sym.members && (!hgs.inEnumDecl || hgs.inEnumDecl != sym))
                    {
                        foreach (em; *sym.members)
                        {
                            if ((cast(EnumMember)em).value.toInteger == v)
                            {
                                buf.printf("%s.%s", sym.toChars(), em.ident.toChars());
                                return ;
                            }
                        }
                    }

                    buf.printf("cast(%s)", te.sym.toChars());
                    t = te.sym.memtype;
                    goto L1;
                }
            case Tchar:
            case Twchar:
            case Tdchar:
                {
                    const o = buf.length;
                    writeSingleCharLiteral(*buf, cast(dchar) v);
                    if (hgs.ddoc)
                        escapeDdocString(buf, o);
                    break;
                }
            case Tint8:
                buf.writestring("cast(byte)");
                goto L2;
            case Tint16:
                buf.writestring("cast(short)");
                goto L2;
            case Tint32:
            L2:
                buf.printf("%d", cast(int)v);
                break;
            case Tuns8:
                buf.writestring("cast(ubyte)");
                goto case Tuns32;
            case Tuns16:
                buf.writestring("cast(ushort)");
                goto case Tuns32;
            case Tuns32:
                buf.printf("%uu", cast(uint)v);
                break;
            case Tint64:
                if (v == long.min)
                {
                    // https://issues.dlang.org/show_bug.cgi?id=23173
                    // This is a special case because - is not part of the
                    // integer literal and 9223372036854775808L overflows a long
                    buf.writestring("cast(long)-9223372036854775808");
                }
                else
                {
                    buf.printf("%lldL", v);
                }
                break;
            case Tuns64:
                buf.printf("%lluLU", v);
                break;
            case Tbool:
                buf.writestring(v ? "true" : "false");
                break;
            case Tpointer:
                buf.writestring("cast(");
                buf.writestring(t.toChars());
                buf.writeByte(')');
                if (target.ptrsize == 8)
                    goto case Tuns64;
                else if (target.ptrsize == 4 ||
                         target.ptrsize == 2)
                    goto case Tuns32;
                else
                    assert(0);

            case Tvoid:
                buf.writestring("cast(void)0");
                break;

            default:
                /* This can happen if errors, such as
                 * the type is painted on like in fromConstInitializer().
                 */
                if (!global.errors)
                {
                    assert(0);
                }
                break;
            }
        }
        else if (v & 0x8000000000000000L)
            buf.printf("0x%llx", v);
        else
            buf.print(v);
    }

    void visitError(ErrorExp e)
    {
        buf.writestring("__error");
    }

    void visitVoidInit(VoidInitExp e)
    {
        buf.writestring("__void");
    }

    void floatToBuffer(Type type, real_t value)
    {
        .floatToBuffer(type, value, buf, hgs.hdrgen);
    }

    void visitReal(RealExp e)
    {
        floatToBuffer(e.type, e.value);
    }

    void visitComplex(ComplexExp e)
    {
        /* Print as:
         *  (re+imi)
         */
        buf.writeByte('(');
        floatToBuffer(e.type, creall(e.value));
        buf.writeByte('+');
        floatToBuffer(e.type, cimagl(e.value));
        buf.writestring("i)");
    }

    void visitIdentifier(IdentifierExp e)
    {
        if (hgs.hdrgen || hgs.ddoc)
            buf.writestring(e.ident.toHChars2());
        else
            buf.writestring(e.ident.toString());
    }

    void visitDsymbol(DsymbolExp e)
    {
        buf.writestring(e.s.toChars());
    }

    void visitThis(ThisExp e)
    {
        buf.writestring("this");
    }

    void visitSuper(SuperExp e)
    {
        buf.writestring("super");
    }

    void visitNull(NullExp e)
    {
        buf.writestring("null");
    }

    void visitString(StringExp e)
    {
        buf.writeByte('"');
        const o = buf.length;
        foreach (i; 0 .. e.len)
        {
            writeCharLiteral(*buf, e.getCodeUnit(i));
        }
        if (hgs.ddoc)
            escapeDdocString(buf, o);
        buf.writeByte('"');
        if (e.postfix)
            buf.writeByte(e.postfix);
    }

    void visitArrayLiteral(ArrayLiteralExp e)
    {
        buf.writeByte('[');
        argsToBuffer(e.elements, buf, hgs, e.basis);
        buf.writeByte(']');
    }

    void visitAssocArrayLiteral(AssocArrayLiteralExp e)
    {
        buf.writeByte('[');
        foreach (i, key; *e.keys)
        {
            if (i)
                buf.writestring(", ");
            expToBuffer(key, PREC.assign, buf, hgs);
            buf.writeByte(':');
            auto value = (*e.values)[i];
            expToBuffer(value, PREC.assign, buf, hgs);
        }
        buf.writeByte(']');
    }

    void visitStructLiteral(StructLiteralExp e)
    {
        buf.writestring(e.sd.toChars());
        buf.writeByte('(');
        // CTFE can generate struct literals that contain an AddrExp pointing
        // to themselves, need to avoid infinite recursion:
        // struct S { this(int){ this.s = &this; } S* s; }
        // const foo = new S(0);
        if (e.stageflags & stageToCBuffer)
            buf.writestring("<recursion>");
        else
        {
            const old = e.stageflags;
            e.stageflags |= stageToCBuffer;
            argsToBuffer(e.elements, buf, hgs);
            e.stageflags = old;
        }
        buf.writeByte(')');
    }

    void visitCompoundLiteral(CompoundLiteralExp e)
    {
        buf.writeByte('(');
        typeToBuffer(e.type, null, buf, hgs);
        buf.writeByte(')');
        e.initializer.initializerToBuffer(buf, hgs);
    }

    void visitType(TypeExp e)
    {
        typeToBuffer(e.type, null, buf, hgs);
    }

    void visitScope(ScopeExp e)
    {
        if (e.sds.isTemplateInstance())
        {
            e.sds.dsymbolToBuffer(buf, hgs);
        }
        else if (hgs !is null && hgs.ddoc)
        {
            // fixes bug 6491
            if (auto m = e.sds.isModule())
                buf.writestring(m.md.toChars());
            else
                buf.writestring(e.sds.toChars());
        }
        else
        {
            buf.writestring(e.sds.kind());
            buf.writeByte(' ');
            buf.writestring(e.sds.toChars());
        }
    }

    void visitTemplate(TemplateExp e)
    {
        buf.writestring(e.td.toChars());
    }

    void visitNew(NewExp e)
    {
        if (e.thisexp)
        {
            expToBuffer(e.thisexp, PREC.primary, buf, hgs);
            buf.writeByte('.');
        }
        buf.writestring("new ");
        typeToBuffer(e.newtype, null, buf, hgs);
        if (e.arguments && e.arguments.length)
        {
            buf.writeByte('(');
            argsToBuffer(e.arguments, buf, hgs, null, e.names);
            buf.writeByte(')');
        }
    }

    void visitNewAnonClass(NewAnonClassExp e)
    {
        if (e.thisexp)
        {
            expToBuffer(e.thisexp, PREC.primary, buf, hgs);
            buf.writeByte('.');
        }
        buf.writestring("new");
        buf.writestring(" class ");
        if (e.arguments && e.arguments.length)
        {
            buf.writeByte('(');
            argsToBuffer(e.arguments, buf, hgs);
            buf.writeByte(')');
        }
        if (e.cd)
            e.cd.dsymbolToBuffer(buf, hgs);
    }

    void visitSymOff(SymOffExp e)
    {
        if (e.offset)
            buf.printf("(& %s%+lld)", e.var.toChars(), e.offset);
        else if (e.var.isTypeInfoDeclaration())
            buf.writestring(e.var.toChars());
        else
            buf.printf("& %s", e.var.toChars());
    }

    void visitVar(VarExp e)
    {
        buf.writestring(e.var.toChars());
    }

    void visitOver(OverExp e)
    {
        buf.writestring(e.vars.ident.toString());
    }

    void visitTuple(TupleExp e)
    {
        if (e.e0)
        {
            buf.writeByte('(');
            e.e0.expressionPrettyPrint(buf, hgs);
            buf.writestring(", tuple(");
            argsToBuffer(e.exps, buf, hgs);
            buf.writestring("))");
        }
        else
        {
            buf.writestring("tuple(");
            argsToBuffer(e.exps, buf, hgs);
            buf.writeByte(')');
        }
    }

    void visitFunc(FuncExp e)
    {
        e.fd.dsymbolToBuffer(buf, hgs);
        //buf.writestring(e.fd.toChars());
    }

    void visitDeclaration(DeclarationExp e)
    {
        /* Normal dmd execution won't reach here - regular variable declarations
         * are handled in visit(ExpStatement), so here would be used only when
         * we'll directly call Expression.toChars() for debugging.
         */
        if (e.declaration)
        {
            if (auto var = e.declaration.isVarDeclaration())
            {
            // For debugging use:
            // - Avoid printing newline.
            // - Intentionally use the format (Type var;)
            //   which isn't correct as regular D code.
                buf.writeByte('(');

                scope v = new DsymbolPrettyPrintVisitor(buf, hgs);
                v.visitVarDecl(var, false);

                buf.writeByte(';');
                buf.writeByte(')');
            }
            else e.declaration.dsymbolToBuffer(buf, hgs);
        }
    }

    void visitTypeid(TypeidExp e)
    {
        buf.writestring("typeid(");
        objectToBuffer(e.obj, buf, hgs);
        buf.writeByte(')');
    }

    void visitTraits(TraitsExp e)
    {
        buf.writestring("__traits(");
        if (e.ident)
            buf.writestring(e.ident.toString());
        if (e.args)
        {
            foreach (arg; *e.args)
            {
                buf.writestring(", ");
                objectToBuffer(arg, buf, hgs);
            }
        }
        buf.writeByte(')');
    }

    void visitHalt(HaltExp e)
    {
        buf.writestring("halt");
    }

    void visitIs(IsExp e)
    {
        buf.writestring("is(");
        typeToBuffer(e.targ, e.id, buf, hgs);
        if (e.tok2 != TOK.reserved)
        {
            buf.printf(" %s %s", Token.toChars(e.tok), Token.toChars(e.tok2));
        }
        else if (e.tspec)
        {
            if (e.tok == TOK.colon)
                buf.writestring(" : ");
            else
                buf.writestring(" == ");
            typeToBuffer(e.tspec, null, buf, hgs);
        }
        if (e.parameters && e.parameters.length)
        {
            buf.writestring(", ");
            scope v = new DsymbolPrettyPrintVisitor(buf, hgs);
            v.visitTemplateParameters(e.parameters);
        }
        buf.writeByte(')');
    }

    void visitUna(UnaExp e)
    {
        buf.writestring(EXPtoString(e.op));
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitBin(BinExp e)
    {
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
        buf.writeByte(' ');
        buf.writestring(EXPtoString(e.op));
        buf.writeByte(' ');
        expToBuffer(e.e2, cast(PREC)(precedence[e.op] + 1), buf, hgs);
    }

    void visitComma(CommaExp e)
    {
        // CommaExp is generated by the compiler so it shouldn't
        // appear in error messages or header files.
        // For now, this treats the case where the compiler
        // generates CommaExp for temporaries by calling
        // the `sideeffect.copyToTemp` function.
        auto ve = e.e2.isVarExp();

        // not a CommaExp introduced for temporaries, go on
        // the old path
        if (!ve || !(ve.var.storage_class & STC.temp))
        {
            visitBin(cast(BinExp)e);
            return;
        }

        // CommaExp that contain temporaries inserted via
        // `copyToTemp` are usually of the form
        // ((T __temp = exp), __tmp).
        // Asserts are here to easily spot
        // missing cases where CommaExp
        // are used for other constructs
        auto vd = ve.var.isVarDeclaration();
        assert(vd && vd._init);

        if (auto ei = vd._init.isExpInitializer())
        {
            Expression commaExtract;
            auto exp = ei.exp;
            if (auto ce = exp.isConstructExp())
                commaExtract = ce.e2;
            else if (auto se = exp.isStructLiteralExp())
                commaExtract = se;

            if (commaExtract)
            {
                expToBuffer(commaExtract, precedence[exp.op], buf, hgs);
                return;
            }
        }

        // not one of the known cases, go on the old path
        visitBin(cast(BinExp)e);
        return;
    }

    void visitMixin(MixinExp e)
    {
        buf.writestring("mixin(");
        argsToBuffer(e.exps, buf, hgs, null);
        buf.writeByte(')');
    }

    void visitImport(ImportExp e)
    {
        buf.writestring("import(");
        expToBuffer(e.e1, PREC.assign, buf, hgs);
        buf.writeByte(')');
    }

    void visitAssert(AssertExp e)
    {
        buf.writestring("assert(");
        expToBuffer(e.e1, PREC.assign, buf, hgs);
        if (e.msg)
        {
            buf.writestring(", ");
            expToBuffer(e.msg, PREC.assign, buf, hgs);
        }
        buf.writeByte(')');
    }

    void visitThrow(ThrowExp e)
    {
        buf.writestring("throw ");
        expToBuffer(e.e1, PREC.unary, buf, hgs);
    }

    void visitDotId(DotIdExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        if (e.arrow)
            buf.writestring("->");
        else
            buf.writeByte('.');
        buf.writestring(e.ident.toString());
    }

    void visitDotTemplate(DotTemplateExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writeByte('.');
        buf.writestring(e.td.toChars());
    }

    void visitDotVar(DotVarExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writeByte('.');
        buf.writestring(e.var.toChars());
    }

    void visitDotTemplateInstance(DotTemplateInstanceExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writeByte('.');
        e.ti.dsymbolToBuffer(buf, hgs);
    }

    void visitDelegate(DelegateExp e)
    {
        buf.writeByte('&');
        if (!e.func.isNested() || e.func.needThis())
        {
            expToBuffer(e.e1, PREC.primary, buf, hgs);
            buf.writeByte('.');
        }
        buf.writestring(e.func.toChars());
    }

    void visitDotType(DotTypeExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writeByte('.');
        buf.writestring(e.sym.toChars());
    }

    void visitCall(CallExp e)
    {
        if (e.e1.op == EXP.type)
        {
            /* Avoid parens around type to prevent forbidden cast syntax:
             *   (sometype)(arg1)
             * This is ok since types in constructor calls
             * can never depend on parens anyway
             */
            e.e1.expressionPrettyPrint(buf, hgs);
        }
        else
            expToBuffer(e.e1, precedence[e.op], buf, hgs);
        buf.writeByte('(');
        argsToBuffer(e.arguments, buf, hgs, null, e.names);
        buf.writeByte(')');
    }

    void visitPtr(PtrExp e)
    {
        buf.writeByte('*');
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitDelete(DeleteExp e)
    {
        buf.writestring("delete ");
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitCast(CastExp e)
    {
        buf.writestring("cast(");
        if (e.to)
            typeToBuffer(e.to, null, buf, hgs);
        else
        {
            MODtoBuffer(buf, e.mod);
        }
        buf.writeByte(')');
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitVector(VectorExp e)
    {
        buf.writestring("cast(");
        typeToBuffer(e.to, null, buf, hgs);
        buf.writeByte(')');
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitVectorArray(VectorArrayExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writestring(".array");
    }

    void visitSlice(SliceExp e)
    {
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
        buf.writeByte('[');
        if (e.upr || e.lwr)
        {
            if (e.lwr)
                sizeToBuffer(e.lwr, buf, hgs);
            else
                buf.writeByte('0');
            buf.writestring("..");
            if (e.upr)
                sizeToBuffer(e.upr, buf, hgs);
            else
                buf.writeByte('$');
        }
        buf.writeByte(']');
    }

    void visitArrayLength(ArrayLengthExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writestring(".length");
    }

    void visitInterval(IntervalExp e)
    {
        expToBuffer(e.lwr, PREC.assign, buf, hgs);
        buf.writestring("..");
        expToBuffer(e.upr, PREC.assign, buf, hgs);
    }

    void visitDelegatePtr(DelegatePtrExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writestring(".ptr");
    }

    void visitDelegateFuncptr(DelegateFuncptrExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writestring(".funcptr");
    }

    void visitArray(ArrayExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writeByte('[');
        argsToBuffer(e.arguments, buf, hgs);
        buf.writeByte(']');
    }

    void visitDot(DotExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writeByte('.');
        expToBuffer(e.e2, PREC.primary, buf, hgs);
    }

    void visitIndex(IndexExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writeByte('[');
        sizeToBuffer(e.e2, buf, hgs);
        buf.writeByte(']');
    }

    void visitPost(PostExp e)
    {
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
        buf.writestring(EXPtoString(e.op));
    }

    void visitPre(PreExp e)
    {
        buf.writestring(EXPtoString(e.op));
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitRemove(RemoveExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.writestring(".remove(");
        expToBuffer(e.e2, PREC.assign, buf, hgs);
        buf.writeByte(')');
    }

    void visitCond(CondExp e)
    {
        expToBuffer(e.econd, PREC.oror, buf, hgs);
        buf.writestring(" ? ");
        expToBuffer(e.e1, PREC.expr, buf, hgs);
        buf.writestring(" : ");
        expToBuffer(e.e2, PREC.cond, buf, hgs);
    }

    void visitDefaultInit(DefaultInitExp e)
    {
        buf.writestring(EXPtoString(e.op));
    }

    void visitClassReference(ClassReferenceExp e)
    {
        buf.writestring(e.value.toChars());
    }

    switch (e.op)
    {
        default:
            if (auto be = e.isBinExp())
                return visitBin(be);
            else if (auto ue = e.isUnaExp())
                return visitUna(ue);
            else if (auto de = e.isDefaultInitExp())
                return visitDefaultInit(e.isDefaultInitExp());
            return visit(e);

        case EXP.int64:         return visitInteger(e.isIntegerExp());
        case EXP.error:         return visitError(e.isErrorExp());
        case EXP.void_:         return visitVoidInit(e.isVoidInitExp());
        case EXP.float64:       return visitReal(e.isRealExp());
        case EXP.complex80:     return visitComplex(e.isComplexExp());
        case EXP.identifier:    return visitIdentifier(e.isIdentifierExp());
        case EXP.dSymbol:       return visitDsymbol(e.isDsymbolExp());
        case EXP.this_:         return visitThis(e.isThisExp());
        case EXP.super_:        return visitSuper(e.isSuperExp());
        case EXP.null_:         return visitNull(e.isNullExp());
        case EXP.string_:       return visitString(e.isStringExp());
        case EXP.arrayLiteral:  return visitArrayLiteral(e.isArrayLiteralExp());
        case EXP.assocArrayLiteral:     return visitAssocArrayLiteral(e.isAssocArrayLiteralExp());
        case EXP.structLiteral: return visitStructLiteral(e.isStructLiteralExp());
        case EXP.compoundLiteral:       return visitCompoundLiteral(e.isCompoundLiteralExp());
        case EXP.type:          return visitType(e.isTypeExp());
        case EXP.scope_:        return visitScope(e.isScopeExp());
        case EXP.template_:     return visitTemplate(e.isTemplateExp());
        case EXP.new_:          return visitNew(e.isNewExp());
        case EXP.newAnonymousClass:     return visitNewAnonClass(e.isNewAnonClassExp());
        case EXP.symbolOffset:  return visitSymOff(e.isSymOffExp());
        case EXP.variable:      return visitVar(e.isVarExp());
        case EXP.overloadSet:   return visitOver(e.isOverExp());
        case EXP.tuple:         return visitTuple(e.isTupleExp());
        case EXP.function_:     return visitFunc(e.isFuncExp());
        case EXP.declaration:   return visitDeclaration(e.isDeclarationExp());
        case EXP.typeid_:       return visitTypeid(e.isTypeidExp());
        case EXP.traits:        return visitTraits(e.isTraitsExp());
        case EXP.halt:          return visitHalt(e.isHaltExp());
        case EXP.is_:           return visitIs(e.isExp());
        case EXP.comma:         return visitComma(e.isCommaExp());
        case EXP.mixin_:        return visitMixin(e.isMixinExp());
        case EXP.import_:       return visitImport(e.isImportExp());
        case EXP.assert_:       return visitAssert(e.isAssertExp());
        case EXP.throw_:        return visitThrow(e.isThrowExp());
        case EXP.dotIdentifier: return visitDotId(e.isDotIdExp());
        case EXP.dotTemplateDeclaration:        return visitDotTemplate(e.isDotTemplateExp());
        case EXP.dotVariable:   return visitDotVar(e.isDotVarExp());
        case EXP.dotTemplateInstance:   return visitDotTemplateInstance(e.isDotTemplateInstanceExp());
        case EXP.delegate_:     return visitDelegate(e.isDelegateExp());
        case EXP.dotType:       return visitDotType(e.isDotTypeExp());
        case EXP.call:          return visitCall(e.isCallExp());
        case EXP.star:          return visitPtr(e.isPtrExp());
        case EXP.delete_:       return visitDelete(e.isDeleteExp());
        case EXP.cast_:         return visitCast(e.isCastExp());
        case EXP.vector:        return visitVector(e.isVectorExp());
        case EXP.vectorArray:   return visitVectorArray(e.isVectorArrayExp());
        case EXP.slice:         return visitSlice(e.isSliceExp());
        case EXP.arrayLength:   return visitArrayLength(e.isArrayLengthExp());
        case EXP.interval:      return visitInterval(e.isIntervalExp());
        case EXP.delegatePointer:       return visitDelegatePtr(e.isDelegatePtrExp());
        case EXP.delegateFunctionPointer:       return visitDelegateFuncptr(e.isDelegateFuncptrExp());
        case EXP.array:         return visitArray(e.isArrayExp());
        case EXP.dot:           return visitDot(e.isDotExp());
        case EXP.index:         return visitIndex(e.isIndexExp());
        case EXP.minusMinus:
        case EXP.plusPlus:      return visitPost(e.isPostExp());
        case EXP.preMinusMinus:
        case EXP.prePlusPlus:   return visitPre(e.isPreExp());
        case EXP.remove:        return visitRemove(e.isRemoveExp());
        case EXP.question:      return visitCond(e.isCondExp());
        case EXP.classReference:        return visitClassReference(e.isClassReferenceExp());
    }
}

/**
 * Formats `value` as a literal of type `type` into `buf`.
 *
 * Params:
 *   type     = literal type (e.g. Tfloat)
 *   value    = value to print
 *   buf      = target buffer
 *   allowHex = whether hex floating point literals may be used
 *              for greater accuracy
 */
void floatToBuffer(Type type, const real_t value, OutBuffer* buf, const bool allowHex)
{
    /** sizeof(value)*3 is because each byte of mantissa is max
        of 256 (3 characters). The string will be "-M.MMMMe-4932".
        (ie, 8 chars more than mantissa). Plus one for trailing \0.
        Plus one for rounding. */
    const(size_t) BUFFER_LEN = value.sizeof * 3 + 8 + 1 + 1;
    char[BUFFER_LEN] buffer = void;
    CTFloat.sprint(buffer.ptr, BUFFER_LEN, 'g', value);
    assert(strlen(buffer.ptr) < BUFFER_LEN);
    if (allowHex)
    {
        bool isOutOfRange;
        real_t r = CTFloat.parse(buffer.ptr, isOutOfRange);
        //assert(!isOutOfRange); // test/compilable/test22725.c asserts here
        if (r != value) // if exact duplication
            CTFloat.sprint(buffer.ptr, BUFFER_LEN, 'a', value);
    }
    buf.writestring(buffer.ptr);
    if (buffer.ptr[strlen(buffer.ptr) - 1] == '.')
        buf.remove(buf.length() - 1, 1);

    if (type)
    {
        Type t = type.toBasetype();
        switch (t.ty)
        {
        case Tfloat32:
        case Timaginary32:
        case Tcomplex32:
            buf.writeByte('F');
            break;
        case Tfloat80:
        case Timaginary80:
        case Tcomplex80:
            buf.writeByte('L');
            break;
        default:
            break;
        }
        if (t.isimaginary())
            buf.writeByte('i');
    }
}

private void templateParameterToBuffer(TemplateParameter tp, OutBuffer* buf, HdrGenState* hgs)
{
    scope v = new TemplateParameterPrettyPrintVisitor(buf, hgs);
    tp.accept(v);
}

private extern (C++) final class TemplateParameterPrettyPrintVisitor : Visitor
{
    alias visit = Visitor.visit;
public:
    OutBuffer* buf;
    HdrGenState* hgs;

    extern (D) this(OutBuffer* buf, HdrGenState* hgs) scope
    {
        this.buf = buf;
        this.hgs = hgs;
    }

    override void visit(TemplateTypeParameter tp)
    {
        buf.writestring(tp.ident.toString());
        if (tp.specType)
        {
            buf.writestring(" : ");
            typeToBuffer(tp.specType, null, buf, hgs);
        }
        if (tp.defaultType)
        {
            buf.writestring(" = ");
            typeToBuffer(tp.defaultType, null, buf, hgs);
        }
    }

    override void visit(TemplateThisParameter tp)
    {
        buf.writestring("this ");
        visit(cast(TemplateTypeParameter)tp);
    }

    override void visit(TemplateAliasParameter tp)
    {
        buf.writestring("alias ");
        if (tp.specType)
            typeToBuffer(tp.specType, tp.ident, buf, hgs);
        else
            buf.writestring(tp.ident.toString());
        if (tp.specAlias)
        {
            buf.writestring(" : ");
            objectToBuffer(tp.specAlias, buf, hgs);
        }
        if (tp.defaultAlias)
        {
            buf.writestring(" = ");
            objectToBuffer(tp.defaultAlias, buf, hgs);
        }
    }

    override void visit(TemplateValueParameter tp)
    {
        typeToBuffer(tp.valType, tp.ident, buf, hgs);
        if (tp.specValue)
        {
            buf.writestring(" : ");
            tp.specValue.expressionToBuffer(buf, hgs);
        }
        if (tp.defaultValue)
        {
            buf.writestring(" = ");
            tp.defaultValue.expressionToBuffer(buf, hgs);
        }
    }

    override void visit(TemplateTupleParameter tp)
    {
        buf.writestring(tp.ident.toString());
        buf.writestring("...");
    }
}

private void conditionToBuffer(Condition c, OutBuffer* buf, HdrGenState* hgs)
{
    scope v = new ConditionPrettyPrintVisitor(buf, hgs);
    c.accept(v);
}

private extern (C++) final class ConditionPrettyPrintVisitor : Visitor
{
    alias visit = Visitor.visit;
public:
    OutBuffer* buf;
    HdrGenState* hgs;

    extern (D) this(OutBuffer* buf, HdrGenState* hgs) scope
    {
        this.buf = buf;
        this.hgs = hgs;
    }

    override void visit(DebugCondition c)
    {
        buf.writestring("debug (");
        if (c.ident)
            buf.writestring(c.ident.toString());
        else
            buf.print(c.level);
        buf.writeByte(')');
    }

    override void visit(VersionCondition c)
    {
        buf.writestring("version (");
        if (c.ident)
            buf.writestring(c.ident.toString());
        else
            buf.print(c.level);
        buf.writeByte(')');
    }

    override void visit(StaticIfCondition c)
    {
        buf.writestring("static if (");
        c.exp.expressionToBuffer(buf, hgs);
        buf.writeByte(')');
    }
}

void toCBuffer(const Statement s, OutBuffer* buf, HdrGenState* hgs)
{
    scope v = new StatementPrettyPrintVisitor(buf, hgs);
    (cast() s).accept(v);
}

void toCBuffer(const Type t, OutBuffer* buf, const Identifier ident, HdrGenState* hgs)
{
    typeToBuffer(cast() t, ident, buf, hgs);
}

void toCBuffer(Dsymbol s, OutBuffer* buf, HdrGenState* hgs)
{
    scope v = new DsymbolPrettyPrintVisitor(buf, hgs);
    s.accept(v);
}

// used from TemplateInstance::toChars() and TemplateMixin::toChars()
void toCBufferInstance(const TemplateInstance ti, OutBuffer* buf, bool qualifyTypes = false)
{
    HdrGenState hgs;
    hgs.fullQual = qualifyTypes;
    scope v = new DsymbolPrettyPrintVisitor(buf, &hgs);
    v.visit(cast() ti);
}

void toCBuffer(const Initializer iz, OutBuffer* buf, HdrGenState* hgs)
{
    initializerToBuffer(cast() iz, buf, hgs);
}

bool stcToBuffer(OutBuffer* buf, StorageClass stc)
{
    //printf("stc: %llx\n", stc);
    bool result = false;

    if (stc & STC.scopeinferred)
    {
        //buf.writestring("scope-inferred ");
        stc &= ~(STC.scope_ | STC.scopeinferred);
    }
    if (stc & STC.returninferred)
    {
        //buf.writestring((stc & STC.returnScope) ? "return-scope-inferred " : "return-ref-inferred ");
        stc &= ~(STC.return_ | STC.returninferred);
    }

    /* Put scope ref return into a standard order
     */
    string rrs;
    const isout = (stc & STC.out_) != 0;
    //printf("bsr = %d %llx\n", buildScopeRef(stc), stc);
    final switch (buildScopeRef(stc))
    {
        case ScopeRef.None:
        case ScopeRef.Scope:
        case ScopeRef.Ref:
        case ScopeRef.Return:
            break;

        case ScopeRef.ReturnScope:      rrs = "return scope"; goto L1;
        case ScopeRef.ReturnRef:        rrs = isout ? "return out"       : "return ref";       goto L1;
        case ScopeRef.RefScope:         rrs = isout ? "out scope"        : "ref scope";        goto L1;
        case ScopeRef.ReturnRef_Scope:  rrs = isout ? "return out scope" : "return ref scope"; goto L1;
        case ScopeRef.Ref_ReturnScope:  rrs = isout ? "out return scope" : "ref return scope"; goto L1;
        L1:
            buf.writestring(rrs);
            result = true;
            stc &= ~(STC.out_ | STC.scope_ | STC.ref_ | STC.return_);
            break;
    }

    while (stc)
    {
        const s = stcToString(stc);
        if (!s.length)
            break;
        if (result)
            buf.writeByte(' ');
        result = true;
        buf.writestring(s);
    }

    return result;
}

/*************************************************
 * Pick off one of the storage classes from stc,
 * and return a string representation of it.
 * stc is reduced by the one picked.
 */
string stcToString(ref StorageClass stc)
{
    static struct SCstring
    {
        StorageClass stc;
        string id;
    }

    // Note: The identifier needs to be `\0` terminated
    // as some code assumes it (e.g. when printing error messages)
    static immutable SCstring[] table =
    [
        SCstring(STC.auto_, Token.toString(TOK.auto_)),
        SCstring(STC.scope_, Token.toString(TOK.scope_)),
        SCstring(STC.static_, Token.toString(TOK.static_)),
        SCstring(STC.extern_, Token.toString(TOK.extern_)),
        SCstring(STC.const_, Token.toString(TOK.const_)),
        SCstring(STC.final_, Token.toString(TOK.final_)),
        SCstring(STC.abstract_, Token.toString(TOK.abstract_)),
        SCstring(STC.synchronized_, Token.toString(TOK.synchronized_)),
        SCstring(STC.deprecated_, Token.toString(TOK.deprecated_)),
        SCstring(STC.override_, Token.toString(TOK.override_)),
        SCstring(STC.lazy_, Token.toString(TOK.lazy_)),
        SCstring(STC.alias_, Token.toString(TOK.alias_)),
        SCstring(STC.out_, Token.toString(TOK.out_)),
        SCstring(STC.in_, Token.toString(TOK.in_)),
        SCstring(STC.manifest, Token.toString(TOK.enum_)),
        SCstring(STC.immutable_, Token.toString(TOK.immutable_)),
        SCstring(STC.shared_, Token.toString(TOK.shared_)),
        SCstring(STC.nothrow_, Token.toString(TOK.nothrow_)),
        SCstring(STC.wild, Token.toString(TOK.inout_)),
        SCstring(STC.pure_, Token.toString(TOK.pure_)),
        SCstring(STC.ref_, Token.toString(TOK.ref_)),
        SCstring(STC.return_, Token.toString(TOK.return_)),
        SCstring(STC.gshared, Token.toString(TOK.gshared)),
        SCstring(STC.nogc, "@nogc"),
        SCstring(STC.live, "@live"),
        SCstring(STC.property, "@property"),
        SCstring(STC.safe, "@safe"),
        SCstring(STC.trusted, "@trusted"),
        SCstring(STC.system, "@system"),
        SCstring(STC.disable, "@disable"),
        SCstring(STC.future, "@__future"),
        SCstring(STC.local, "__local"),
    ];
    foreach (ref entry; table)
    {
        const StorageClass tbl = entry.stc;
        assert(tbl & STC.visibleStorageClasses);
        if (stc & tbl)
        {
            stc &= ~tbl;
            return entry.id;
        }
    }
    //printf("stc = %llx\n", stc);
    return null;
}

private void linkageToBuffer(OutBuffer* buf, LINK linkage)
{
    const s = linkageToString(linkage);
    if (s.length)
    {
        buf.writestring("extern (");
        buf.writestring(s);
        buf.writeByte(')');
    }
}

const(char)* linkageToChars(LINK linkage)
{
    /// Works because we return a literal
    return linkageToString(linkage).ptr;
}

string linkageToString(LINK linkage) pure nothrow
{
    final switch (linkage)
    {
    case LINK.default_:
        return null;
    case LINK.d:
        return "D";
    case LINK.c:
        return "C";
    case LINK.cpp:
        return "C++";
    case LINK.windows:
        return "Windows";
    case LINK.objc:
        return "Objective-C";
    case LINK.system:
        return "System";
    }
}

void visibilityToBuffer(OutBuffer* buf, Visibility vis)
{
    buf.writestring(visibilityToString(vis.kind));
    if (vis.kind == Visibility.Kind.package_ && vis.pkg)
    {
        buf.writeByte('(');
        buf.writestring(vis.pkg.toPrettyChars(true));
        buf.writeByte(')');
    }
}

/**
 * Returns:
 *   a human readable representation of `kind`
 */
const(char)* visibilityToChars(Visibility.Kind kind)
{
    // Null terminated because we return a literal
    return visibilityToString(kind).ptr;
}

/// Ditto
extern (D) string visibilityToString(Visibility.Kind kind) nothrow pure
{
    final switch (kind)
    {
    case Visibility.Kind.undefined:
        return null;
    case Visibility.Kind.none:
        return "none";
    case Visibility.Kind.private_:
        return "private";
    case Visibility.Kind.package_:
        return "package";
    case Visibility.Kind.protected_:
        return "protected";
    case Visibility.Kind.public_:
        return "public";
    case Visibility.Kind.export_:
        return "export";
    }
}

// Print the full function signature with correct ident, attributes and template args
void functionToBufferFull(TypeFunction tf, OutBuffer* buf, const Identifier ident, HdrGenState* hgs, TemplateDeclaration td)
{
    //printf("TypeFunction::toCBuffer() this = %p\n", this);
    visitFuncIdentWithPrefix(tf, ident, td, buf, hgs);
}

// ident is inserted before the argument list and will be "function" or "delegate" for a type
void functionToBufferWithIdent(TypeFunction tf, OutBuffer* buf, const(char)* ident, bool isStatic)
{
    HdrGenState hgs;
    visitFuncIdentWithPostfix(tf, ident.toDString(), buf, &hgs, isStatic);
}

void toCBuffer(const Expression e, OutBuffer* buf, HdrGenState* hgs)
{
    expressionPrettyPrint(cast()e, buf, hgs);
}

/**************************************************
 * Write out argument types to buf.
 */
void argExpTypesToCBuffer(OutBuffer* buf, Expressions* arguments)
{
    if (!arguments || !arguments.length)
        return;
    HdrGenState hgs;
    foreach (i, arg; *arguments)
    {
        if (i)
            buf.writestring(", ");
        typeToBuffer(arg.type, null, buf, &hgs);
    }
}

void toCBuffer(const TemplateParameter tp, OutBuffer* buf, HdrGenState* hgs)
{
    scope v = new TemplateParameterPrettyPrintVisitor(buf, hgs);
    (cast() tp).accept(v);
}

void arrayObjectsToBuffer(OutBuffer* buf, Objects* objects)
{
    if (!objects || !objects.length)
        return;
    HdrGenState hgs;
    foreach (i, o; *objects)
    {
        if (i)
            buf.writestring(", ");
        objectToBuffer(o, buf, &hgs);
    }
}

/*************************************************************
 * Pretty print function parameters.
 * Params:
 *  pl = parameter list to print
 * Returns: Null-terminated string representing parameters.
 */
extern (C++) const(char)* parametersTypeToChars(ParameterList pl)
{
    OutBuffer buf;
    HdrGenState hgs;
    parametersToBuffer(pl, &buf, &hgs);
    return buf.extractChars();
}

/*************************************************************
 * Pretty print function parameter.
 * Params:
 *  parameter = parameter to print.
 *  tf = TypeFunction which holds parameter.
 *  fullQual = whether to fully qualify types.
 * Returns: Null-terminated string representing parameters.
 */
const(char)* parameterToChars(Parameter parameter, TypeFunction tf, bool fullQual)
{
    OutBuffer buf;
    HdrGenState hgs;
    hgs.fullQual = fullQual;

    parameterToBuffer(parameter, &buf, &hgs);

    if (tf.parameterList.varargs == VarArg.typesafe && parameter == tf.parameterList[tf.parameterList.parameters.length - 1])
    {
        buf.writestring("...");
    }
    return buf.extractChars();
}


/*************************************************
 * Write ParameterList to buffer.
 * Params:
 *      pl = parameter list to serialize
 *      buf = buffer to write it to
 *      hgs = context
 */

private void parametersToBuffer(ParameterList pl, OutBuffer* buf, HdrGenState* hgs)
{
    buf.writeByte('(');
    foreach (i; 0 .. pl.length)
    {
        if (i)
            buf.writestring(", ");
        pl[i].parameterToBuffer(buf, hgs);
    }
    final switch (pl.varargs)
    {
        case VarArg.none:
            break;

        case VarArg.variadic:
            if (pl.length)
                buf.writestring(", ");

            if (stcToBuffer(buf, pl.stc))
                buf.writeByte(' ');
            goto case VarArg.typesafe;

        case VarArg.typesafe:
            buf.writestring("...");
            break;
    }
    buf.writeByte(')');
}


/***********************************************************
 * Write parameter `p` to buffer `buf`.
 * Params:
 *      p = parameter to serialize
 *      buf = buffer to write it to
 *      hgs = context
 */
private void parameterToBuffer(Parameter p, OutBuffer* buf, HdrGenState* hgs)
{
    if (p.userAttribDecl)
    {
        buf.writeByte('@');

        bool isAnonymous = p.userAttribDecl.atts.length > 0 && !(*p.userAttribDecl.atts)[0].isCallExp();
        if (isAnonymous)
            buf.writeByte('(');

        argsToBuffer(p.userAttribDecl.atts, buf, hgs);

        if (isAnonymous)
            buf.writeByte(')');
        buf.writeByte(' ');
    }
    if (p.storageClass & STC.auto_)
        buf.writestring("auto ");

    StorageClass stc = p.storageClass;
    if (p.storageClass & STC.in_)
    {
        buf.writestring("in ");
        if (global.params.previewIn && p.storageClass & STC.ref_)
            stc &= ~STC.ref_;
    }
    else if (p.storageClass & STC.lazy_)
        buf.writestring("lazy ");
    else if (p.storageClass & STC.alias_)
        buf.writestring("alias ");

    if (p.type && p.type.mod & MODFlags.shared_)
        stc &= ~STC.shared_;

    if (stcToBuffer(buf, stc & (STC.const_ | STC.immutable_ | STC.wild | STC.shared_ |
        STC.return_ | STC.returninferred | STC.scope_ | STC.scopeinferred | STC.out_ | STC.ref_ | STC.returnScope)))
        buf.writeByte(' ');

    if (p.storageClass & STC.alias_)
    {
        if (p.ident)
            buf.writestring(p.ident.toString());
    }
    else if (p.type.ty == Tident &&
             (cast(TypeIdentifier)p.type).ident.toString().length > 3 &&
             strncmp((cast(TypeIdentifier)p.type).ident.toChars(), "__T", 3) == 0)
    {
        // print parameter name, instead of undetermined type parameter
        buf.writestring(p.ident.toString());
    }
    else
    {
        typeToBuffer(p.type, p.ident, buf, hgs, (stc & STC.in_) ? MODFlags.const_ : 0);
    }

    if (p.defaultArg)
    {
        buf.writestring(" = ");
        p.defaultArg.expToBuffer(PREC.assign, buf, hgs);
    }
}


/**************************************************
 * Write out argument list to buf.
 * Params:
 *     expressions = argument list
 *     buf = buffer to write to
 *     hgs = context
 *     basis = replace `null`s in argument list with this expression (for sparse array literals)
 *     names = if non-null, use these as the names for the arguments
 */
private void argsToBuffer(Expressions* expressions, OutBuffer* buf, HdrGenState* hgs, Expression basis = null, Identifiers* names = null)
{
    if (!expressions || !expressions.length)
        return;
    version (all)
    {
        foreach (i, el; *expressions)
        {
            if (i)
                buf.writestring(", ");

            if (names && i < names.length && (*names)[i])
            {
                buf.writestring((*names)[i].toString());
                buf.writestring(": ");
            }
            if (!el)
                el = basis;
            if (el)
                expToBuffer(el, PREC.assign, buf, hgs);
        }
    }
    else
    {
        // Sparse style formatting, for debug use only
        //      [0..length: basis, 1: e1, 5: e5]
        if (basis)
        {
            buf.writestring("0..");
            buf.print(expressions.length);
            buf.writestring(": ");
            expToBuffer(basis, PREC.assign, buf, hgs);
        }
        foreach (i, el; *expressions)
        {
            if (el)
            {
                if (basis)
                {
                    buf.writestring(", ");
                    buf.print(i);
                    buf.writestring(": ");
                }
                else if (i)
                    buf.writestring(", ");
                expToBuffer(el, PREC.assign, buf, hgs);
            }
        }
    }
}

private void sizeToBuffer(Expression e, OutBuffer* buf, HdrGenState* hgs)
{
    if (e.type == Type.tsize_t)
    {
        Expression ex = (e.op == EXP.cast_ ? (cast(CastExp)e).e1 : e);
        ex = ex.optimize(WANTvalue);
        const dinteger_t uval = ex.op == EXP.int64 ? ex.toInteger() : cast(dinteger_t)-1;
        if (cast(sinteger_t)uval >= 0)
        {
            dinteger_t sizemax = void;
            if (target.ptrsize == 8)
                sizemax = 0xFFFFFFFFFFFFFFFFUL;
            else if (target.ptrsize == 4)
                sizemax = 0xFFFFFFFFU;
            else if (target.ptrsize == 2)
                sizemax = 0xFFFFU;
            else
                assert(0);
            if (uval <= sizemax && uval <= 0x7FFFFFFFFFFFFFFFUL)
            {
                buf.print(uval);
                return;
            }
        }
    }
    expToBuffer(e, PREC.assign, buf, hgs);
}

private void expressionToBuffer(Expression e, OutBuffer* buf, HdrGenState* hgs)
{
    expressionPrettyPrint(e, buf, hgs);
}

/**************************************************
 * Write expression out to buf, but wrap it
 * in ( ) if its precedence is less than pr.
 */
private void expToBuffer(Expression e, PREC pr, OutBuffer* buf, HdrGenState* hgs)
{
    debug
    {
        if (precedence[e.op] == PREC.zero)
            printf("precedence not defined for token '%s'\n", EXPtoString(e.op).ptr);
    }
    if (e.op == 0xFF)
    {
        buf.writestring("<FF>");
        return;
    }
    assert(precedence[e.op] != PREC.zero);
    assert(pr != PREC.zero);
    /* Despite precedence, we don't allow a<b<c expressions.
     * They must be parenthesized.
     */
    if (precedence[e.op] < pr || (pr == PREC.rel && precedence[e.op] == pr)
        || (pr >= PREC.or && pr <= PREC.and && precedence[e.op] == PREC.rel))
    {
        buf.writeByte('(');
        e.expressionToBuffer(buf, hgs);
        buf.writeByte(')');
    }
    else
    {
        e.expressionToBuffer(buf, hgs);
    }
}


/**************************************************
 * An entry point to pretty-print type.
 */
private void typeToBuffer(Type t, const Identifier ident, OutBuffer* buf, HdrGenState* hgs,
                          ubyte modMask = 0)
{
    if (auto tf = t.isTypeFunction())
    {
        visitFuncIdentWithPrefix(tf, ident, null, buf, hgs);
        return;
    }
    visitWithMask(t, modMask, buf, hgs);
    if (ident)
    {
        buf.writeByte(' ');
        buf.writestring(ident.toString());
    }
}

private void visitWithMask(Type t, ubyte modMask, OutBuffer* buf, HdrGenState* hgs)
{
    // Tuples and functions don't use the type constructor syntax
    if (modMask == t.mod || t.ty == Tfunction || t.ty == Ttuple)
    {
        typeToBufferx(t, buf, hgs);
    }
    else
    {
        ubyte m = t.mod & ~(t.mod & modMask);
        if (m & MODFlags.shared_)
        {
            MODtoBuffer(buf, MODFlags.shared_);
            buf.writeByte('(');
        }
        if (m & MODFlags.wild)
        {
            MODtoBuffer(buf, MODFlags.wild);
            buf.writeByte('(');
        }
        if (m & (MODFlags.const_ | MODFlags.immutable_))
        {
            MODtoBuffer(buf, m & (MODFlags.const_ | MODFlags.immutable_));
            buf.writeByte('(');
        }
        typeToBufferx(t, buf, hgs);
        if (m & (MODFlags.const_ | MODFlags.immutable_))
            buf.writeByte(')');
        if (m & MODFlags.wild)
            buf.writeByte(')');
        if (m & MODFlags.shared_)
            buf.writeByte(')');
    }
}


private void dumpTemplateInstance(TemplateInstance ti, OutBuffer* buf, HdrGenState* hgs)
{
    buf.writeByte('{');
    buf.writenl();
    buf.level++;

    if (ti.aliasdecl)
    {
        ti.aliasdecl.dsymbolToBuffer(buf, hgs);
        buf.writenl();
    }
    else if (ti.members)
    {
        foreach(m;*ti.members)
            m.dsymbolToBuffer(buf, hgs);
    }

    buf.level--;
    buf.writeByte('}');
    buf.writenl();

}

private void tiargsToBuffer(TemplateInstance ti, OutBuffer* buf, HdrGenState* hgs)
{
    buf.writeByte('!');
    if (ti.nest)
    {
        buf.writestring("(...)");
        return;
    }
    if (!ti.tiargs)
    {
        buf.writestring("()");
        return;
    }
    if (ti.tiargs.length == 1)
    {
        RootObject oarg = (*ti.tiargs)[0];
        if (Type t = isType(oarg))
        {
            if (t.equals(Type.tstring) || t.equals(Type.twstring) || t.equals(Type.tdstring) || t.mod == 0 && (t.isTypeBasic() || t.ty == Tident && (cast(TypeIdentifier)t).idents.length == 0))
            {
                buf.writestring(t.toChars());
                return;
            }
        }
        else if (Expression e = isExpression(oarg))
        {
            if (e.op == EXP.int64 || e.op == EXP.float64 || e.op == EXP.null_ || e.op == EXP.string_ || e.op == EXP.this_)
            {
                buf.writestring(e.toChars());
                return;
            }
        }
    }
    buf.writeByte('(');
    ti.nestUp();
    foreach (i, arg; *ti.tiargs)
    {
        if (i)
            buf.writestring(", ");
        objectToBuffer(arg, buf, hgs);
    }
    ti.nestDown();
    buf.writeByte(')');
}

/****************************************
 * This makes a 'pretty' version of the template arguments.
 * It's analogous to genIdent() which makes a mangled version.
 */
private void objectToBuffer(RootObject oarg, OutBuffer* buf, HdrGenState* hgs)
{
    //printf("objectToBuffer()\n");
    /* The logic of this should match what genIdent() does. The _dynamic_cast()
     * function relies on all the pretty strings to be unique for different classes
     * See https://issues.dlang.org/show_bug.cgi?id=7375
     * Perhaps it would be better to demangle what genIdent() does.
     */
    if (auto t = isType(oarg))
    {
        //printf("\tt: %s ty = %d\n", t.toChars(), t.ty);
        typeToBuffer(t, null, buf, hgs);
    }
    else if (auto e = isExpression(oarg))
    {
        if (e.op == EXP.variable)
            e = e.optimize(WANTvalue); // added to fix https://issues.dlang.org/show_bug.cgi?id=7375
        expToBuffer(e, PREC.assign, buf, hgs);
    }
    else if (Dsymbol s = isDsymbol(oarg))
    {
        const p = s.ident ? s.ident.toChars() : s.toChars();
        buf.writestring(p);
    }
    else if (auto v = isTuple(oarg))
    {
        auto args = &v.objects;
        foreach (i, arg; *args)
        {
            if (i)
                buf.writestring(", ");
            objectToBuffer(arg, buf, hgs);
        }
    }
    else if (auto p = isParameter(oarg))
    {
        parameterToBuffer(p, buf, hgs);
    }
    else if (!oarg)
    {
        buf.writestring("NULL");
    }
    else
    {
        debug
        {
            printf("bad Object = %p\n", oarg);
        }
        assert(0);
    }
}


private void visitFuncIdentWithPostfix(TypeFunction t, const char[] ident, OutBuffer* buf, HdrGenState* hgs, bool isStatic)
{
    if (t.inuse)
    {
        t.inuse = 2; // flag error to caller
        return;
    }
    t.inuse++;
    if (t.linkage > LINK.d && hgs.ddoc != 1 && !hgs.hdrgen)
    {
        linkageToBuffer(buf, t.linkage);
        buf.writeByte(' ');
    }
    if (t.linkage == LINK.objc && isStatic)
        buf.write("static ");
    if (t.next)
    {
        typeToBuffer(t.next, null, buf, hgs);
        if (ident)
            buf.writeByte(' ');
    }
    else if (hgs.ddoc)
        buf.writestring("auto ");
    if (ident)
        buf.writestring(ident);
    parametersToBuffer(t.parameterList, buf, hgs);
    /* Use postfix style for attributes
     */
    if (t.mod)
    {
        buf.writeByte(' ');
        MODtoBuffer(buf, t.mod);
    }

    void dg(string str)
    {
        buf.writeByte(' ');
        buf.writestring(str);
    }
    t.attributesApply(&dg);

    t.inuse--;
}

private void visitFuncIdentWithPrefix(TypeFunction t, const Identifier ident, TemplateDeclaration td,
    OutBuffer* buf, HdrGenState* hgs)
{
    if (t.inuse)
    {
        t.inuse = 2; // flag error to caller
        return;
    }
    t.inuse++;

    /* Use 'storage class' (prefix) style for attributes
     */
    if (t.mod)
    {
        MODtoBuffer(buf, t.mod);
        buf.writeByte(' ');
    }

    void ignoreReturn(string str)
    {
        if (str != "return")
        {
            // don't write 'ref' for ctors
            if ((ident == Id.ctor) && str == "ref")
                return;
            buf.writestring(str);
            buf.writeByte(' ');
        }
    }
    t.attributesApply(&ignoreReturn);

    if (t.linkage > LINK.d && hgs.ddoc != 1 && !hgs.hdrgen)
    {
        linkageToBuffer(buf, t.linkage);
        buf.writeByte(' ');
    }
    if (ident && ident.toHChars2() != ident.toChars())
    {
        // Don't print return type for ctor, dtor, unittest, etc
    }
    else if (t.next)
    {
        typeToBuffer(t.next, null, buf, hgs);
        if (ident)
            buf.writeByte(' ');
    }
    else if (hgs.ddoc)
        buf.writestring("auto ");
    if (ident)
        buf.writestring(ident.toHChars2());
    if (td)
    {
        buf.writeByte('(');
        foreach (i, p; *td.origParameters)
        {
            if (i)
                buf.writestring(", ");
            p.templateParameterToBuffer(buf, hgs);
        }
        buf.writeByte(')');
    }
    parametersToBuffer(t.parameterList, buf, hgs);
    if (t.isreturn)
    {
        buf.writestring(" return");
    }
    t.inuse--;
}


private void initializerToBuffer(Initializer inx, OutBuffer* buf, HdrGenState* hgs)
{
    void visitError(ErrorInitializer iz)
    {
        buf.writestring("__error__");
    }

    void visitVoid(VoidInitializer iz)
    {
        buf.writestring("void");
    }

    void visitStruct(StructInitializer si)
    {
        //printf("StructInitializer::toCBuffer()\n");
        buf.writeByte('{');
        foreach (i, const id; si.field)
        {
            if (i)
                buf.writestring(", ");
            if (id)
            {
                buf.writestring(id.toString());
                buf.writeByte(':');
            }
            if (auto iz = si.value[i])
                initializerToBuffer(iz, buf, hgs);
        }
        buf.writeByte('}');
    }

    void visitArray(ArrayInitializer ai)
    {
        buf.writeByte('[');
        foreach (i, ex; ai.index)
        {
            if (i)
                buf.writestring(", ");
            if (ex)
            {
                ex.expressionToBuffer(buf, hgs);
                buf.writeByte(':');
            }
            if (auto iz = ai.value[i])
                initializerToBuffer(iz, buf, hgs);
        }
        buf.writeByte(']');
    }

    void visitExp(ExpInitializer ei)
    {
        ei.exp.expressionToBuffer(buf, hgs);
    }

    void visitC(CInitializer ci)
    {
        buf.writeByte('{');
        foreach (i, ref DesigInit di; ci.initializerList)
        {
            if (i)
                buf.writestring(", ");
            if (di.designatorList)
            {
                foreach (ref Designator d; (*di.designatorList)[])
                {
                    if (d.exp)
                    {
                        buf.writeByte('[');
                        toCBuffer(d.exp, buf, hgs);
                        buf.writeByte(']');
                    }
                    else
                    {
                        buf.writeByte('.');
                        buf.writestring(d.ident.toString());
                    }
                }
                buf.writeByte('=');
            }
            initializerToBuffer(di.initializer, buf, hgs);
        }
        buf.writeByte('}');
    }

    final switch (inx.kind)
    {
        case InitKind.error:   return visitError (inx.isErrorInitializer ());
        case InitKind.void_:   return visitVoid  (inx.isVoidInitializer  ());
        case InitKind.struct_: return visitStruct(inx.isStructInitializer());
        case InitKind.array:   return visitArray (inx.isArrayInitializer ());
        case InitKind.exp:     return visitExp   (inx.isExpInitializer   ());
        case InitKind.C_:      return visitC     (inx.isCInitializer     ());
    }
}


private void typeToBufferx(Type t, OutBuffer* buf, HdrGenState* hgs)
{
    void visitType(Type t)
    {
        printf("t = %p, ty = %d\n", t, t.ty);
        assert(0);
    }

    void visitError(TypeError t)
    {
        buf.writestring("_error_");
    }

    void visitBasic(TypeBasic t)
    {
        //printf("TypeBasic::toCBuffer2(t.mod = %d)\n", t.mod);
        buf.writestring(t.dstring);
    }

    void visitTraits(TypeTraits t)
    {
        //printf("TypeBasic::toCBuffer2(t.mod = %d)\n", t.mod);
        t.exp.expressionToBuffer(buf, hgs);
    }

    void visitVector(TypeVector t)
    {
        //printf("TypeVector::toCBuffer2(t.mod = %d)\n", t.mod);
        buf.writestring("__vector(");
        visitWithMask(t.basetype, t.mod, buf, hgs);
        buf.writestring(")");
    }

    void visitSArray(TypeSArray t)
    {
        visitWithMask(t.next, t.mod, buf, hgs);
        buf.writeByte('[');
        sizeToBuffer(t.dim, buf, hgs);
        buf.writeByte(']');
    }

    void visitDArray(TypeDArray t)
    {
        Type ut = t.castMod(0);
        if (hgs.declstring)
            goto L1;
        if (ut.equals(Type.tstring))
            buf.writestring("string");
        else if (ut.equals(Type.twstring))
            buf.writestring("wstring");
        else if (ut.equals(Type.tdstring))
            buf.writestring("dstring");
        else
        {
        L1:
            visitWithMask(t.next, t.mod, buf, hgs);
            buf.writestring("[]");
        }
    }

    void visitAArray(TypeAArray t)
    {
        visitWithMask(t.next, t.mod, buf, hgs);
        buf.writeByte('[');
        visitWithMask(t.index, 0, buf, hgs);
        buf.writeByte(']');
    }

    void visitPointer(TypePointer t)
    {
        //printf("TypePointer::toCBuffer2() next = %d\n", t.next.ty);
        if (t.next.ty == Tfunction)
            visitFuncIdentWithPostfix(cast(TypeFunction)t.next, "function", buf, hgs, false);
        else
        {
            visitWithMask(t.next, t.mod, buf, hgs);
            buf.writeByte('*');
        }
    }

    void visitReference(TypeReference t)
    {
        visitWithMask(t.next, t.mod, buf, hgs);
        buf.writeByte('&');
    }

    void visitFunction(TypeFunction t)
    {
        //printf("TypeFunction::toCBuffer2() t = %p, ref = %d\n", t, t.isref);
        visitFuncIdentWithPostfix(t, null, buf, hgs, false);
    }

    void visitDelegate(TypeDelegate t)
    {
        visitFuncIdentWithPostfix(cast(TypeFunction)t.next, "delegate", buf, hgs, false);
    }

    void visitTypeQualifiedHelper(TypeQualified t)
    {
        foreach (id; t.idents)
        {
            switch (id.dyncast()) with (DYNCAST)
            {
            case dsymbol:
                buf.writeByte('.');
                TemplateInstance ti = cast(TemplateInstance)id;
                ti.dsymbolToBuffer(buf, hgs);
                break;
            case expression:
                buf.writeByte('[');
                (cast(Expression)id).expressionToBuffer(buf, hgs);
                buf.writeByte(']');
                break;
            case type:
                buf.writeByte('[');
                typeToBufferx(cast(Type)id, buf, hgs);
                buf.writeByte(']');
                break;
            default:
                buf.writeByte('.');
                buf.writestring(id.toString());
            }
        }
    }

    void visitIdentifier(TypeIdentifier t)
    {
        buf.writestring(t.ident.toString());
        visitTypeQualifiedHelper(t);
    }

    void visitInstance(TypeInstance t)
    {
        t.tempinst.dsymbolToBuffer(buf, hgs);
        visitTypeQualifiedHelper(t);
    }

    void visitTypeof(TypeTypeof t)
    {
        buf.writestring("typeof(");
        t.exp.expressionToBuffer(buf, hgs);
        buf.writeByte(')');
        visitTypeQualifiedHelper(t);
    }

    void visitReturn(TypeReturn t)
    {
        buf.writestring("typeof(return)");
        visitTypeQualifiedHelper(t);
    }

    void visitEnum(TypeEnum t)
    {
        buf.writestring(hgs.fullQual ? t.sym.toPrettyChars() : t.sym.toChars());
    }

    void visitStruct(TypeStruct t)
    {
        // https://issues.dlang.org/show_bug.cgi?id=13776
        // Don't use ti.toAlias() to avoid forward reference error
        // while printing messages.
        TemplateInstance ti = t.sym.parent ? t.sym.parent.isTemplateInstance() : null;
        if (ti && ti.aliasdecl == t.sym)
            buf.writestring(hgs.fullQual ? ti.toPrettyChars() : ti.toChars());
        else
            buf.writestring(hgs.fullQual ? t.sym.toPrettyChars() : t.sym.toChars());
    }

    void visitClass(TypeClass t)
    {
        // https://issues.dlang.org/show_bug.cgi?id=13776
        // Don't use ti.toAlias() to avoid forward reference error
        // while printing messages.
        TemplateInstance ti = t.sym.parent ? t.sym.parent.isTemplateInstance() : null;
        if (ti && ti.aliasdecl == t.sym)
            buf.writestring(hgs.fullQual ? ti.toPrettyChars() : ti.toChars());
        else
            buf.writestring(hgs.fullQual ? t.sym.toPrettyChars() : t.sym.toChars());
    }

    void visitTag(TypeTag t)
    {
        if (t.mod & MODFlags.const_)
            buf.writestring("const ");
        buf.writestring(Token.toChars(t.tok));
        buf.writeByte(' ');
        if (t.id)
            buf.writestring(t.id.toChars());
        if (t.tok == TOK.enum_ && t.base.ty != TY.Tint32)
        {
            buf.writestring(" : ");
            visitWithMask(t.base, t.mod, buf, hgs);
        }
    }

    void visitTuple(TypeTuple t)
    {
        parametersToBuffer(ParameterList(t.arguments, VarArg.none), buf, hgs);
    }

    void visitSlice(TypeSlice t)
    {
        visitWithMask(t.next, t.mod, buf, hgs);
        buf.writeByte('[');
        sizeToBuffer(t.lwr, buf, hgs);
        buf.writestring(" .. ");
        sizeToBuffer(t.upr, buf, hgs);
        buf.writeByte(']');
    }

    void visitNull(TypeNull t)
    {
        buf.writestring("typeof(null)");
    }

    void visitMixin(TypeMixin t)
    {
        buf.writestring("mixin(");
        argsToBuffer(t.exps, buf, hgs, null);
        buf.writeByte(')');
    }

    void visitNoreturn(TypeNoreturn t)
    {
        buf.writestring("noreturn");
    }


    switch (t.ty)
    {
        default:        return t.isTypeBasic() ?
                                visitBasic(cast(TypeBasic)t) :
                                visitType(t);

        case Terror:     return visitError(cast(TypeError)t);
        case Ttraits:    return visitTraits(cast(TypeTraits)t);
        case Tvector:    return visitVector(cast(TypeVector)t);
        case Tsarray:    return visitSArray(cast(TypeSArray)t);
        case Tarray:     return visitDArray(cast(TypeDArray)t);
        case Taarray:    return visitAArray(cast(TypeAArray)t);
        case Tpointer:   return visitPointer(cast(TypePointer)t);
        case Treference: return visitReference(cast(TypeReference)t);
        case Tfunction:  return visitFunction(cast(TypeFunction)t);
        case Tdelegate:  return visitDelegate(cast(TypeDelegate)t);
        case Tident:     return visitIdentifier(cast(TypeIdentifier)t);
        case Tinstance:  return visitInstance(cast(TypeInstance)t);
        case Ttypeof:    return visitTypeof(cast(TypeTypeof)t);
        case Treturn:    return visitReturn(cast(TypeReturn)t);
        case Tenum:      return visitEnum(cast(TypeEnum)t);
        case Tstruct:    return visitStruct(cast(TypeStruct)t);
        case Tclass:     return visitClass(cast(TypeClass)t);
        case Ttuple:     return visitTuple (cast(TypeTuple)t);
        case Tslice:     return visitSlice(cast(TypeSlice)t);
        case Tnull:      return visitNull(cast(TypeNull)t);
        case Tmixin:     return visitMixin(cast(TypeMixin)t);
        case Tnoreturn:  return visitNoreturn(cast(TypeNoreturn)t);
        case Ttag:       return visitTag(cast(TypeTag)t);
    }
}

/****************************************
 * Convert EXP to char*.
 */

string EXPtoString(EXP op)
{
    static immutable char*[EXP.max + 1] strings =
    [
        EXP.type : "type",
        EXP.error : "error",
        EXP.objcClassReference : "class",

        EXP.typeof_ : "typeof",
        EXP.mixin_ : "mixin",

        EXP.import_ : "import",
        EXP.dotVariable : "dotvar",
        EXP.scope_ : "scope",
        EXP.identifier : "identifier",
        EXP.this_ : "this",
        EXP.super_ : "super",
        EXP.int64 : "long",
        EXP.float64 : "double",
        EXP.complex80 : "creal",
        EXP.null_ : "null",
        EXP.string_ : "string",
        EXP.arrayLiteral : "arrayliteral",
        EXP.assocArrayLiteral : "assocarrayliteral",
        EXP.classReference : "classreference",
        EXP.file : "__FILE__",
        EXP.fileFullPath : "__FILE_FULL_PATH__",
        EXP.line : "__LINE__",
        EXP.moduleString : "__MODULE__",
        EXP.functionString : "__FUNCTION__",
        EXP.prettyFunction : "__PRETTY_FUNCTION__",
        EXP.typeid_ : "typeid",
        EXP.is_ : "is",
        EXP.assert_ : "assert",
        EXP.halt : "halt",
        EXP.template_ : "template",
        EXP.dSymbol : "symbol",
        EXP.function_ : "function",
        EXP.variable : "var",
        EXP.symbolOffset : "symoff",
        EXP.structLiteral : "structLiteral",
        EXP.compoundLiteral : "compoundliteral",
        EXP.arrayLength : "arraylength",
        EXP.delegatePointer : "delegateptr",
        EXP.delegateFunctionPointer : "delegatefuncptr",
        EXP.remove : "remove",
        EXP.tuple : "tuple",
        EXP.traits : "__traits",
        EXP.default_ : "default",
        EXP.overloadSet : "__overloadset",
        EXP.void_ : "void",
        EXP.vectorArray : "vectorarray",
        EXP._Generic : "_Generic",

        // post
        EXP.dotTemplateInstance : "dotti",
        EXP.dotIdentifier : "dotid",
        EXP.dotTemplateDeclaration : "dottd",
        EXP.dot : ".",
        EXP.dotType : "dottype",
        EXP.plusPlus : "++",
        EXP.minusMinus : "--",
        EXP.prePlusPlus : "++",
        EXP.preMinusMinus : "--",
        EXP.call : "call",
        EXP.slice : "..",
        EXP.array : "[]",
        EXP.index : "[i]",

        EXP.delegate_ : "delegate",
        EXP.address : "&",
        EXP.star : "*",
        EXP.negate : "-",
        EXP.uadd : "+",
        EXP.not : "!",
        EXP.tilde : "~",
        EXP.delete_ : "delete",
        EXP.new_ : "new",
        EXP.newAnonymousClass : "newanonclass",
        EXP.cast_ : "cast",

        EXP.vector : "__vector",
        EXP.pow : "^^",

        EXP.mul : "*",
        EXP.div : "/",
        EXP.mod : "%",

        EXP.add : "+",
        EXP.min : "-",
        EXP.concatenate : "~",

        EXP.leftShift : "<<",
        EXP.rightShift : ">>",
        EXP.unsignedRightShift : ">>>",

        EXP.lessThan : "<",
        EXP.lessOrEqual : "<=",
        EXP.greaterThan : ">",
        EXP.greaterOrEqual : ">=",
        EXP.in_ : "in",

        EXP.equal : "==",
        EXP.notEqual : "!=",
        EXP.identity : "is",
        EXP.notIdentity : "!is",

        EXP.and : "&",
        EXP.xor : "^",
        EXP.or : "|",

        EXP.andAnd : "&&",
        EXP.orOr : "||",

        EXP.question : "?",

        EXP.assign : "=",
        EXP.construct : "=",
        EXP.blit : "=",
        EXP.addAssign : "+=",
        EXP.minAssign : "-=",
        EXP.concatenateAssign : "~=",
        EXP.concatenateElemAssign : "~=",
        EXP.concatenateDcharAssign : "~=",
        EXP.mulAssign : "*=",
        EXP.divAssign : "/=",
        EXP.modAssign : "%=",
        EXP.powAssign : "^^=",
        EXP.leftShiftAssign : "<<=",
        EXP.rightShiftAssign : ">>=",
        EXP.unsignedRightShiftAssign : ">>>=",
        EXP.andAssign : "&=",
        EXP.orAssign : "|=",
        EXP.xorAssign : "^=",

        EXP.comma : ",",
        EXP.declaration : "declaration",

        EXP.interval : "interval",
    ];
    const p = strings[op];
    if (!p)
    {
        printf("error: EXP %d has no string\n", op);
        return "XXXXX";
        //assert(0);
    }
    assert(p);
    return p[0 .. strlen(p)];
}
