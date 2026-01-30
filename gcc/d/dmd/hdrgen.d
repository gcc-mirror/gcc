/**
 * Generate $(LINK2 https://dlang.org/dmd-windows.html#interface-files, D interface files).
 *
 * Also used to convert AST nodes to D code in general, e.g. for error messages or `printf` debugging.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/hdrgen.d, _hdrgen.d)
 * Documentation:  https://dlang.org/phobos/dmd_hdrgen.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/hdrgen.d
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
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.mtype;
import dmd.nspace;
import dmd.optimize;
import dmd.parse;
import dmd.root.complex;
import dmd.root.ctfloat;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.root.string;
import dmd.statement;
import dmd.staticassert;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

struct HdrGenState
{
    bool hdrgen;        /// true if generating header file
    bool ddoc;          /// true if generating Ddoc file
    bool fullDump;      /// true if generating a full AST dump file
    bool importcHdr;    /// true if generating a .di file from an ImportC file
    bool inCAlias;      /// Set to prevent ImportC translating typedefs as `alias X = X`
    bool doFuncBodies;  /// include function bodies in output
    bool vcg_ast;       /// write out codegen-ast
    bool skipConstraints;  // skip constraints when doing templates
    bool showOneMember = true;
    bool errorMsg;      /// true if formatting for inside an error message

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

/****************************************
 * Generate a header (.di) file for Module m.
 * Params:
 *      m = Module to generate header for
 *      doFuncBodies = generate function definitions rather than just declarations
 *      buf = buffer to write the data to
 */
void genhdrfile(Module m, bool doFuncBodies, ref OutBuffer buf)
{
    buf.doindent = 1;
    buf.printf("// D import file generated from '%s'", m.srcfile.toChars());
    buf.writenl();
    HdrGenState hgs;
    hgs.hdrgen = true;
    hgs.importcHdr = (m.filetype == FileType.c);
    hgs.doFuncBodies = doFuncBodies;
    toCBuffer(m, buf, hgs);
}

/**
 * Convert `o` to a string for error messages.
 * Params:
 *      e = object to convert
 * Returns: string representation of `e`
 */
const(char)* toErrMsg(const RootObject o)
{
    if (auto e = o.isExpression())
        return toErrMsg(e);
    if (auto d = o.isDsymbol())
        return toErrMsg(d);
    if (auto t = o.isType())
        return t.toChars();
    if (auto id = o.isIdentifier())
        return id.toChars();
    assert(0);
}

/// ditto
const(char)* toErrMsg(const Expression e)
{
    HdrGenState hgs;
    hgs.errorMsg = true;
    OutBuffer buf;
    toCBuffer(e, buf, hgs);
    truncateForError(buf, 60);

    return buf.extractChars();
}

/// ditto
const(char)* toErrMsg(const Dsymbol d)
{
    if (d.isFuncDeclaration() || d.isTemplateInstance())
    {
        if (d.ident && d.ident.toString.startsWith("__") && !d.isCtorDeclaration())
        {
            HdrGenState hgs;
            hgs.errorMsg = true;
            OutBuffer buf;
            toCBuffer(cast() d, buf, hgs);
            truncateForError(buf, 80);
            return buf.extractChars();
        }
    }

    return d.toChars();
}

/**
 * Make the content of `buf` fit inline for an error message.
 * Params:
 *   buf = buffer with text to modify
 *   maxLength = truncate text when it exceeds this length
 */
private void truncateForError(ref OutBuffer buf, size_t maxLength)
{
    // Remove newlines, escape backticks ` by doubling them
    for (size_t i = 0; i < buf.length; i++)
    {
        if (buf[i] == '\r')
            buf.remove(i, 1);
        if (buf[i] == '\n')
            buf.peekSlice[i] = ' ';
        if (buf[i] == '`')
            i = buf.insert(i, "`");
    }

    // Strip trailing whitespace
    while (buf.length && buf[$-1] == ' ')
        buf.setsize(buf.length - 1);

    // Truncate
    if (buf.length > maxLength)
    {
        buf.setsize(maxLength - 3);
        buf.put("...");
    }
}

/***************************************
 * Turn a Statement into a string suitable for printf.
 * Leaks memory.
 * Params:
 *      s = Statement to convert
 * Returns:
 *      0-terminated string
 */
public const(char)* toChars(const Statement s)
{
    HdrGenState hgs;
    OutBuffer buf;
    toCBuffer(s, buf, hgs);
    buf.put(cast(ubyte)0);
    return buf.extractSlice().ptr;
}

public const(char)* toChars(const Expression e)
{
    HdrGenState hgs;
    OutBuffer buf;
    toCBuffer(e, buf, hgs);
    return buf.extractChars();
}

public const(char)* toChars(const Initializer i)
{
    OutBuffer buf;
    HdrGenState hgs;
    toCBuffer(i, buf, hgs);
    return buf.extractChars();
}

public const(char)* toChars(const Type t)
{
    OutBuffer buf;
    buf.reserve(16);
    HdrGenState hgs;
    hgs.fullQual = (t.ty == Tclass && !t.mod);

    toCBuffer(t, buf, null, hgs);
    return buf.extractChars();
}

public const(char)* toChars(const Dsymbol d)
{
    if (auto td = d.isTemplateDeclaration())
    {
        HdrGenState hgs;
        OutBuffer buf;
        toCharsMaybeConstraints(td, buf, hgs);
        return buf.extractChars();
    }

    if (auto ti = d.isTemplateInstance())
    {
        OutBuffer buf;
        toCBufferInstance(ti, buf);
        return buf.extractChars();
    }

    if (auto tm = d.isTemplateMixin())
    {
        OutBuffer buf;
        toCBufferInstance(tm, buf);
        return buf.extractChars();
    }

    if (auto tid = d.isTypeInfoDeclaration())
    {
        OutBuffer buf;
        buf.put("typeid(");
        buf.put(tid.tinfo.toChars());
        buf.put(')');
        return buf.extractChars();
    }

    return d.ident ? d.ident.toHChars2() : "__anonymous";
}

public const(char)[] toString(const Initializer i)
{
    OutBuffer buf;
    HdrGenState hgs;
    toCBuffer(i, buf, hgs);
    return buf.extractSlice();
}

/**
 * Dumps the full contents of module `m` to `buf`.
 * Params:
 *   buf = buffer to write to.
 *   vcg_ast = write out codegen ast
 *   m = module to visit all members of.
 */
void moduleToBuffer(ref OutBuffer buf, bool vcg_ast, Module m)
{
    HdrGenState hgs;
    hgs.fullDump = true;
    hgs.vcg_ast = vcg_ast;
    toCBuffer(m, buf, hgs);
}

void moduleToBuffer2(Module m, ref OutBuffer buf, ref HdrGenState hgs)
{
    if (m.md)
    {
        if (m.userAttribDecl)
        {
            buf.put("@(");
            argsToBuffer(m.userAttribDecl.atts, buf, hgs);
            buf.put(')');
            buf.writenl();
        }
        if (m.md.isdeprecated)
        {
            if (m.md.msg)
            {
                buf.put("deprecated(");
                m.md.msg.expressionToBuffer(buf, hgs);
                buf.put(") ");
            }
            else
                buf.put("deprecated ");
        }
        buf.put("module ");
        buf.put(m.md.toChars());
        buf.put(';');
        buf.writenl();
    }

    foreach (s; *m.members)
    {
        s.dsymbolToBuffer(buf, hgs);
    }
}

private void statementToBuffer(Statement s, ref OutBuffer buf, ref HdrGenState hgs)
{
    void visitDefaultCase(Statement s)
    {
        printf("Statement::toCBuffer() %d\n", s.stmt);
        assert(0, "unrecognized statement in statementToBuffer()");
    }

    void visitError(ErrorStatement s)
    {
        buf.put("__error__");
        buf.writenl();
    }

    void visitExp(ExpStatement s)
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
        buf.put(';');
        if (!hgs.forStmtInit)
            buf.writenl();
    }

    void visitDtorExp(DtorExpStatement s)
    {
        visitExp(s);
    }

    void visitMixin(MixinStatement s)
    {
        buf.put("mixin(");
        argsToBuffer(s.exps, buf, hgs, null);
        buf.put(");");
        if (!hgs.forStmtInit)
            buf.writenl();
    }

    void visitCompound(CompoundStatement s)
    {
        foreach (sx; *s.statements)
        {
            if (sx)
                sx.statementToBuffer(buf, hgs);
        }
    }

    void visitCompoundAsm(CompoundAsmStatement s)
    {
        visitCompound(s);
    }

    void visitCompoundDeclaration(CompoundDeclarationStatement s)
    {
        bool anywritten = false;
        foreach (sx; *s.statements)
        {
            auto ds = sx ? sx.isExpStatement() : null;
            if (ds && ds.exp.isDeclarationExp())
            {
                auto d = ds.exp.isDeclarationExp().declaration;
                if (auto v = d.isVarDeclaration())
                {
                    visitVarDecl(v, anywritten, buf, hgs);
                }
                else
                    d.dsymbolToBuffer(buf, hgs);
                anywritten = true;
            }
        }
        buf.put(';');
        if (!hgs.forStmtInit)
            buf.writenl();
    }

    void visitUnrolledLoop(UnrolledLoopStatement s)
    {
        buf.put("/*unrolled*/ {");
        buf.writenl();
        buf.level++;
        foreach (sx; *s.statements)
        {
            if (sx)
                sx.statementToBuffer(buf, hgs);
        }
        buf.level--;
        buf.put('}');
        buf.writenl();
    }

    void visitScope(ScopeStatement s)
    {
        buf.put('{');
        buf.writenl();
        buf.level++;
        if (s.statement)
            s.statement.statementToBuffer(buf, hgs);
        buf.level--;
        buf.put('}');
        buf.writenl();
    }

    void printConditionAssignment(Parameter p, Expression condition)
    {
        if (p)
        {
            // Print condition assignment
            STC stc = p.storageClass;
            if (!p.type && !stc)
                stc = STC.auto_;
            if (stcToBuffer(buf, stc))
                buf.put(' ');
            if (p.type)
                typeToBuffer(p.type, p.ident, buf, hgs);
            else
                buf.put(p.ident.toString());
            buf.put(" = ");
        }
        condition.expressionToBuffer(buf, hgs);
    }

    void visitWhile(WhileStatement s)
    {
        buf.put("while (");
        printConditionAssignment(s.param, s.condition);
        buf.put(')');
        buf.writenl();
        if (s._body)
            s._body.statementToBuffer(buf, hgs);
    }

    void visitDo(DoStatement s)
    {
        buf.put("do");
        buf.writenl();
        if (s._body)
            s._body.statementToBuffer(buf, hgs);
        buf.put("while (");
        s.condition.expressionToBuffer(buf, hgs);
        buf.put(");");
        buf.writenl();
    }

    void visitFor(ForStatement s)
    {
        buf.put("for (");
        if (s._init)
        {
            hgs.forStmtInit++;
            s._init.statementToBuffer(buf, hgs);
            hgs.forStmtInit--;
        }
        else
            buf.put(';');
        if (s.condition)
        {
            buf.put(' ');
            s.condition.expressionToBuffer(buf, hgs);
        }
        buf.put(';');
        if (s.increment)
        {
            buf.put(' ');
            s.increment.expressionToBuffer(buf, hgs);
        }
        buf.put(')');
        buf.writenl();
        buf.put('{');
        buf.writenl();
        buf.level++;
        if (s._body)
            s._body.statementToBuffer(buf, hgs);
        buf.level--;
        buf.put('}');
        buf.writenl();
    }

    void foreachWithoutBody(ForeachStatement s)
    {
        buf.put(Token.toString(s.op));
        buf.put(" (");
        foreach (i, p; *s.parameters)
        {
            if (i)
                buf.put(", ");
            if (stcToBuffer(buf, p.storageClass))
                buf.put(' ');
            if (p.type)
                typeToBuffer(p.type, p.ident, buf, hgs);
            else
                buf.put(p.ident.toString());
        }
        buf.put("; ");
        s.aggr.expressionToBuffer(buf, hgs);
        buf.put(')');
        buf.writenl();
    }

    void visitForeach(ForeachStatement s)
    {
        foreachWithoutBody(s);
        buf.put('{');
        buf.writenl();
        buf.level++;
        if (s._body)
            s._body.statementToBuffer(buf, hgs);
        buf.level--;
        buf.put('}');
        buf.writenl();
    }

    void foreachRangeWithoutBody(ForeachRangeStatement s)
    {
        buf.put(Token.toString(s.op));
        buf.put(" (");
        if (s.param.type)
            typeToBuffer(s.param.type, s.param.ident, buf, hgs);
        else
            buf.put(s.param.ident.toString());
        buf.put("; ");
        s.lwr.expressionToBuffer(buf, hgs);
        buf.put(" .. ");
        s.upr.expressionToBuffer(buf, hgs);
        buf.put(')');
        buf.writenl();
    }

    void visitForeachRange(ForeachRangeStatement s)
    {
        foreachRangeWithoutBody(s);
        buf.put('{');
        buf.writenl();
        buf.level++;
        if (s._body)
            s._body.statementToBuffer(buf, hgs);
        buf.level--;
        buf.put('}');
        buf.writenl();
    }

    void visitStaticForeach(StaticForeachStatement s)
    {
        buf.put("static ");
        if (s.sfe.aggrfe)
        {
            visitForeach(s.sfe.aggrfe);
        }
        else
        {
            assert(s.sfe.rangefe);
            visitForeachRange(s.sfe.rangefe);
        }
    }

    void visitForwarding(ForwardingStatement s)
    {
        s.statement.statementToBuffer(buf, hgs);
    }

    void visitIf(IfStatement s)
    {
        buf.put("if (");
        printConditionAssignment(s.param, s.condition);
        buf.put(')');
        buf.writenl();
        if (s.ifbody.isScopeStatement())
        {
            s.ifbody.statementToBuffer(buf, hgs);
        }
        else
        {
            buf.level++;
            s.ifbody.statementToBuffer(buf, hgs);
            buf.level--;
        }
        if (s.elsebody)
        {
            buf.put("else");
            if (!s.elsebody.isIfStatement())
            {
                buf.writenl();
            }
            else
            {
                buf.put(' ');
            }
            if (s.elsebody.isScopeStatement() || s.elsebody.isIfStatement())
            {
                s.elsebody.statementToBuffer(buf, hgs);
            }
            else
            {
                buf.level++;
                s.elsebody.statementToBuffer(buf, hgs);
                buf.level--;
            }
        }
    }

    void visitConditional(ConditionalStatement s)
    {
        s.condition.conditionToBuffer(buf, hgs);
        buf.writenl();
        buf.put('{');
        buf.writenl();
        buf.level++;
        if (s.ifbody)
            s.ifbody.statementToBuffer(buf, hgs);
        buf.level--;
        buf.put('}');
        buf.writenl();
        if (s.elsebody)
        {
            buf.put("else");
            buf.writenl();
            buf.put('{');
            buf.level++;
            buf.writenl();
            s.elsebody.statementToBuffer(buf, hgs);
            buf.level--;
            buf.put('}');
        }
        buf.writenl();
    }

    void visitPragma(PragmaStatement s)
    {
        buf.put("pragma (");
        buf.put(s.ident.toString());
        if (s.args && s.args.length)
        {
            buf.put(", ");
            argsToBuffer(s.args, buf, hgs);
        }
        buf.put(')');
        if (s._body)
        {
            buf.writenl();
            buf.put('{');
            buf.writenl();
            buf.level++;
            s._body.statementToBuffer(buf, hgs);
            buf.level--;
            buf.put('}');
            buf.writenl();
        }
        else
        {
            buf.put(';');
            buf.writenl();
        }
    }

    void visitStaticAssert(StaticAssertStatement s)
    {
        s.sa.dsymbolToBuffer(buf, hgs);
    }

    void visitSwitch(SwitchStatement s)
    {
        buf.put(s.isFinal ? "final switch (" : "switch (");
        printConditionAssignment(s.param, s.condition);
        buf.put(')');
        buf.writenl();
        if (s._body)
        {
            if (!s._body.isScopeStatement())
            {
                buf.put('{');
                buf.writenl();
                buf.level++;
                s._body.statementToBuffer(buf, hgs);
                buf.level--;
                buf.put('}');
                buf.writenl();
            }
            else
            {
                s._body.statementToBuffer(buf, hgs);
            }
        }
    }

    void visitCase(CaseStatement s)
    {
        buf.put("case ");
        s.exp.expressionToBuffer(buf, hgs);
        buf.put(':');
        buf.writenl();
        s.statement.statementToBuffer(buf, hgs);
    }

    void visitCaseRange(CaseRangeStatement s)
    {
        buf.put("case ");
        s.first.expressionToBuffer(buf, hgs);
        buf.put(": .. case ");
        s.last.expressionToBuffer(buf, hgs);
        buf.put(':');
        buf.writenl();
        s.statement.statementToBuffer(buf, hgs);
    }

    void visitDefault(DefaultStatement s)
    {
        buf.put("default:");
        buf.writenl();
        s.statement.statementToBuffer(buf, hgs);
    }

    void visitGotoDefault(GotoDefaultStatement s)
    {
        buf.put("goto default;");
        buf.writenl();
    }

    void visitGotoCase(GotoCaseStatement s)
    {
        buf.put("goto case");
        if (s.exp)
        {
            buf.put(' ');
            s.exp.expressionToBuffer(buf, hgs);
        }
        buf.put(';');
        buf.writenl();
    }

    void visitSwitchError(SwitchErrorStatement s)
    {
        buf.put("SwitchErrorStatement::toCBuffer()");
        buf.writenl();
    }

    void visitReturn(ReturnStatement s)
    {
        buf.put("return ");
        if (s.exp)
            s.exp.expressionToBuffer(buf, hgs);
        buf.put(';');
        buf.writenl();
    }

    void visitBreak(BreakStatement s)
    {
        buf.put("break");
        if (s.ident)
        {
            buf.put(' ');
            buf.put(s.ident.toString());
        }
        buf.put(';');
        buf.writenl();
    }

    void visitContinue(ContinueStatement s)
    {
        buf.put("continue");
        if (s.ident)
        {
            buf.put(' ');
            buf.put(s.ident.toString());
        }
        buf.put(';');
        buf.writenl();
    }

    void visitSynchronized(SynchronizedStatement s)
    {
        buf.put("synchronized");
        if (s.exp)
        {
            buf.put('(');
            s.exp.expressionToBuffer(buf, hgs);
            buf.put(')');
        }
        if (s._body)
        {
            buf.put(' ');
            s._body.statementToBuffer(buf, hgs);
        }
    }

    void visitWith(WithStatement s)
    {
        buf.put("with (");
        s.exp.expressionToBuffer(buf, hgs);
        buf.put(")");
        buf.writenl();
        if (s._body)
            s._body.statementToBuffer(buf, hgs);
    }

    void visitTryCatch(TryCatchStatement s)
    {
        buf.put("try");
        buf.writenl();
        if (s._body)
        {
            if (s._body.isScopeStatement())
            {
                s._body.statementToBuffer(buf, hgs);
            }
            else
            {
                buf.level++;
                s._body.statementToBuffer(buf, hgs);
                buf.level--;
            }
        }
        foreach (c; *s.catches)
        {
            buf.put("catch");
            if (c.type)
            {
                buf.put('(');
                typeToBuffer(c.type, c.ident, buf, hgs);
                buf.put(')');
            }
            buf.writenl();
            buf.put('{');
            buf.writenl();
            buf.level++;
            if (c.handler)
                c.handler.statementToBuffer(buf, hgs);
            buf.level--;
            buf.put('}');
            buf.writenl();
        }
    }

    void visitTryFinally(TryFinallyStatement s)
    {
        buf.put("try");
        buf.writenl();
        buf.put('{');
        buf.writenl();
        buf.level++;
        s._body.statementToBuffer(buf, hgs);
        buf.level--;
        buf.put('}');
        buf.writenl();
        buf.put("finally");
        buf.writenl();
        if (s.finalbody.isScopeStatement())
        {
            s.finalbody.statementToBuffer(buf, hgs);
        }
        else
        {
            buf.level++;
            s.finalbody.statementToBuffer(buf, hgs);
            buf.level--;
        }
    }

    void visitScopeGuard(ScopeGuardStatement s)
    {
        buf.put(Token.toString(s.tok));
        buf.put(' ');
        if (s.statement)
            s.statement.statementToBuffer(buf, hgs);
    }

    void visitThrow(ThrowStatement s)
    {
        buf.put("throw ");
        s.exp.expressionToBuffer(buf, hgs);
        buf.put(';');
        buf.writenl();
    }

    void visitDebug(DebugStatement s)
    {
        if (s.statement)
        {
            s.statement.statementToBuffer(buf, hgs);
        }
    }

    void visitGoto(GotoStatement s)
    {
        buf.put("goto ");
        buf.put(s.ident.toString());
        buf.put(';');
        buf.writenl();
    }

    void visitLabel(LabelStatement s)
    {
        buf.put(s.ident.toString());
        buf.put(':');
        buf.writenl();
        if (s.statement)
            s.statement.statementToBuffer(buf, hgs);
    }

    void visitAsm(AsmStatement s)
    {
        buf.put("asm { ");
        Token* t = s.tokens;
        buf.level++;
        while (t)
        {
            buf.put(t.toString());
            if (t.next &&
                t.value != TOK.min      &&
                t.value != TOK.comma    && t.next.value != TOK.comma    &&
                t.value != TOK.leftBracket && t.next.value != TOK.leftBracket &&
                                          t.next.value != TOK.rightBracket &&
                t.value != TOK.leftParenthesis   && t.next.value != TOK.leftParenthesis   &&
                                          t.next.value != TOK.rightParenthesis   &&
                t.value != TOK.dot      && t.next.value != TOK.dot)
            {
                buf.put(' ');
            }
            t = t.next;
        }
        buf.level--;
        buf.put("; }");
        buf.writenl();
    }

    void visitInlineAsm(InlineAsmStatement s)
    {
        visitAsm(s);
    }

    void visitGccAsm(GccAsmStatement s)
    {
        visitAsm(s);
    }

    void visitImport(ImportStatement s)
    {
        foreach (imp; *s.imports)
        {
            imp.dsymbolToBuffer(buf, hgs);
        }
    }

    mixin VisitStatement!void visit;
    visit.VisitStatement(s);
}

private void dsymbolToBuffer(Dsymbol s, ref OutBuffer buf, ref HdrGenState hgs)
{
    toCBuffer(s, buf, hgs);
}

void toCBuffer(Dsymbol s, ref OutBuffer buf, ref HdrGenState hgs)
{
    void visitDsymbol(Dsymbol s)
    {
        buf.put(s.toChars());
    }

    void visitStaticAssert(StaticAssert s)
    {
        buf.put(s.kind());
        buf.put('(');
        s.exp.expressionToBuffer(buf, hgs);
        if (s.msgs)
        {
            foreach (m; (*s.msgs)[])
            {
                buf.put(", ");
                m.expressionToBuffer(buf, hgs);
            }
        }
        buf.put(");");
        buf.writenl();
    }

    void visitDebugSymbol(DebugSymbol s)
    {
        buf.put("debug = ");
        buf.put(s.ident.toString());
        buf.put(';');
        buf.writenl();
    }

    void visitVersionSymbol(VersionSymbol s)
    {
        buf.put("version = ");
        buf.put(s.ident.toString());
        buf.put(';');
        buf.writenl();
    }

    void visitEnumMember(EnumMember em)
    {
        if (em.type)
            typeToBuffer(em.type, em.ident, buf, hgs);
        else
            buf.put(em.ident.toString());
        if (em.value)
        {
            buf.put(" = ");
            em.value.expressionToBuffer(buf, hgs);
        }
    }

    void visitImport(Import imp)
    {
        if (hgs.hdrgen && imp.id == Id.object)
            return; // object is imported by default
        if (imp.isstatic)
            buf.put("static ");
        buf.put("import ");
        if (imp.aliasId)
        {
            buf.printf("%s = ", imp.aliasId.toChars());
        }
        foreach (const pid; imp.packages)
        {
            buf.write(pid.toString());
            buf.put('.');
        }
        buf.put(imp.id.toString());
        if (imp.names.length)
        {
            buf.put(" : ");
            foreach (const i, const name; imp.names)
            {
                if (i)
                    buf.put(", ");
                const _alias = imp.aliases[i];
                if (_alias)
                    buf.printf("%s = %s", _alias.toChars(), name.toChars());
                else
                    buf.put(name.toChars());
            }
        }
        buf.put(';');
        buf.writenl();
    }

    void visitAliasThis(AliasThis d)
    {
        buf.put("alias ");
        buf.put(d.ident.toString());
        buf.put(" this;\n");
    }

    void visitAttribDeclaration(AttribDeclaration d)
    {
        bool hasSTC;
        if (auto stcd = d.isStorageClassDeclaration)
        {
            hasSTC = stcToBuffer(buf, stcd.stc);
        }

        if (!d.decl)
        {
            buf.put(';');
            buf.writenl();
            return;
        }
        if (d.decl.length == 0 || (hgs.hdrgen && d.decl.length == 1 && (*d.decl)[0].isUnitTestDeclaration()))
        {
            // hack for https://issues.dlang.org/show_bug.cgi?id=8081
            if (hasSTC) buf.put(' ');
            buf.put("{}");
        }
        else if (d.decl.length == 1)
        {
            if (hasSTC) buf.put(' ');
            toCBuffer((*d.decl)[0], buf, hgs);
            return;
        }
        else
        {
            buf.writenl();
            buf.put('{');
            buf.writenl();
            buf.level++;
            foreach (de; *d.decl)
                toCBuffer(de, buf, hgs);
            buf.level--;
            buf.put('}');
        }
        buf.writenl();
    }

    void visitStorageClassDeclaration(StorageClassDeclaration d)
    {
        visitAttribDeclaration(d);
    }

    void visitDeprecatedDeclaration(DeprecatedDeclaration d)
    {
        buf.put("deprecated(");
        d.msg.expressionToBuffer(buf, hgs);
        buf.put(") ");
        visitAttribDeclaration(d);
    }

    void visitLinkDeclaration(LinkDeclaration d)
    {
        buf.put("extern (");
        buf.put(linkageToString(d.linkage));
        buf.put(") ");
        visitAttribDeclaration(d);
    }

    void visitCPPMangleDeclaration(CPPMangleDeclaration d)
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
        buf.put("extern (C++, ");
        buf.put(s);
        buf.put(") ");
        visitAttribDeclaration(d);
    }

    void visitVisibilityDeclaration(VisibilityDeclaration d)
    {
        visibilityToBuffer(buf, d.visibility);
        AttribDeclaration ad = cast(AttribDeclaration)d;
        if (ad.decl.length <= 1)
            buf.put(' ');
        if (ad.decl.length == 1 && (*ad.decl)[0].isVisibilityDeclaration)
            visitAttribDeclaration((*ad.decl)[0].isVisibilityDeclaration);
        else
            visitAttribDeclaration(d);
    }

    void visitAlignDeclaration(AlignDeclaration d)
    {
        if (d.exps)
        {
            foreach (i, exp; (*d.exps)[])
            {
                if (i)
                    buf.put(' ');
                buf.put("align (");
                toCBuffer(exp, buf, hgs);
                buf.put(')');
            }
            if (d.decl && d.decl.length < 2)
                buf.put(' ');
        }
        else
            buf.put("align ");

        visitAttribDeclaration(d.isAttribDeclaration());
    }

    void visitAnonDeclaration(AnonDeclaration d)
    {
        buf.put(d.isunion ? "union" : "struct");
        buf.writenl();
        buf.put("{");
        buf.writenl();
        buf.level++;
        if (d.decl)
        {
            foreach (de; *d.decl)
                toCBuffer(de, buf, hgs);
        }
        buf.level--;
        buf.put("}");
        buf.writenl();
    }

    void visitPragmaDeclaration(PragmaDeclaration d)
    {
        buf.put("pragma (");
        buf.put(d.ident.toString());
        if (d.args && d.args.length)
        {
            buf.put(", ");
            argsToBuffer(d.args, buf, hgs);
        }

        buf.put(')');

        // https://issues.dlang.org/show_bug.cgi?id=14690
        // Unconditionally perform a full output dump
        // for `pragma(inline)` declarations.
        const saved = hgs.doFuncBodies;
        if (d.ident == Id.Pinline)
            hgs.doFuncBodies = true;

        visitAttribDeclaration(d);
        hgs.doFuncBodies = saved;
    }

    void visitConditionalDeclaration(ConditionalDeclaration d)
    {
        d.condition.conditionToBuffer(buf, hgs);
        if (d.decl || d.elsedecl)
        {
            buf.writenl();
            buf.put('{');
            buf.writenl();
            buf.level++;
            if (d.decl)
            {
                foreach (de; *d.decl)
                    toCBuffer(de, buf, hgs);
            }
            buf.level--;
            buf.put('}');
            if (d.elsedecl)
            {
                buf.writenl();
                buf.put("else");
                buf.writenl();
                buf.put('{');
                buf.writenl();
                buf.level++;
                foreach (de; *d.elsedecl)
                    toCBuffer(de, buf, hgs);
                buf.level--;
                buf.put('}');
            }
        }
        else
            buf.put(':');
        buf.writenl();
    }

    void visitStaticForeachDeclaration(StaticForeachDeclaration s)
    {
        void foreachWithoutBody(ForeachStatement s)
        {
            buf.put(Token.toString(s.op));
            buf.put(" (");
            foreach (i, p; *s.parameters)
            {
                if (i)
                    buf.put(", ");
                if (stcToBuffer(buf, p.storageClass))
                    buf.put(' ');
                if (p.type)
                    typeToBuffer(p.type, p.ident, buf, hgs);
                else
                    buf.put(p.ident.toString());
            }
            buf.put("; ");
            s.aggr.expressionToBuffer(buf, hgs);
            buf.put(')');
            buf.writenl();
        }

        void foreachRangeWithoutBody(ForeachRangeStatement s)
        {
            /* s.op ( param ; lwr .. upr )
             */
            buf.put(Token.toString(s.op));
            buf.put(" (");
            if (s.param.type)
                typeToBuffer(s.param.type, s.param.ident, buf, hgs);
            else
                buf.put(s.param.ident.toString());
            buf.put("; ");
            s.lwr.expressionToBuffer(buf, hgs);
            buf.put(" .. ");
            s.upr.expressionToBuffer(buf, hgs);
            buf.put(')');
            buf.writenl();
        }

        buf.put("static ");
        if (s.sfe.aggrfe)
        {
            foreachWithoutBody(s.sfe.aggrfe);
        }
        else
        {
            assert(s.sfe.rangefe);
            foreachRangeWithoutBody(s.sfe.rangefe);
        }
        buf.put('{');
        buf.writenl();
        buf.level++;
        visitAttribDeclaration(s);
        buf.level--;
        buf.put('}');
        buf.writenl();

    }

    void visitMixinDeclaration(MixinDeclaration d)
    {
        buf.put("mixin(");
        argsToBuffer(d.exps, buf, hgs, null);
        buf.put(");");
        buf.writenl();
    }

    void visitUserAttributeDeclaration(UserAttributeDeclaration d)
    {
        buf.put("@(");
        argsToBuffer(d.atts, buf, hgs);
        buf.put(')');
        visitAttribDeclaration(d);
    }

    void visitTemplateConstraint(Expression constraint)
    {
        if (!constraint)
            return;
        buf.put(" if (");
        constraint.expressionToBuffer(buf, hgs);
        buf.put(')');
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
                buf.put("in");
                if (auto es = frequire.isExpStatement())
                {
                    assert(es.exp && es.exp.op == EXP.assert_);
                    buf.put(" (");
                    (cast(AssertExp)es.exp).e1.expressionToBuffer(buf, hgs);
                    buf.put(')');
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
                buf.put("out");
                if (auto es = fensure.ensure.isExpStatement())
                {
                    assert(es.exp && es.exp.op == EXP.assert_);
                    buf.put(" (");
                    if (fensure.id)
                    {
                        buf.put(fensure.id.toString());
                    }
                    buf.put("; ");
                    (cast(AssertExp)es.exp).e1.expressionToBuffer(buf, hgs);
                    buf.put(')');
                    buf.writenl();
                    requireDo = false;
                }
                else
                {
                    if (fensure.id)
                    {
                        buf.put('(');
                        buf.put(fensure.id.toString());
                        buf.put(')');
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
        if (!f.fbody || (hgs.hdrgen && hgs.doFuncBodies == false && !hgs.autoMember && !hgs.tpltMember && !hgs.insideFuncBody))
        {
            if (!f.fbody && (f.fensures || f.frequires))
            {
                buf.writenl();
                contractsToBuffer(f);
            }
            buf.put(';');
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
            buf.put("do");
            buf.writenl();
        }
        buf.put('{');
        buf.writenl();
        buf.level++;
        f.fbody.statementToBuffer(buf, hgs);
        buf.level--;
        buf.put('}');
        buf.writenl();
        hgs.tpltMember = savetlpt;
        hgs.autoMember = saveauto;
    }

    void visitBaseClasses(ClassDeclaration d)
    {
        if (!d || !d.baseclasses.length)
            return;
        if (!d.isAnonymous())
            buf.put(" : ");
        foreach (i, b; *d.baseclasses)
        {
            if (i)
                buf.put(", ");
            typeToBuffer(b.type, null, buf, hgs);
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
                buf.put(' ');
            functionToBufferFull(cast(TypeFunction)fd.type, buf, d.ident, hgs, d);
            visitTemplateConstraint(d.constraint);
            hgs.tpltMember++;
            bodyToBuffer(fd);
            hgs.tpltMember--;
            return true;
        }
        if (AggregateDeclaration ad = onemember.isAggregateDeclaration())
        {
            buf.put(ad.kind());
            buf.put(' ');
            buf.put(ad.ident.toString());
            buf.put('(');
            visitTemplateParameters(hgs.ddoc ? d.origParameters : d.parameters, buf, hgs);
            buf.put(')');
            visitTemplateConstraint(d.constraint);
            visitBaseClasses(ad.isClassDeclaration());
            hgs.tpltMember++;
            if (ad.members)
            {
                buf.writenl();
                buf.put('{');
                buf.writenl();
                buf.level++;
                foreach (s; *ad.members)
                    toCBuffer(s, buf, hgs);
                buf.level--;
                buf.put('}');
            }
            else
                buf.put(';');
            buf.writenl();
            hgs.tpltMember--;
            return true;
        }
        if (VarDeclaration vd = onemember.isVarDeclaration())
        {
            if (d.constraint)
                return false;
            if (stcToBuffer(buf, vd.storage_class))
                buf.put(' ');
            if (vd.type)
                typeToBuffer(vd.type, vd.ident, buf, hgs);
            else
                buf.put(vd.ident.toString());
            buf.put('(');
            visitTemplateParameters(hgs.ddoc ? d.origParameters : d.parameters, buf, hgs);
            buf.put(')');
            if (vd._init)
            {
                buf.put(" = ");
                ExpInitializer ie = vd._init.isExpInitializer();
                if (ie && (ie.exp.op == EXP.construct || ie.exp.op == EXP.blit))
                    (cast(AssignExp)ie.exp).e2.expressionToBuffer(buf, hgs);
                else
                    vd._init.initializerToBuffer(buf, hgs);
            }
            buf.put(';');
            buf.writenl();
            return true;
        }
        return false;
    }

    void visitTemplateDeclaration(TemplateDeclaration d)
    {
        version (none)
        {
            // Should handle template functions for doc generation
            if (onemember && onemember.isFuncDeclaration())
                buf.put("foo ");
        }
        if ((hgs.hdrgen || hgs.fullDump) && visitEponymousMember(d))
            return;
        if (hgs.ddoc)
            buf.put(d.kind());
        else
            buf.put("template");
        buf.put(' ');
        buf.put(d.ident.toString());
        buf.put('(');
        visitTemplateParameters(hgs.ddoc ? d.origParameters : d.parameters, buf, hgs);
        buf.put(')');
        visitTemplateConstraint(d.constraint);
        if (hgs.hdrgen || hgs.fullDump)
        {
            hgs.tpltMember++;
            buf.writenl();
            buf.put('{');
            buf.writenl();
            buf.level++;
            foreach (s; *d.members)
                toCBuffer(s, buf, hgs);
            buf.level--;
            buf.put('}');
            buf.writenl();
            hgs.tpltMember--;
        }
    }

    void visitTemplateInstance(TemplateInstance ti)
    {
        buf.put(ti.name.toChars());
        tiargsToBuffer(ti, buf, hgs);

        if (hgs.fullDump)
        {
            buf.writenl();
            dumpTemplateInstance(ti, buf, hgs);
        }
    }

    void visitTemplateMixin(TemplateMixin tm)
    {
        buf.put("mixin ");
        typeToBuffer(tm.tqual, null, buf, hgs);
        tiargsToBuffer(tm, buf, hgs);
        if (tm.ident && memcmp(tm.ident.toString().ptr, cast(const(char)*) "__mixin", 7) != 0)
        {
            buf.put(' ');
            buf.put(tm.ident.toString());
        }
        buf.put(';');
        buf.writenl();
        if (hgs.fullDump)
            dumpTemplateInstance(tm, buf, hgs);
    }

    void visitEnumDeclaration(EnumDeclaration d)
    {
        auto oldInEnumDecl = hgs.inEnumDecl;
        scope(exit) hgs.inEnumDecl = oldInEnumDecl;
        hgs.inEnumDecl = d;
        buf.put("enum ");
        if (d.ident)
        {
            buf.put(d.ident.toString());
        }
        if (d.memtype)
        {
            buf.put(" : ");
            typeToBuffer(d.memtype, null, buf, hgs);
        }
        if (!d.members)
        {
            buf.put(';');
            buf.writenl();
            return;
        }
        buf.writenl();
        buf.put('{');
        buf.writenl();
        buf.level++;
        foreach (em; *d.members)
        {
            if (!em)
                continue;
            toCBuffer(em, buf, hgs);
            buf.put(',');
            buf.writenl();
        }
        buf.level--;
        buf.put('}');
        buf.writenl();

        if (!hgs.importcHdr || !d.ident)
            return;

        /* C enums get their members inserted into the symbol table of the enum declaration.
         * This is accomplished in addEnumMembersToSymtab().
         * But when generating D code from ImportC code, D rulez are followed.
         * Accomplish this by generating an alias declaration for each member
         */
        foreach (em; *d.members)
        {
            if (!em)
                continue;
            buf.put("alias ");
            buf.put(em.ident.toString);
            buf.put(" = ");
            buf.put(d.ident.toString);
            buf.put('.');
            buf.put(em.ident.toString);
            buf.put(';');
            buf.writenl();
        }
    }

    void visitNspace(Nspace d)
    {
        buf.put("extern (C++, ");
        buf.put(d.ident.toString());
        buf.put(')');
        buf.writenl();
        buf.put('{');
        buf.writenl();
        buf.level++;
        foreach (s; *d.members)
            toCBuffer(s, buf, hgs);
        buf.level--;
        buf.put('}');
        buf.writenl();
    }

    void visitStructDeclaration(StructDeclaration d)
    {
        //printf("visitStructDeclaration() %s\n", d.ident.toChars());
        buf.put(d.kind());
        buf.put(' ');
        if (!d.isAnonymous())
            buf.put(d.toChars());
        if (!d.members)
        {
            buf.put(';');
            buf.writenl();
            return;
        }
        buf.writenl();
        buf.put('{');
        buf.writenl();
        buf.level++;
        hgs.insideAggregate++;
        foreach (s; *d.members)
            toCBuffer(s, buf, hgs);
        hgs.insideAggregate--;
        buf.level--;
        buf.put('}');
        buf.writenl();
    }

    void visitClassDeclaration(ClassDeclaration d)
    {
        if (!d.isAnonymous())
        {
            buf.put(d.kind());
            buf.put(' ');
            buf.put(d.ident.toString());
        }
        visitBaseClasses(d);
        if (d.members)
        {
            buf.writenl();
            buf.put('{');
            buf.writenl();
            buf.level++;
            hgs.insideAggregate++;
            foreach (s; *d.members)
                toCBuffer(s, buf, hgs);
            hgs.insideAggregate--;
            buf.level--;
            buf.put('}');
        }
        else
            buf.put(';');
        buf.writenl();
    }

    void visitAliasDeclaration(AliasDeclaration d)
    {
        if (d.storage_class & STC.local)
            return;
        if (d.hidden)
            return;
        buf.put("alias ");
        if (d.aliassym)
        {
            buf.put(d.ident.toString());
            buf.put(" = ");
            if (stcToBuffer(buf, d.storage_class))
                buf.put(' ');
            /*
                https://issues.dlang.org/show_bug.cgi?id=23223
                https://issues.dlang.org/show_bug.cgi?id=23222
                This special case (initially just for modules) avoids some segfaults
                and nicer -vcg-ast output.
            */
            if (d.aliassym.isModule())
            {
                buf.put(d.aliassym.ident.toString());
            }
            else
            {
                toCBuffer(d.aliassym, buf, hgs);
            }
        }
        else if (d.type.ty == Tfunction)
        {
            if (stcToBuffer(buf, d.storage_class))
                buf.put(' ');
            typeToBuffer(d.type, d.ident, buf, hgs);
        }
        else if (d.ident)
        {
            hgs.declstring = (d.ident == Id.string || d.ident == Id.wstring || d.ident == Id.dstring);
            buf.put(d.ident.toString());
            buf.put(" = ");
            if (stcToBuffer(buf, d.storage_class))
                buf.put(' ');
            hgs.inCAlias = hgs.importcHdr;
            typeToBuffer(d.type, null, buf, hgs);
            hgs.inCAlias = false;
            hgs.declstring = false;
        }
        buf.put(';');
        buf.writenl();
    }

    void visitAliasAssign(AliasAssign d)
    {
        buf.put(d.ident.toString());
        buf.put(" = ");
        if (d.aliassym)
            toCBuffer(d.aliassym, buf, hgs);
        else // d.type
            typeToBuffer(d.type, null, buf, hgs);
        buf.put(';');
        buf.writenl();
    }

    void visitVarDeclaration(VarDeclaration d)
    {
        if (d.storage_class & STC.local)
            return;
        visitVarDecl(d, false, buf, hgs);
        buf.put(';');
        buf.writenl();
    }

    void visitFuncDeclaration(FuncDeclaration f)
    {
        //printf("FuncDeclaration::toCBuffer() '%s'\n", f.toChars());

        // https://issues.dlang.org/show_bug.cgi?id=24891
        // return/scope storage classes are printed as part of function type
        if (stcToBuffer(buf, f.storage_class & ~(STC.scope_ | STC.return_ | STC.returnScope | STC.returnRef)))
            buf.put(' ');
        typeToBuffer(f.type, f.ident, buf, hgs);
        auto tf = f.type.isTypeFunction();

        if (hgs.hdrgen && tf)
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
            else if (hgs.tpltMember == 0 && hgs.doFuncBodies == false && !hgs.insideFuncBody)
            {
                if (!f.fbody)
                {
                    // this can happen on interfaces / abstract functions, see `allowsContractWithoutBody`
                    if (f.fensures || f.frequires)
                        buf.writenl();
                    contractsToBuffer(f);
                }
                buf.put(';');
                buf.writenl();
            }
            else
                bodyToBuffer(f);
        }
        else
            bodyToBuffer(f);
    }

    void visitFuncLiteralDeclaration(FuncLiteralDeclaration f)
    {
        if (f.type.ty == Terror)
        {
            buf.put("__error");
            return;
        }
        if (f.tok != TOK.reserved && !hgs.errorMsg)
        {
            buf.put(f.kind());
            buf.put(' ');
        }
        TypeFunction tf = cast(TypeFunction)f.type;

        if (!f.inferRetType && tf.next)
            typeToBuffer(tf.next, null, buf, hgs);
        parametersToBuffer(tf.parameterList, buf, hgs);

        // https://issues.dlang.org/show_bug.cgi?id=20074
        void printAttribute(string str)
        {
            buf.put(' ');
            buf.put(str);
        }

        if (!hgs.errorMsg)
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
            buf.put(" => ");
            rs.exp.expressionToBuffer(buf, hgs);
        }
        else
        {
            hgs.tpltMember++;
            bodyToBuffer(f);
            hgs.tpltMember--;
        }
    }

    void visitPostBlitDeclaration(PostBlitDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class))
            buf.put(' ');
        buf.put("this(this)");
        bodyToBuffer(d);
    }

    void visitDtorDeclaration(DtorDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class))
            buf.put(' ');
        buf.put("~this()");
        bodyToBuffer(d);
    }

    void visitStaticCtorDeclaration(StaticCtorDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class & ~STC.static_))
            buf.put(' ');
        if (d.isSharedStaticCtorDeclaration())
            buf.put("shared ");
        buf.put("static this()");
        if (hgs.hdrgen && !hgs.tpltMember)
        {
            buf.put(';');
            buf.writenl();
        }
        else
            bodyToBuffer(d);
    }

    void visitStaticDtorDeclaration(StaticDtorDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class & ~STC.static_))
            buf.put(' ');
        if (d.isSharedStaticDtorDeclaration())
            buf.put("shared ");
        buf.put("static ~this()");
        if (hgs.hdrgen && !hgs.tpltMember)
        {
            buf.put(';');
            buf.writenl();
        }
        else
            bodyToBuffer(d);
    }

    void visitInvariantDeclaration(InvariantDeclaration d)
    {
        if (hgs.hdrgen)
            return;
        if (stcToBuffer(buf, d.storage_class))
            buf.put(' ');
        buf.put("invariant");
        auto es = d.fbody.isExpStatement();
        if (es && es.exp && es.exp.op == EXP.assert_)
        {
            buf.put(" (");
            (cast(AssertExp)es.exp).e1.expressionToBuffer(buf, hgs);
            buf.put(");");
            buf.writenl();
        }
        else
        {
            bodyToBuffer(d);
        }
    }

    void visitUnitTestDeclaration(UnitTestDeclaration d)
    {
        if (hgs.hdrgen)
            return;
        if (stcToBuffer(buf, d.storage_class))
            buf.put(' ');
        buf.put("unittest");
        bodyToBuffer(d);
    }

    void visitBitFieldDeclaration(BitFieldDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class))
            buf.put(' ');
        Identifier id = d.isAnonymous() ? null : d.ident;
        typeToBuffer(d.type, id, buf, hgs);
        buf.put(" : ");
        d.width.expressionToBuffer(buf, hgs);
        buf.put(';');
        buf.writenl();
    }

    void visitNewDeclaration(NewDeclaration d)
    {
        if (stcToBuffer(buf, d.storage_class & ~STC.static_))
            buf.put(' ');
        buf.put("new();");
    }

    void visitModule(Module m)
    {
        moduleToBuffer2(m, buf, hgs);
    }

    extern (C++)
    final class DsymbolPrettyPrintVisitor : Visitor
    {
        alias visit = Visitor.visit;

      public:
      override:
        void visit(Dsymbol s)                  { visitDsymbol(s); }
        void visit(StaticAssert s)             { visitStaticAssert(s); }
        void visit(DebugSymbol s)              { visitDebugSymbol(s); }
        void visit(VersionSymbol s)            { visitVersionSymbol(s); }
        void visit(EnumMember em)              { visitEnumMember(em); }
        void visit(Import imp)                 { visitImport(imp); }
        void visit(AliasThis d)                { visitAliasThis(d); }
        void visit(AttribDeclaration d)        { visitAttribDeclaration(d); }
        void visit(StorageClassDeclaration d)  { visitStorageClassDeclaration(d); }
        void visit(DeprecatedDeclaration d)    { visitDeprecatedDeclaration(d); }
        void visit(LinkDeclaration d)          { visitLinkDeclaration(d); }
        void visit(CPPMangleDeclaration d)     { visitCPPMangleDeclaration(d); }
        void visit(VisibilityDeclaration d)    { visitVisibilityDeclaration(d); }
        void visit(AlignDeclaration d)         { visitAlignDeclaration(d); }
        void visit(AnonDeclaration d)          { visitAnonDeclaration(d); }
        void visit(PragmaDeclaration d)        { visitPragmaDeclaration(d); }
        void visit(ConditionalDeclaration d)   { visitConditionalDeclaration(d); }
        void visit(StaticForeachDeclaration s) { visitStaticForeachDeclaration(s); }
        void visit(MixinDeclaration d)         { visitMixinDeclaration(d); }
        void visit(UserAttributeDeclaration d) { visitUserAttributeDeclaration(d); }
        void visit(TemplateDeclaration d)      { visitTemplateDeclaration(d); }
        void visit(TemplateInstance ti)        { visitTemplateInstance(ti); }
        void visit(TemplateMixin tm)           { visitTemplateMixin(tm); }
        void visit(EnumDeclaration d)          { visitEnumDeclaration(d); }
        void visit(Nspace d)                   { visitNspace(d); }
        void visit(StructDeclaration d)        { visitStructDeclaration(d); }
        void visit(ClassDeclaration d)         { visitClassDeclaration(d); }
        void visit(AliasDeclaration d)         { visitAliasDeclaration(d); }
        void visit(AliasAssign d)              { visitAliasAssign(d); }
        void visit(VarDeclaration d)           { visitVarDeclaration(d); }
        void visit(FuncDeclaration f)          { visitFuncDeclaration(f); }
        void visit(FuncLiteralDeclaration f)   { visitFuncLiteralDeclaration(f); }
        void visit(PostBlitDeclaration d)      { visitPostBlitDeclaration(d); }
        void visit(DtorDeclaration d)          { visitDtorDeclaration(d); }
        void visit(StaticCtorDeclaration d)    { visitStaticCtorDeclaration(d); }
        void visit(StaticDtorDeclaration d)    { visitStaticDtorDeclaration(d); }
        void visit(InvariantDeclaration d)     { visitInvariantDeclaration(d); }
        void visit(UnitTestDeclaration d)      { visitUnitTestDeclaration(d); }
        void visit(BitFieldDeclaration d)      { visitBitFieldDeclaration(d); }
        void visit(NewDeclaration d)           { visitNewDeclaration(d); }
        void visit(Module m)                   { visitModule(m); }
    }

    scope v = new DsymbolPrettyPrintVisitor();
    s.accept(v);
}

// Note: this function is not actually `const`, because iterating the
// function parameter list may run dsymbolsemantic on enum types
public
void toCharsMaybeConstraints(const TemplateDeclaration td, ref OutBuffer buf, ref HdrGenState hgs)
{
    buf.put(td.ident == Id.ctor ? "this" : td.ident.toString());
    buf.put('(');
    foreach (i, const tp; *td.parameters)
    {
        if (i)
            buf.put(", ");
        toCBuffer(tp, buf, hgs);
    }
    buf.put(')');

    if (hgs.showOneMember && td.onemember)
    {
        if (const fd = td.onemember.isFuncDeclaration())
        {
            if (TypeFunction tf = cast(TypeFunction)fd.type.isTypeFunction())
            {
                // !! Casted away const
                buf.put(parametersTypeToChars(tf.parameterList));
                if (tf.mod)
                {
                    buf.put(' ');
                    buf.MODtoBuffer(tf.mod);
                }
            }
        }
    }

    if (!hgs.skipConstraints &&
        td.constraint)
    {
        buf.put(" if (");
        toCBuffer(td.constraint, buf, hgs);
        buf.put(')');
    }
}


/*****************************************
 * Pretty-print a template parameter list to a buffer.
 */
private void visitTemplateParameters(TemplateParameters* parameters, ref OutBuffer buf, ref HdrGenState hgs)
{
    if (!parameters)
        return;
    foreach (i, p; *parameters)
    {
        if (i)
            buf.put(", ");
        toCBuffer(p, buf, hgs);
    }
}


/*******************************************
 * Pretty-print a VarDeclaration to buf.
 */
private void visitVarDecl(VarDeclaration v, bool anywritten, ref OutBuffer buf, ref HdrGenState hgs)
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

    const commentIt = hgs.importcHdr && isSpecialCName(v.ident);
    if (commentIt)
        buf.put("/+");

    if (anywritten)
    {
        buf.put(", ");
        buf.put(v.ident.toString());
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
            buf.put(' ');
        if (v.type)
            typeToBuffer(v.type, v.ident, buf, hgs);
        else if (useTypeof)
        {
            buf.put("typeof(");
            vinit(v);
            buf.put(") ");
            buf.put(v.ident.toString());
        }
        else
            buf.put(v.ident.toString());
    }
    if (v._init && !isextern)
    {
        buf.put(" = ");
        vinit(v);
    }
    if (commentIt)
        buf.put("+/");
}

/*************************************
 * The names __DATE__, __TIME__,__EOF__, __VENDOR__, __TIMESTAMP__, __VERSION__
 * are special to the D lexer and cannot be used as D source variable names.
 * Params:
 *      id = name to check
 * Returns:
 *      true if special C name
 */
private bool isSpecialCName(Identifier id)
{
    auto s = id.toString();
    if (s.length >= 7 && s[0] == '_' && s[1] == '_' &&
        (id == Id.DATE ||
         id == Id.TIME ||
         id == Id.EOFX ||
         id == Id.VENDOR ||
         id == Id.TIMESTAMP ||
         id == Id.VERSIONX))
        return true;
    return false;
}

/*********************************************
 * Print expression to buffer.
 */
private void expressionPrettyPrint(Expression e, ref OutBuffer buf, ref HdrGenState hgs)
{
    void visit(Expression e)
    {
        buf.put(EXPtoString(e.op));
    }

    void visitInteger(IntegerExp e)
    {
        const ulong v = e.toInteger();
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
                                const id = em.ident.toString();
                                buf.printf("%s.%.*s", sym.toChars(), cast(int)id.length, id.ptr);
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
                    writeSingleCharLiteral(buf, cast(dchar) v);
                    if (hgs.ddoc)
                        escapeDdocString(buf, o);
                    break;
                }
            case Tint8:
                buf.put("cast(byte)");
                goto L2;
            case Tint16:
                buf.put("cast(short)");
                goto L2;
            case Tint32:
            L2:
                buf.printf("%d", cast(int)v);
                break;
            case Tuns8:
                buf.put("cast(ubyte)");
                goto case Tuns32;
            case Tuns16:
                buf.put("cast(ushort)");
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
                    buf.put("cast(long)-9223372036854775808");
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
                buf.put(v ? "true" : "false");
                break;
            case Tpointer:
                buf.put("cast(");

                HdrGenState hgs2;               // should re-examine need for new hgs
                hgs2.fullQual = (t.ty == Tclass && !t.mod);
                toCBuffer(t, buf, null, hgs2);

                buf.put(")cast(size_t)");
                goto case Tuns64;

            case Tvoid:
                buf.put("cast(void)0");
                break;

            default:
                /* This can happen if errors, such as
                 * the type is painted on like in fromConstInitializer().
                 * Just ignore
                 */
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
        buf.put("__error");
    }

    void visitVoidInit(VoidInitExp e)
    {
        buf.put("void");
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
        buf.put('(');
        floatToBuffer(e.type, creall(e.value));
        buf.put('+');
        floatToBuffer(e.type, cimagl(e.value));
        buf.put("i)");
    }

    void visitIdentifier(IdentifierExp e)
    {
        if (hgs.hdrgen || hgs.ddoc)
            buf.put(e.ident.toHChars2());
        else
            buf.put(e.ident.toString());
    }

    void visitDsymbol(Dsymbol s)
    {
        // For -vcg-ast, print internal names such as __invariant, __ctor etc.
        // This condition is a bit kludge, and can be cleaned up if the
        // mutual dependency `AST.toChars <> hdrgen.d` gets refactored
        if (hgs.vcg_ast && s.ident && !s.isTemplateInstance() && !s.isTemplateDeclaration())
            buf.put(s.ident.toChars());
        else
            buf.put(s.toChars());
    }

    void visitDsymbolExp(DsymbolExp e)
    {
        visitDsymbol(e.s);
    }

    void visitThis(ThisExp e)
    {
        buf.put("this");
    }

    void visitSuper(SuperExp e)
    {
        buf.put("super");
    }

    void visitNull(NullExp e)
    {
        buf.put("null");
    }

    void visitString(StringExp e)
    {
        if (e.hexString || e.sz == 8)
        {
            buf.put('x');
            buf.put('"');
            foreach (i; 0 .. e.len)
                buf.printf("%0*llX", e.sz, e.getIndex(i));
            buf.put('"');
            if (e.postfix)
                buf.put(e.postfix);
            return;
        }
        buf.put('"');
        const o = buf.length;
        foreach (i; 0 .. e.len)
        {
            writeCharLiteral(buf, e.getCodeUnit(i));
        }
        if (hgs.ddoc)
            escapeDdocString(buf, o);
        buf.put('"');
        if (e.postfix)
            buf.put(e.postfix);
    }

    void visitInterpolation(InterpExp e)
    {
        buf.put('i');
        buf.put('"');
        const o = buf.length;

        foreach (idx, str; e.interpolatedSet.parts)
        {
            if (idx % 2 == 0)
            {
                foreach(ch; str)
                    writeCharLiteral(buf, ch);
            }
            else
            {
                buf.put('$');
                buf.put('(');
                foreach(ch; str)
                    buf.put(ch);
                buf.put(')');
            }
        }

        if (hgs.ddoc)
            escapeDdocString(buf, o);
        buf.put('"');
        if (e.postfix)
            buf.put(e.postfix);

    }

    void visitArrayLiteral(ArrayLiteralExp e)
    {
        buf.put('[');
        argsToBuffer(e.elements, buf, hgs, e.basis);
        buf.put(']');
    }

    void visitAssocArrayLiteral(AssocArrayLiteralExp e)
    {
        if (hgs.vcg_ast && e.lowering)
        {
            expToBuffer(e.lowering, PREC.assign, buf, hgs);
            return;
        }
        buf.put('[');
        foreach (i, key; *e.keys)
        {
            if (i)
                buf.put(", ");
            expToBuffer(key, PREC.assign, buf, hgs);
            buf.put(':');
            auto value = (*e.values)[i];
            expToBuffer(value, PREC.assign, buf, hgs);
        }
        buf.put(']');
    }

    void visitStructLiteral(StructLiteralExp e)
    {
        buf.put(e.sd.toChars());
        buf.put('(');
        // CTFE can generate struct literals that contain an AddrExp pointing
        // to themselves, need to avoid infinite recursion:
        // struct S { this(int){ this.s = &this; } S* s; }
        // const foo = new S(0);
        if (e.stageflags & StructLiteralExp.StageFlags.toCBuffer)
            buf.put("<recursion>");
        else
        {
            const old = e.stageflags;
            e.stageflags |= StructLiteralExp.StageFlags.toCBuffer;
            argsToBuffer(e.elements, buf, hgs);
            e.stageflags = old;
        }
        buf.put(')');
    }

    void visitCompoundLiteral(CompoundLiteralExp e)
    {
        buf.put('(');
        typeToBuffer(e.type, null, buf, hgs);
        buf.put(')');
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
        else if (hgs.ddoc)
        {
            // fixes bug 6491
            if (auto m = e.sds.isModule())
                buf.put(m.md.toChars());
            else
                buf.put(e.sds.toChars());
        }
        else
        {
            buf.put(e.sds.kind());
            buf.put(' ');
            buf.put(e.sds.toChars());
        }
    }

    void visitTemplate(TemplateExp e)
    {
        buf.put(e.td.toChars());
    }

    void visitNew(NewExp e)
    {
        if (hgs.vcg_ast && e.lowering)
        {
            expToBuffer(e.lowering, PREC.primary, buf, hgs);
            return;
        }
        if (e.thisexp)
        {
            expToBuffer(e.thisexp, PREC.primary, buf, hgs);
            buf.put('.');
        }
        buf.put("new ");
        if (e.placement)
        {
            buf.put('(');
            expToBuffer(e.placement, PREC.assign, buf, hgs);
            buf.put(')');
            buf.put(' ');
        }
        typeToBuffer(e.newtype, null, buf, hgs);
        if (e.arguments && e.arguments.length)
        {
            buf.put('(');
            argsToBuffer(e.arguments, buf, hgs, null, e.names);
            buf.put(')');
        }
    }

    void visitNewAnonClass(NewAnonClassExp e)
    {
        if (e.thisexp)
        {
            expToBuffer(e.thisexp, PREC.primary, buf, hgs);
            buf.put('.');
        }
        buf.put("new");
        if (e.placement)
        {
            buf.put(' ');
            buf.put('(');
            expToBuffer(e.placement, PREC.assign, buf, hgs);
            buf.put(')');
        }
        buf.put(" class ");
        if (e.arguments && e.arguments.length)
        {
            buf.put('(');
            argsToBuffer(e.arguments, buf, hgs);
            buf.put(')');
        }
        if (e.cd)
            e.cd.dsymbolToBuffer(buf, hgs);
    }

    void visitSymOff(SymOffExp e)
    {
        if (e.offset)
            buf.printf("(& %s + %llu)", e.var.toChars(), e.offset);
        else if (e.var.isTypeInfoDeclaration())
            buf.put(e.var.toChars());
        else
            buf.printf("& %s", e.var.toChars());
    }

    void visitVar(VarExp e)
    {
        visitDsymbol(e.var);
    }

    void visitOver(OverExp e)
    {
        buf.put(e.vars.ident.toString());
    }

    void visitTuple(TupleExp e)
    {
        if (e.e0)
        {
            buf.put('(');
            e.e0.expressionPrettyPrint(buf, hgs);
            buf.put(", AliasSeq!(");
            argsToBuffer(e.exps, buf, hgs);
            buf.put("))");
        }
        else
        {
            buf.put("AliasSeq!(");
            argsToBuffer(e.exps, buf, hgs);
            buf.put(')');
        }
    }

    void visitFunc(FuncExp e)
    {
        e.fd.dsymbolToBuffer(buf, hgs);
        //buf.put(e.fd.toChars());
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
                buf.put('(');

                visitVarDecl(var, false, buf, hgs);

                buf.put(';');
                buf.put(')');
            }
            else e.declaration.dsymbolToBuffer(buf, hgs);
        }
    }

    void visitTypeid(TypeidExp e)
    {
        buf.put("typeid(");
        objectToBuffer(e.obj, buf, hgs);
        buf.put(')');
    }

    void visitTraits(TraitsExp e)
    {
        buf.put("__traits(");
        if (e.ident)
            buf.put(e.ident.toString());
        if (e.args)
        {
            foreach (arg; *e.args)
            {
                buf.put(", ");
                objectToBuffer(arg, buf, hgs);
            }
        }
        buf.put(')');
    }

    void visitHalt(HaltExp e)
    {
        buf.put("halt");
    }

    void visitIs(IsExp e)
    {
        buf.put("is(");
        typeToBuffer(e.targ, e.id, buf, hgs);
        if (e.tok2 != TOK.reserved)
        {
            buf.put(' ');
            buf.put(Token.toString(e.tok));
            buf.put(' ');
            buf.put(Token.toString(e.tok2));
        }
        else if (e.tspec)
        {
            if (e.tok == TOK.colon)
                buf.put(" : ");
            else
                buf.put(" == ");
            typeToBuffer(e.tspec, null, buf, hgs);
        }
        if (e.parameters && e.parameters.length)
        {
            buf.put(", ");
            visitTemplateParameters(e.parameters, buf, hgs);
        }
        buf.put(')');
    }

    void visitUna(UnaExp e)
    {
        buf.put(EXPtoString(e.op));
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitLoweredAssignExp(LoweredAssignExp e)
    {
        if (hgs.vcg_ast)
        {
            expressionToBuffer(e.lowering, buf, hgs);
            return;
        }

        visit(cast(BinExp)e);
    }
    void visitBin(BinExp e)
    {
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
        buf.put(' ');
        buf.put(EXPtoString(e.op));
        buf.put(' ');
        expToBuffer(e.e2, cast(PREC)(precedence[e.op] + 1), buf, hgs);
    }

    void visitComma(CommaExp e)
    {
        if (e.originalExp !is null)
        {
            e.originalExp.expressionPrettyPrint(buf, hgs);
            return;
        }

        // CommaExp is generated by the compiler so it shouldn't
        // appear in error messages or header files.
        // For now, this treats the case where the compiler
        // generates CommaExp for temporaries by calling
        // the `sideeffect.copyToTemp` function.
        auto ve = e.e2.isVarExp();

        // Not a CommaExp introduced for temporaries, or -vcg-ast,
        // print the full comma
        if (!ve || !(ve.var.storage_class & STC.temp) || hgs.vcg_ast)
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
                expToBuffer(commaExtract, expPrecedence(hgs, exp), buf, hgs);
                return;
            }
        }

        // not one of the known cases, go on the old path
        visitBin(cast(BinExp)e);
        return;
    }

    void visitMixin(MixinExp e)
    {
        buf.put("mixin(");
        argsToBuffer(e.exps, buf, hgs, null);
        buf.put(')');
    }

    void visitImport(ImportExp e)
    {
        buf.put("import(");
        expToBuffer(e.e1, PREC.assign, buf, hgs);
        buf.put(')');
    }

    void visitAssert(AssertExp e)
    {
        buf.put("assert(");
        expToBuffer(e.e1, PREC.assign, buf, hgs);
        if (e.msg)
        {
            buf.put(", ");
            expToBuffer(e.msg, PREC.assign, buf, hgs);
        }
        buf.put(')');
    }

    void visitThrow(ThrowExp e)
    {
        buf.put("throw ");
        expToBuffer(e.e1, PREC.unary, buf, hgs);
    }

    void visitDotId(DotIdExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put('.');
        buf.put(e.ident.toString());
    }

    void visitDotTemplate(DotTemplateExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put('.');
        buf.put(e.td.toChars());
    }

    void visitDotVar(DotVarExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put('.');
        visitDsymbol(e.var);
    }

    void visitDotTemplateInstance(DotTemplateInstanceExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put('.');
        e.ti.dsymbolToBuffer(buf, hgs);
    }

    void visitDelegate(DelegateExp e)
    {
        buf.put('&');
        if (!e.func.isNested() || e.func.needThis())
        {
            expToBuffer(e.e1, PREC.primary, buf, hgs);
            buf.put('.');
        }
        buf.put(e.func.toChars());
    }

    void visitDotType(DotTypeExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put('.');
        buf.put(e.sym.toChars());
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
        {
            if (!hgs.vcg_ast && e.loweredFrom)
            {
                // restore original syntax for expressions lowered to calls
                expressionToBuffer(e.loweredFrom, buf, hgs);
                return;
            }
            expToBuffer(e.e1, precedence[e.op], buf, hgs);
        }
        buf.put('(');
        argsToBuffer(e.arguments, buf, hgs, null, e.names);
        buf.put(')');
    }

    void visitNot(NotExp e)
    {
        if (!hgs.vcg_ast && e.loweredFrom)
            return expressionToBuffer(e.loweredFrom, buf, hgs);
        return visitUna(e);
    }

    void visitPtr(PtrExp e)
    {
        buf.put('*');
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitDelete(DeleteExp e)
    {
        buf.put("delete ");
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitCast(CastExp e)
    {
        buf.put("cast(");
        if (e.to)
            typeToBuffer(e.to, null, buf, hgs);
        else
        {
            MODtoBuffer(buf, e.mod);
        }
        buf.put(')');
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitVector(VectorExp e)
    {
        buf.put("cast(");
        typeToBuffer(e.to, null, buf, hgs);
        buf.put(')');
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitVectorArray(VectorArrayExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put(".array");
    }

    void visitSlice(SliceExp e)
    {
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
        buf.put('[');
        if (e.upr || e.lwr)
        {
            if (e.lwr)
                sizeToBuffer(e.lwr, buf, hgs);
            else
                buf.put('0');
            buf.put("..");
            if (e.upr)
                sizeToBuffer(e.upr, buf, hgs);
            else
                buf.put('$');
        }
        buf.put(']');
    }

    void visitArrayLength(ArrayLengthExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put(".length");
    }

    void visitInterval(IntervalExp e)
    {
        expToBuffer(e.lwr, PREC.assign, buf, hgs);
        buf.put("..");
        expToBuffer(e.upr, PREC.assign, buf, hgs);
    }

    void visitDelegatePtr(DelegatePtrExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put(".ptr");
    }

    void visitDelegateFuncptr(DelegateFuncptrExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put(".funcptr");
    }

    void visitArray(ArrayExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put('[');
        argsToBuffer(e.arguments, buf, hgs);
        buf.put(']');
    }

    void visitDot(DotExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put('.');
        expToBuffer(e.e2, PREC.primary, buf, hgs);
    }

    void visitCat(CatExp e)
    {
        if (hgs.vcg_ast && e.lowering)
            expressionToBuffer(e.lowering, buf, hgs);
        else
            visitBin(e);
    }

    void visitCatAssign(CatAssignExp e)
    {
        if (hgs.vcg_ast && e.lowering)
            expressionToBuffer(e.lowering, buf, hgs);
        else
            visitBin(e);
    }

    void visitIndex(IndexExp e)
    {
        if (!hgs.vcg_ast && e.loweredFrom)
        {
            expressionToBuffer(e.loweredFrom, buf, hgs);
            return;
        }
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put('[');
        sizeToBuffer(e.e2, buf, hgs);
        buf.put(']');
    }

    void visitPost(PostExp e)
    {
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
        buf.put(EXPtoString(e.op));
    }

    void visitPre(PreExp e)
    {
        buf.put(EXPtoString(e.op));
        expToBuffer(e.e1, precedence[e.op], buf, hgs);
    }

    void visitRemove(RemoveExp e)
    {
        expToBuffer(e.e1, PREC.primary, buf, hgs);
        buf.put(".remove(");
        expToBuffer(e.e2, PREC.assign, buf, hgs);
        buf.put(')');
    }

    void visitCond(CondExp e)
    {
        expToBuffer(e.econd, PREC.oror, buf, hgs);
        buf.put(" ? ");
        expToBuffer(e.e1, PREC.expr, buf, hgs);
        buf.put(" : ");
        expToBuffer(e.e2, PREC.cond, buf, hgs);
    }

    void visitDefaultInit(DefaultInitExp e)
    {
        buf.put(EXPtoString(e.op));
    }

    void visitClassReference(ClassReferenceExp e)
    {
        buf.put(e.value.toChars());
    }

    if (e.rvalue)
        buf.put("__rvalue(");

    scope (exit)
        if (e.rvalue)
            buf.put(')');

    switch (e.op)
    {
        default:
            if (auto be = e.isBinExp())
                return visitBin(be);
            if (auto ue = e.isUnaExp())
                return visitUna(ue);
            if (auto de = e.isDefaultInitExp())
                return visitDefaultInit(e.isDefaultInitExp());
            return visit(e);

        case EXP.int64:         return visitInteger(e.isIntegerExp());
        case EXP.error:         return visitError(e.isErrorExp());
        case EXP.void_:         return visitVoidInit(e.isVoidInitExp());
        case EXP.float64:       return visitReal(e.isRealExp());
        case EXP.complex80:     return visitComplex(e.isComplexExp());
        case EXP.identifier:    return visitIdentifier(e.isIdentifierExp());
        case EXP.dSymbol:       return visitDsymbolExp(e.isDsymbolExp());
        case EXP.this_:         return visitThis(e.isThisExp());
        case EXP.super_:        return visitSuper(e.isSuperExp());
        case EXP.null_:         return visitNull(e.isNullExp());
        case EXP.string_:       return visitString(e.isStringExp());
        case EXP.interpolated:  return visitInterpolation(e.isInterpExp());
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
        case EXP.is_:           return visitIs(e.isIsExp());
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
        case EXP.not:           return visitNot(e.isNotExp());
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
        case EXP.concatenate:   return visitCat(e.isCatExp());
        case EXP.concatenateAssign:     return visitCatAssign(e.isCatAssignExp());
        case EXP.concatenateElemAssign: return visitCatAssign(e.isCatElemAssignExp());
        case EXP.concatenateDcharAssign:        return visitCatAssign(e.isCatDcharAssignExp());
        case EXP.minusMinus:
        case EXP.plusPlus:      return visitPost(e.isPostExp());
        case EXP.preMinusMinus:
        case EXP.prePlusPlus:   return visitPre(e.isPreExp());
        case EXP.remove:        return visitRemove(e.isRemoveExp());
        case EXP.question:      return visitCond(e.isCondExp());
        case EXP.classReference:        return visitClassReference(e.isClassReferenceExp());
        case EXP.loweredAssignExp:      return visitLoweredAssignExp(e.isLoweredAssignExp());
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
void floatToBuffer(Type type, const real_t value, ref OutBuffer buf, const bool allowHex)
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
    buf.put(buffer.ptr);
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
            buf.put('F');
            break;
        case Tfloat80:
        case Timaginary80:
        case Tcomplex80:
            buf.put('L');
            break;
        default:
            break;
        }
        if (t.isImaginary())
            buf.put('i');
    }
}

void toCBuffer(const TemplateParameter tp, ref OutBuffer buf, ref HdrGenState hgs)
{
    scope v = new TemplateParameterPrettyPrintVisitor(&buf, &hgs);
    (cast() tp).accept(v);
}

private extern (C++) final class TemplateParameterPrettyPrintVisitor : Visitor
{
    alias visit = Visitor.visit;
public:
    OutBuffer* buf;
    HdrGenState* hgs;

    extern (D) this(OutBuffer* buf, HdrGenState* hgs) scope @safe
    {
        this.buf = buf;
        this.hgs = hgs;
    }

    override void visit(TemplateTypeParameter tp)
    {
        buf.put(tp.ident.toString());
        if (tp.specType)
        {
            buf.put(" : ");
            typeToBuffer(tp.specType, null, *buf, *hgs);
        }
        if (tp.defaultType)
        {
            buf.put(" = ");
            typeToBuffer(tp.defaultType, null, *buf, *hgs);
        }
    }

    override void visit(TemplateThisParameter tp)
    {
        buf.put("this ");
        visit(cast(TemplateTypeParameter)tp);
    }

    override void visit(TemplateAliasParameter tp)
    {
        buf.put("alias ");
        if (tp.specType)
            typeToBuffer(tp.specType, tp.ident, *buf, *hgs);
        else
            buf.put(tp.ident.toString());
        if (tp.specAlias)
        {
            buf.put(" : ");
            objectToBuffer(tp.specAlias, *buf, *hgs);
        }
        if (tp.defaultAlias)
        {
            buf.put(" = ");
            objectToBuffer(tp.defaultAlias, *buf, *hgs);
        }
    }

    override void visit(TemplateValueParameter tp)
    {
        typeToBuffer(tp.valType, tp.ident, *buf, *hgs);
        if (tp.specValue)
        {
            buf.put(" : ");
            tp.specValue.expressionToBuffer(*buf, *hgs);
        }
        if (tp.defaultValue)
        {
            buf.put(" = ");
            tp.defaultValue.expressionToBuffer(*buf, *hgs);
        }
    }

    override void visit(TemplateTupleParameter tp)
    {
        buf.put(tp.ident.toString());
        buf.put("...");
    }
}

private void conditionToBuffer(Condition c, ref OutBuffer buf, ref HdrGenState hgs)
{
    scope v = new ConditionPrettyPrintVisitor(&buf, &hgs);
    c.accept(v);
}

private extern (C++) final class ConditionPrettyPrintVisitor : Visitor
{
    alias visit = Visitor.visit;
public:
    OutBuffer* buf;
    HdrGenState* hgs;

    extern (D) this(OutBuffer* buf, HdrGenState* hgs) scope @safe
    {
        this.buf = buf;
        this.hgs = hgs;
    }

    override void visit(DebugCondition c)
    {
        buf.put("debug");
        if (c.ident)
        {
            buf.put(" (");
            buf.put(c.ident.toString());
            buf.put(')');
        }
    }

    override void visit(VersionCondition c)
    {
        buf.put("version (");
        buf.put(c.ident.toString());
        buf.put(')');
    }

    override void visit(StaticIfCondition c)
    {
        buf.put("static if (");
        c.exp.expressionToBuffer(*buf, *hgs);
        buf.put(')');
    }
}

void toCBuffer(const Statement s, ref OutBuffer buf, ref HdrGenState hgs)
{
    (cast()s).statementToBuffer(buf, hgs);
}

void toCBuffer(const Type t, ref OutBuffer buf, const Identifier ident, ref HdrGenState hgs)
{
    typeToBuffer(cast() t, ident, buf, hgs);
}

// used from TemplateInstance::toChars() and TemplateMixin::toChars()
void toCBufferInstance(const TemplateInstance ti, ref OutBuffer buf, bool qualifyTypes = false)
{
    HdrGenState hgs;
    hgs.fullQual = qualifyTypes;

    buf.put(ti.name.toChars());
    tiargsToBuffer(cast() ti, buf, hgs);
}

void toCBuffer(const Initializer iz, ref OutBuffer buf, ref HdrGenState hgs)
{
    initializerToBuffer(cast() iz, buf, hgs);
}

bool stcToBuffer(ref OutBuffer buf, STC stc) @safe
{
    //printf("stc: %llx\n", stc);
    bool result = false;

    if (stc & STC.scopeinferred)
    {
        //buf.put("scope-inferred ");
        stc &= ~(STC.scope_ | STC.scopeinferred);
    }
    if (stc & STC.returninferred)
    {
        //buf.put((stc & STC.returnScope) ? "return-scope-inferred " : "return-ref-inferred ");
        stc &= ~(STC.return_ | STC.returninferred);
    }

    // ensure `auto ref` keywords are (almost) adjacent
    if (stc & STC.auto_)
    {
        buf.put("auto ");
        stc &= ~STC.auto_;
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
        case ScopeRef.Return:
            break;

        case ScopeRef.ReturnScope:      rrs = "return scope"; goto L1;
        case ScopeRef.ReturnRef:        rrs = isout ? "return out"       : "return ref";       goto L1;
        case ScopeRef.Ref:              rrs = isout ? "out"              : "ref";              goto L1;
        case ScopeRef.RefScope:         rrs = isout ? "out scope"        : "ref scope";        goto L1;
        case ScopeRef.ReturnRef_Scope:  rrs = isout ? "return out scope" : "return ref scope"; goto L1;
        case ScopeRef.Ref_ReturnScope:  rrs = isout ? "out return scope" : "ref return scope"; goto L1;
        L1:
            buf.put(rrs);
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
            buf.put(' ');
        result = true;
        buf.put(s);
    }

    return result;
}

/*************************************************
 * Pick off one of the storage classes from stc,
 * and return a string representation of it.
 * stc is reduced by the one picked.
 */
string stcToString(ref STC stc) @safe
{
    static struct SCstring
    {
        STC stc;
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
        const STC tbl = entry.stc;
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

private void linkageToBuffer(ref OutBuffer buf, LINK linkage) @safe
{
    const s = linkageToString(linkage);
    if (s.length)
    {
        buf.put("extern (");
        buf.put(s);
        buf.put(')');
    }
}

const(char)* linkageToChars(LINK linkage)
{
    /// Works because we return a literal
    return linkageToString(linkage).ptr;
}

string linkageToString(LINK linkage) pure nothrow @safe
{
    with (LINK)
    {
        immutable string[7] a = [
                default_ : null,
                d        : "D",
                c        : "C",
                cpp      : "C++",
                windows  : "Windows",
                objc     : "Objective-C",
                system   : "System" ];
        return a[linkage];
    }
}

void visibilityToBuffer(ref OutBuffer buf, Visibility vis)
{
    buf.put(visibilityToString(vis.kind));
    if (vis.kind == Visibility.Kind.package_ && vis.pkg)
    {
        buf.put('(');
        buf.put(vis.pkg.toPrettyChars(true));
        buf.put(')');
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
extern (D) string visibilityToString(Visibility.Kind kind) nothrow pure @safe
{
    with (Visibility.Kind)
    {
        immutable string[7] a = [
                none       : "none",
                private_   : "private",
                package_   : "package",
                protected_ : "protected",
                public_    : "public",
                export_    : "export" ];
        return a[kind];
    }
}

// Print the full function signature with correct ident, attributes and template args
void functionToBufferFull(TypeFunction tf, ref OutBuffer buf, const Identifier ident, ref HdrGenState hgs, TemplateDeclaration td)
{
    //printf("TypeFunction::toCBuffer() this = %p\n", this);
    visitFuncIdentWithPrefix(tf, ident, td, buf, hgs);
}

// ident is inserted before the argument list and will be "function" or "delegate" for a type
void functionToBufferWithIdent(TypeFunction tf, ref OutBuffer buf, const(char)* ident, bool isStatic)
{
    HdrGenState hgs;
    visitFuncIdentWithPostfix(tf, ident.toDString(), buf, hgs, isStatic);
}

void toCBuffer(const Expression e, ref OutBuffer buf, ref HdrGenState hgs)
{
    expressionPrettyPrint(cast()e, buf, hgs);
}

/**************************************************
 * Write out argument types to buf.
 */
void argExpTypesToCBuffer(ref OutBuffer buf, Expressions* arguments)
{
    if (!arguments || !arguments.length)
        return;
    HdrGenState hgs;
    foreach (i, arg; *arguments)
    {
        if (i)
            buf.put(", ");
        typeToBuffer(arg.type, null, buf, hgs);
    }
}

void arrayObjectsToBuffer(ref OutBuffer buf, Objects* objects)
{
    if (!objects || !objects.length)
        return;
    HdrGenState hgs;
    foreach (i, o; *objects)
    {
        if (i)
            buf.put(", ");
        objectToBuffer(o, buf, hgs);
    }
}

/*************************************************************
 * Pretty print function parameters.
 * Params:
 *  pl = parameter list to print
 * Returns: Null-terminated string representing parameters.
 */
const(char)* parametersTypeToChars(ParameterList pl)
{
    OutBuffer buf;
    HdrGenState hgs;
    parametersToBuffer(pl, buf, hgs);
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

    parameterToBuffer(parameter, buf, hgs);

    if (tf.parameterList.varargs == VarArg.typesafe && parameter == tf.parameterList[tf.parameterList.parameters.length - 1])
    {
        buf.put("...");
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

private void parametersToBuffer(ParameterList pl, ref OutBuffer buf, ref HdrGenState hgs)
{
    buf.put('(');
    foreach (i; 0 .. pl.length)
    {
        if (i)
            buf.put(", ");
        pl[i].parameterToBuffer(buf, hgs);
    }
    final switch (pl.varargs)
    {
        case VarArg.none:
        case VarArg.KRvariadic:
            break;

        case VarArg.variadic:
            if (pl.length)
                buf.put(", ");

            if (stcToBuffer(buf, pl.stc))
                buf.put(' ');
            goto case VarArg.typesafe;

        case VarArg.typesafe:
            buf.put("...");
            break;
    }
    buf.put(')');
}


/***********************************************************
 * Write parameter `p` to buffer `buf`.
 * Params:
 *      p = parameter to serialize
 *      buf = buffer to write it to
 *      hgs = context
 */
private void parameterToBuffer(Parameter p, ref OutBuffer buf, ref HdrGenState hgs)
{
    if (p.userAttribDecl)
    {
        buf.put('@');

        bool isAnonymous = p.userAttribDecl.atts.length > 0 && !(*p.userAttribDecl.atts)[0].isCallExp();
        if (isAnonymous)
            buf.put('(');

        argsToBuffer(p.userAttribDecl.atts, buf, hgs);

        if (isAnonymous)
            buf.put(')');
        buf.put(' ');
    }
    if (p.storageClass & STC.auto_)
        buf.put("auto ");

    STC stc = p.storageClass;
    if (p.storageClass & STC.in_)
    {
        buf.put("in ");
        if ((p.storageClass & (STC.constscoperef | STC.ref_)) == (STC.constscoperef | STC.ref_))
            stc &= ~STC.ref_;
    }
    else if (p.storageClass & STC.lazy_)
        buf.put("lazy ");
    else if (p.storageClass & STC.alias_)
        buf.put("alias ");

    if (p.type && p.type.mod & MODFlags.shared_)
        stc &= ~STC.shared_;

    if (stcToBuffer(buf, stc & (STC.const_ | STC.immutable_ | STC.wild | STC.shared_ |
        STC.return_ | STC.returninferred | STC.scope_ | STC.scopeinferred | STC.out_ | STC.ref_ | STC.returnScope)))
        buf.put(' ');

    const(char)[] s;
    if (p.storageClass & STC.alias_)
    {
        if (p.ident)
            buf.put(p.ident.toString());
    }
    else if (p.type.isTypeIdentifier() &&
             (s = p.type.isTypeIdentifier().ident.toString()).length > 3 &&
             s[0..3] == "__T")
    {
        // print parameter name, instead of undetermined type parameter
        buf.put(p.ident.toString());
    }
    else
    {
        typeToBuffer(p.type, p.ident, buf, hgs, (stc & STC.in_) ? MODFlags.const_ : 0);
    }

    if (p.defaultArg)
    {
        buf.put(" = ");
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
 *     names = if non-null, use these as the argument Labels for the arguments
 */
private void argsToBuffer(Expressions* expressions, ref OutBuffer buf, ref HdrGenState hgs, Expression basis = null, ArgumentLabels* names = null)
{
    if (!expressions || !expressions.length)
        return;
    version (all)
    {
        foreach (i, el; *expressions)
        {
            if (i)
                buf.put(", ");

            if (names && i < names.length && (*names)[i].name)
            {
                buf.put((*names)[i].name.toString());
                buf.put(": ");
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
            buf.put("0..");
            buf.print(expressions.length);
            buf.put(": ");
            expToBuffer(basis, PREC.assign, buf, hgs);
        }
        foreach (i, el; *expressions)
        {
            if (el)
            {
                if (basis)
                {
                    buf.put(", ");
                    buf.print(i);
                    buf.put(": ");
                }
                else if (i)
                    buf.put(", ");
                expToBuffer(el, PREC.assign, buf, hgs);
            }
        }
    }
}

private void sizeToBuffer(Expression e, ref OutBuffer buf, ref HdrGenState hgs)
{
    if (e.type == Type.tsize_t)
    {
        Expression ex = (e.op == EXP.cast_ ? (cast(CastExp)e).e1 : e);
        ex = ex.optimize(WANTvalue);
        const ulong uval = ex.op == EXP.int64 ? ex.toInteger() : cast(ulong)-1;
        if (cast(long)uval >= 0)
        {
            if (uval <= 0xFFFFU)
            {
                buf.print(uval);
                return;
            }
            if (uval <= 0x7FFF_FFFF_FFFF_FFFFUL)
            {
                buf.put("cast(size_t)");
                buf.print(uval);
                return;
            }
        }
    }
    expToBuffer(e, PREC.assign, buf, hgs);
}

private void expressionToBuffer(Expression e, ref OutBuffer buf, ref HdrGenState hgs)
{
    expressionPrettyPrint(e, buf, hgs);
}

// to be called if e could be loweredFrom another expression instead of acessing precedence[e.op] directly
private PREC expPrecedence(ref HdrGenState hgs, Expression e)
{
    if (!hgs.vcg_ast)
    {
        if (auto ce = e.isCallExp())
        {
            if (ce.loweredFrom)
                e = ce.loweredFrom;
        }
        else if (auto ne = e.isNotExp())
            if (ne.loweredFrom)
                e = ne.loweredFrom;
    }
    return precedence[e.op];
}

/**************************************************
 * Write expression out to buf, but wrap it
 * in ( ) if its precedence is less than pr.
 */
private void expToBuffer(Expression e, PREC pr, ref OutBuffer buf, ref HdrGenState hgs)
{
    auto prec = expPrecedence(hgs, e);
    debug
    {
        if (prec == PREC.zero)
            printf("precedence not defined for token '%s'\n", EXPtoString(e.op).ptr);
    }
    if (e.op == 0xFF)
    {
        buf.put("<FF>");
        return;
    }
    assert(prec != PREC.zero);
    assert(pr != PREC.zero);
    /* Despite precedence, we don't allow a<b<c expressions.
     * They must be parenthesized.
     */
    if (prec < pr || (pr == PREC.rel && prec == pr)
        || (pr >= PREC.or && pr <= PREC.and && prec == PREC.rel))
    {
        buf.put('(');
        e.expressionToBuffer(buf, hgs);
        buf.put(')');
    }
    else
    {
        e.expressionToBuffer(buf, hgs);
    }
}


/**************************************************
 * An entry point to pretty-print type.
 */
private void typeToBuffer(Type t, const Identifier ident, ref OutBuffer buf, ref HdrGenState hgs,
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
        buf.put(' ');
        buf.put(ident.toString());
    }
}

private void visitWithMask(Type t, ubyte modMask, ref OutBuffer buf, ref HdrGenState hgs)
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
            buf.put('(');
        }
        if (m & MODFlags.wild)
        {
            MODtoBuffer(buf, MODFlags.wild);
            buf.put('(');
        }
        if (m & (MODFlags.const_ | MODFlags.immutable_))
        {
            MODtoBuffer(buf, m & (MODFlags.const_ | MODFlags.immutable_));
            buf.put('(');
        }
        typeToBufferx(t, buf, hgs);
        if (m & (MODFlags.const_ | MODFlags.immutable_))
            buf.put(')');
        if (m & MODFlags.wild)
            buf.put(')');
        if (m & MODFlags.shared_)
            buf.put(')');
    }
}


private void dumpTemplateInstance(TemplateInstance ti, ref OutBuffer buf, ref HdrGenState hgs)
{
    buf.put('{');
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
    buf.put('}');
    buf.writenl();

}

private void tiargsToBuffer(TemplateInstance ti, ref OutBuffer buf, ref HdrGenState hgs)
{
    buf.put('!');
    if (ti.nest)
    {
        buf.put("(...)");
        return;
    }
    if (!ti.tiargs)
    {
        buf.put("()");
        return;
    }
    if (ti.tiargs.length == 1)
    {
        RootObject oarg = (*ti.tiargs)[0];
        if (Type t = isType(oarg))
        {
            if (t.equals(Type.tstring) || t.equals(Type.twstring) || t.equals(Type.tdstring) || t.mod == 0 && (t.isTypeBasic() || t.ty == Tident && (cast(TypeIdentifier)t).idents.length == 0))
            {
                HdrGenState hgs2;       // re-examine need for new hgs
                hgs2.fullQual = (t.ty == Tclass && !t.mod);
                toCBuffer(t, buf, null, hgs2);
                return;
            }
        }
        else if (Expression e = isExpression(oarg))
        {
            if (!(e.type && e.type.isTypeEnum()) && e.op == EXP.int64 || e.op == EXP.float64 ||
                e.op == EXP.null_ || e.op == EXP.string_ || e.op == EXP.this_)
            {
                toCBuffer(e, buf, hgs);
                return;
            }
        }
    }
    buf.put('(');
    ti.nestUp();
    foreach (i, arg; *ti.tiargs)
    {
        if (i)
            buf.put(", ");
        objectToBuffer(arg, buf, hgs);
    }
    ti.nestDown();
    buf.put(')');
}

/****************************************
 * This makes a 'pretty' version of the template arguments.
 * It's analogous to genIdent() which makes a mangled version.
 */
private void objectToBuffer(RootObject oarg, ref OutBuffer buf, ref HdrGenState hgs)
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
        if (s.ident)
            buf.put(s.ident.toString());
        else
            buf.put(s.toChars());
    }
    else if (auto v = isTuple(oarg))
    {
        auto args = &v.objects;
        foreach (i, arg; *args)
        {
            if (i)
                buf.put(", ");
            objectToBuffer(arg, buf, hgs);
        }
    }
    else if (auto p = isParameter(oarg))
    {
        parameterToBuffer(p, buf, hgs);
    }
    else if (!oarg)
    {
        buf.put("NULL");
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


private void visitFuncIdentWithPostfix(TypeFunction t, const char[] ident, ref OutBuffer buf, ref HdrGenState hgs, bool isStatic)
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
        buf.put(' ');
    }
    if (t.linkage == LINK.objc && isStatic)
        buf.write("static ");
    if (t.next)
    {
        typeToBuffer(t.next, null, buf, hgs);
        if (ident)
            buf.put(' ');
    }
    else if (hgs.ddoc)
        buf.put("auto ");
    if (ident)
        buf.put(ident);
    parametersToBuffer(t.parameterList, buf, hgs);
    /* Use postfix style for attributes
     */
    if (t.mod)
    {
        buf.put(' ');
        MODtoBuffer(buf, t.mod);
    }

    void dg(string str)
    {
        buf.put(' ');
        buf.put(str);
    }
    t.attributesApply(&dg);

    t.inuse--;
}

private void visitFuncIdentWithPrefix(TypeFunction t, const Identifier ident, TemplateDeclaration td,
    ref OutBuffer buf, ref HdrGenState hgs)
{
    if (t.inuse)
    {
        t.inuse = 2; // flag error to caller
        return;
    }
    t.inuse++;

    /* Use 'storage class' (prefix) style for attributes
     */
    if (t.mod && !(hgs.ddoc || hgs.hdrgen))
    {
        MODtoBuffer(buf, t.mod);
        buf.put(' ');
    }

    void dg(string str)
    {
        if (str != "return" && str != "scope")
        {
            // don't write 'ref' for ctors
            if ((ident == Id.ctor) && str == "ref")
                return;
            buf.put(str);
            buf.put(' ');
        }
    }
    t.attributesApply(&dg);

    if (t.linkage > LINK.d && hgs.ddoc != 1 && !hgs.hdrgen)
    {
        linkageToBuffer(buf, t.linkage);
        buf.put(' ');
    }
    if (ident && ident.toHChars2() != ident.toChars())
    {
        // Don't print return type for ctor, dtor, unittest, etc
    }
    else if (t.next)
    {
        typeToBuffer(t.next, null, buf, hgs);
        if (ident)
            buf.put(' ');
    }
    else if (hgs.ddoc)
        buf.put("auto ");
    if (ident)
        buf.put(ident.toHChars2());
    if (td)
    {
        buf.put('(');
        foreach (i, p; *td.origParameters)
        {
            if (i)
                buf.put(", ");
            toCBuffer(p, buf, hgs);
        }
        buf.put(')');
    }
    parametersToBuffer(t.parameterList, buf, hgs);
    // postfix this attributes are more readable
    if (t.mod && (hgs.ddoc || hgs.hdrgen))
    {
        buf.put(' ');
        MODtoBuffer(buf, t.mod);
    }
    if (t.isReturnScope && !t.isReturnInferred)
    {
        buf.put(" return scope");
    }
    else if (t.isScopeQual && !t.isScopeInferred)
    {
        buf.put(" scope");
    }
    if (t.isReturn && !t.isReturnScope && !t.isReturnInferred)
    {
        buf.put(" return");
    }
    t.inuse--;
}


private void initializerToBuffer(Initializer inx, ref OutBuffer buf, ref HdrGenState hgs)
{
    void visitError(ErrorInitializer iz)
    {
        buf.put("__error__");
    }

    void visitVoid(VoidInitializer iz)
    {
        buf.put("void");
    }

    void visitDefault(DefaultInitializer iz)
    {
        buf.put("{ }");
    }

    void visitStruct(StructInitializer si)
    {
        //printf("StructInitializer::toCBuffer()\n");
        buf.put('{');
        foreach (i, const id; si.field)
        {
            if (i)
                buf.put(", ");
            if (id)
            {
                buf.put(id.toString());
                buf.put(':');
            }
            if (auto iz = si.value[i])
                initializerToBuffer(iz, buf, hgs);
        }
        buf.put('}');
    }

    void visitArray(ArrayInitializer ai)
    {
        buf.put('[');
        foreach (i, ex; ai.index)
        {
            if (i)
                buf.put(", ");
            if (ex)
            {
                ex.expressionToBuffer(buf, hgs);
                buf.put(':');
            }
            if (auto iz = ai.value[i])
                initializerToBuffer(iz, buf, hgs);
        }
        buf.put(']');
    }

    void visitExp(ExpInitializer ei)
    {
        ei.exp.expressionToBuffer(buf, hgs);
    }

    void visitC(CInitializer ci)
    {
        buf.put('{');
        foreach (i, ref DesigInit di; ci.initializerList)
        {
            if (i)
                buf.put(", ");
            if (di.designatorList)
            {
                foreach (ref Designator d; (*di.designatorList)[])
                {
                    if (d.exp)
                    {
                        buf.put('[');
                        toCBuffer(d.exp, buf, hgs);
                        buf.put(']');
                    }
                    else
                    {
                        buf.put('.');
                        buf.put(d.ident.toString());
                    }
                }
                buf.put('=');
            }
            initializerToBuffer(di.initializer, buf, hgs);
        }
        buf.put('}');
    }

    mixin VisitInitializer!void visit;
    visit.VisitInitializer(inx);
}


private void typeToBufferx(Type t, ref OutBuffer buf, ref HdrGenState hgs)
{
    void visitType(Type t)
    {
        printf("t = %p, ty = %d\n", t, t.ty);
        assert(0);
    }

    void visitError(TypeError t)
    {
        buf.put("_error_");
    }

    void visitBasic(TypeBasic t)
    {
        //printf("TypeBasic::toCBuffer2(t.mod = %d)\n", t.mod);
        buf.put(t.dstring);
    }

    void visitTraits(TypeTraits t)
    {
        //printf("TypeBasic::toCBuffer2(t.mod = %d)\n", t.mod);
        t.exp.expressionToBuffer(buf, hgs);
    }

    void visitVector(TypeVector t)
    {
        //printf("TypeVector::toCBuffer2(t.mod = %d)\n", t.mod);
        buf.put("__vector(");
        visitWithMask(t.basetype, t.mod, buf, hgs);
        buf.put(")");
    }

    void visitSArray(TypeSArray t)
    {
        visitWithMask(t.next, t.mod, buf, hgs);
        buf.put('[');
        sizeToBuffer(t.dim, buf, hgs);
        buf.put(']');
    }

    void visitDArray(TypeDArray t)
    {
        Type ut = t.castMod(0);
        if (hgs.declstring)
            goto L1;
        if (ut.equals(Type.tstring))
            buf.put("string");
        else if (ut.equals(Type.twstring))
            buf.put("wstring");
        else if (ut.equals(Type.tdstring))
            buf.put("dstring");
        else
        {
        L1:
            visitWithMask(t.next, t.mod, buf, hgs);
            buf.put("[]");
        }
    }

    void visitAArray(TypeAArray t)
    {
        visitWithMask(t.next, t.mod, buf, hgs);
        buf.put('[');
        visitWithMask(t.index, 0, buf, hgs);
        buf.put(']');
    }

    void visitPointer(TypePointer t)
    {
        //printf("TypePointer::toCBuffer2() next = %d\n", t.next.ty);
        if (t.next.ty == Tfunction)
            visitFuncIdentWithPostfix(cast(TypeFunction)t.next, "function", buf, hgs, false);
        else
        {
            visitWithMask(t.next, t.mod, buf, hgs);
            buf.put('*');
        }
    }

    void visitReference(TypeReference t)
    {
        visitWithMask(t.next, t.mod, buf, hgs);
        buf.put('&');
    }

    void visitFunction(TypeFunction t)
    {
        //printf("TypeFunction::toCBuffer2() t = %p, ref = %d\n", t, t.isRef);
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
                buf.put('.');
                TemplateInstance ti = cast(TemplateInstance)id;
                ti.dsymbolToBuffer(buf, hgs);
                break;
            case expression:
                buf.put('[');
                (cast(Expression)id).expressionToBuffer(buf, hgs);
                buf.put(']');
                break;
            case type:
                buf.put('[');
                typeToBufferx(cast(Type)id, buf, hgs);
                buf.put(']');
                break;
            default:
                buf.put('.');
                buf.put(id.toString());
            }
        }
    }

    void visitIdentifier(TypeIdentifier t)
    {
        //printf("visitTypeIdentifier() %s\n", t.ident.toChars());
        buf.put(t.ident.toString());
        visitTypeQualifiedHelper(t);
    }

    void visitInstance(TypeInstance t)
    {
        t.tempinst.dsymbolToBuffer(buf, hgs);
        visitTypeQualifiedHelper(t);
    }

    void visitTypeof(TypeTypeof t)
    {
        buf.put("typeof(");
        t.exp.expressionToBuffer(buf, hgs);
        buf.put(')');
        visitTypeQualifiedHelper(t);
    }

    void visitReturn(TypeReturn t)
    {
        buf.put("typeof(return)");
        visitTypeQualifiedHelper(t);
    }

    void visitEnum(TypeEnum t)
    {
        //printf("visitEnum: %s\n", t.sym.toChars());
        buf.put(hgs.fullQual ? t.sym.toPrettyChars() : t.sym.toChars());
    }

    void visitStruct(TypeStruct t)
    {
        //printf("visitTypeStruct() %s\n", t.sym.toChars());

        // https://issues.dlang.org/show_bug.cgi?id=13776
        // Don't use ti.toAlias() to avoid forward reference error
        // while printing messages.
        TemplateInstance ti = t.sym.parent ? t.sym.parent.isTemplateInstance() : null;
        if (ti && ti.aliasdecl == t.sym)
            buf.put(hgs.fullQual ? ti.toPrettyChars() : ti.toChars());
        else
            buf.put(hgs.fullQual ? t.sym.toPrettyChars() : t.sym.toChars());
    }

    void visitClass(TypeClass t)
    {
        // https://issues.dlang.org/show_bug.cgi?id=13776
        // Don't use ti.toAlias() to avoid forward reference error
        // while printing messages.
        TemplateInstance ti = t.sym.parent ? t.sym.parent.isTemplateInstance() : null;
        if (ti && ti.aliasdecl == t.sym)
            buf.put(hgs.fullQual ? ti.toPrettyChars() : ti.toChars());
        else
            buf.put(hgs.fullQual ? t.sym.toPrettyChars() : t.sym.toChars());
    }

    void visitTag(TypeTag t)
    {
        if (hgs.importcHdr && t.id)
        {
            // https://issues.dlang.org/show_bug.cgi?id=24670
            // `const` must be parenthesized because it can be a return type
            if (t.mod & MODFlags.const_)
                buf.put("const(");

            // For C to D translation, `struct S` or `enum S` simply becomes `S`
            buf.put(t.id.toString());

            if (t.mod & MODFlags.const_)
                buf.put(")");
            return;
        }
        // The following produces something like "const enum E : short"
        if (t.mod & MODFlags.const_)
            buf.put("const ");
        buf.put(Token.toString(t.tok));
        buf.put(' ');
        if (t.id)
            buf.put(t.id.toString());
        if (t.tok == TOK.enum_ && t.base && t.base.ty != TY.Tint32)
        {
            buf.put(" : ");
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
        buf.put('[');
        sizeToBuffer(t.lwr, buf, hgs);
        buf.put(" .. ");
        sizeToBuffer(t.upr, buf, hgs);
        buf.put(']');
    }

    void visitNull(TypeNull t)
    {
        buf.put("typeof(null)");
    }

    void visitMixin(TypeMixin t)
    {
        buf.put("mixin(");
        argsToBuffer(t.exps, buf, hgs, null);
        buf.put(')');
    }

    void visitNoreturn(TypeNoreturn t)
    {
        buf.put("noreturn");
    }

    if (hgs.importcHdr && !hgs.inCAlias && t.mcache && t.mcache.typedefIdent)
    {
        buf.put(t.mcache.typedefIdent.toString());
        return;
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
        EXP.tuple : "sequence",
        EXP.traits : "__traits",
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
        EXP.loweredAssignExp : "=",

        EXP.thrownException : "CTFE ThrownException",
        EXP.cantExpression :  "<cant>",
        EXP.voidExpression : "cast(void)0",
        EXP.showCtfeContext : "<error>",
        EXP.break_ : "<break>",
        EXP.continue_ : "<continue>",
        EXP.goto_ : "<goto>",
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
