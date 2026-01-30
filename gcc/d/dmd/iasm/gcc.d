/**
 * Inline assembler for the GCC D compiler.
 *
 *              Copyright (C) 2018-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     Iain Buclaw
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/iasmgcc.d, _iasmgcc.d)
 * Documentation:  https://dlang.org/phobos/dmd_iasmgcc.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/iasmgcc.d
 */

module dmd.iasm.gcc;

import core.stdc.string;

import dmd.arraytypes;
import dmd.astcodegen;
import dmd.dscope;
import dmd.dsymbol;
import dmd.errors;
import dmd.errorsink;
import dmd.expression;
import dmd.expressionsem;
import dmd.identifier;
import dmd.globals;
import dmd.location;
import dmd.cparse;
import dmd.parse;
import dmd.target;
import dmd.tokens;
import dmd.statement;
import dmd.statementsem;

/***********************************
 * Parse and run semantic analysis on a GccAsmStatement.
 * Params:
 *      s  = gcc asm statement being parsed
 *      sc = the scope where the asm statement is located
 * Returns:
 *      the completed gcc asm statement, or null if errors occurred
 */
public Statement gccAsmSemantic(GccAsmStatement s, Scope* sc)
{
    //printf("GccAsmStatement.semantic()\n");
    const bool doUnittests = global.params.parsingUnittestsRequired();
    scope p = (sc && sc.inCfile)
        ? new CParser!ASTCodegen(sc._module, "", false, global.errorSink, target.c, null, &global.compileEnv)
        : new Parser!ASTCodegen(sc._module, "", false, global.errorSink, &global.compileEnv, doUnittests);

    // Make a safe copy of the token list before parsing.
    Token* toklist = null;
    Token **ptoklist = &toklist;

    for (Token* token = s.tokens; token; token = token.next)
    {
        *ptoklist = p.allocateToken();
        memcpy(*ptoklist, token, Token.sizeof);
        ptoklist = &(*ptoklist).next;
        *ptoklist = null;
    }
    // Append closing `;` location.
    *ptoklist = p.allocateToken();
    (*ptoklist).value = TOK.semicolon;
    (*ptoklist).loc = s.loc;
    ptoklist = &(*ptoklist).next;
    *ptoklist = null;

    // Adjust starting line number of the parser.
    p.token = *toklist;
    p.baseLoc.startLine = s.loc.linnum;
    p.linnum = s.loc.linnum;

    // Parse the gcc asm statement.
    const errors = global.errors;
    s = p.parseGccAsm(s);
    if (errors != global.errors)
        return null;
    s.stc = sc.stc;

    // Fold the instruction template string.
    s.insn = semanticAsmString(sc, s.insn, "asm instruction template");

    if (s.labels && s.outputargs)
        p.eSink.error(s.loc, "extended asm statements with labels cannot have output constraints");

    // Analyse all input and output operands.
    if (s.args)
    {
        foreach (i; 0 .. s.args.length)
        {
            Expression ec = (*s.constraints)[i];
            (*s.constraints)[i] = semanticAsmString(sc, ec, "asm operand");

            Expression earg = (*s.args)[i];
            earg = earg.expressionSemantic(sc);
            // Check argument is a valid lvalue/rvalue.
            if (i < s.outputargs)
                earg = earg.modifiableLvalue(sc);
            else if (earg.checkValue())
                earg = ErrorExp.get();
            (*s.args)[i] = earg;
        }
    }

    // Analyse all clobbers.
    if (s.clobbers)
    {
        foreach (i; 0 .. s.clobbers.length)
        {
            Expression ec = (*s.clobbers)[i];
            (*s.clobbers)[i] = semanticAsmString(sc, ec, "asm clobber");
        }
    }

    // Analyse all goto labels.
    if (s.labels)
    {
        foreach (i; 0 .. s.labels.length)
        {
            Identifier ident = (*s.labels)[i];
            GotoStatement gs = new GotoStatement(s.loc, ident);
            if (!s.gotos)
                s.gotos = new GotoStatements();
            s.gotos.push(gs);
            gs.statementSemantic(sc);
        }
    }

    return s;
}

/***********************************
 * Run semantic analysis on an CAsmDeclaration.
 * Params:
 *      ad  = asm declaration
 *      sc = the scope where the asm declaration is located
 */
public void gccAsmSemantic(CAsmDeclaration ad, Scope* sc)
{
    import dmd.typesem : pointerTo;
    ad.code = semanticString(sc, ad.code, "asm definition");
    ad.code.type = ad.code.type.nextOf().pointerTo();

    // Asm definition always needs emitting into the root module.
    import dmd.dmodule : Module;
    if (sc._module && sc._module.isRoot())
        return;
    if (Module m = Module.rootModule)
        m.members.push(ad);
}

private:


/***********************************
 * Issue error if the current token is not `value`.
 * Otherwise, advance to next token.
 * Params:
 *      p = parser state
 *      value = token value to compare with
 * Returns:
 *      true    advanced to next token
 *      false   error was issued
 */
bool requireToken(Parser)(Parser p, TOK value)
{
    if (p.token.value == value)
    {
        p.nextToken();
        return true;
    }

    p.eSink.error(p.token.loc, "found `%s` when expecting `%s`",
                  p.token.toChars(), Token.toChars(value));
    return false;
}

/***********************************
 * Run semantic analysis on `exp`, resolving it as a compile-time string.
 * Params:
 *      sc = scope
 *      exp = Expression which expected as a string
 *      s = What the string is expected for, used in error diagnostic
 * Returns:
 *      StringExp or ErrorExp
 */
Expression semanticAsmString(Scope* sc, Expression exp, const char *s)
{
    import dmd.dcast : implicitCastTo;
    import dmd.dsymbolsem : resolveAliasThis;
    import dmd.mtype : isAggregate, Type;

    exp = expressionSemantic(exp, sc);

    // Resolve `alias this` if we were given a struct literal.
    if (auto ad = isAggregate(exp.type))
    {
        if (ad.aliasthis && ad.type && !ad.type.isTypeError())
            exp = resolveAliasThis(sc, exp);
    }

    // Evaluate the expression as a string now or error trying.
    if (auto se = semanticString(sc, exp, s))
        exp = implicitCastTo(se, sc, Type.tstring);

    return exp;
}

/***********************************
 * Parse a D or ImportC assignment expression
 */
Expression parseAssignment(Parser)(Parser p)
{
    if (p.Ccompile)
        return (cast(CParser!ASTCodegen)p).cparseAssignExp();

    return p.parseAssignExp();
}

/***********************************
 * Parse a D or ImportC conditional expression
 */
Expression parseConditional(Parser)(Parser p)
{
    if (p.Ccompile)
        return (cast(CParser!ASTCodegen)p).cparseCondExp();

    return p.parseCondExp();
}

/***********************************
 * Parse a D or ImportC primary expression
 */
Expression parsePrimary(Parser)(Parser p)
{
    if (p.Ccompile)
        return (cast(CParser!ASTCodegen)p).cparsePrimaryExp();

    return p.parsePrimaryExp();
}

/***********************************
 * Parse an expression that evaluates to a string.
 * Grammar:
 *      | AsmStringExpr:
 *      |     StringLiteral
 *      |     ( ConditionalExpression )
 * Params:
 *      p = parser state
 * Returns:
 *      the parsed string expression
 */
Expression parseAsmString(Parser)(Parser p)
{
    if (p.token.value == TOK.leftParenthesis)
    {
        // Skip over opening `(`
        p.nextToken();
        Expression insn = p.parseConditional();
        if (insn.isErrorExp())
            return insn;

        // Look for closing `)`.
        if (!p.requireToken(TOK.rightParenthesis))
            return ErrorExp.get();

        return insn;
    }
    else if (p.token.value != TOK.string_)
    {
        p.eSink.error(p.token.loc, "expected string literal or expression in parentheses");
        return ErrorExp.get();
    }

    return p.parsePrimary();
}

/***********************************
 * Parse list of extended asm input or output operands.
 * Grammar:
 *      | Operands:
 *      |     SymbolicName(opt) AsmStringExpr ( AssignExpression )
 *      |     SymbolicName(opt) AsmStringExpr ( AssignExpression ), Operands
 *      |
 *      | SymbolicName:
 *      |     [ Identifier ]
 * Params:
 *      p = parser state
 *      s = asm statement to parse
 * Returns:
 *      number of operands added to the gcc asm statement
 */
int parseExtAsmOperands(Parser)(Parser p, GccAsmStatement s)
{
    int numargs = 0;

    if (p.token.value == TOK.colon ||
        p.token.value == TOK.colonColon ||
        p.token.value == TOK.semicolon ||
        p.token.value == TOK.endOfFile)
        return numargs;

    while (1)
    {
        Expression arg;
        Identifier name;

        if (p.token.value == TOK.leftBracket)
        {
            // Skip over opening `[`
            p.nextToken();
            if (p.token.value == TOK.identifier)
            {
                // Store the symbolic name
                name = p.token.ident;
                p.nextToken();
            }
            else
            {
                p.eSink.error(p.token.loc, "identifier expected after `[`");
                goto Lerror;
            }
            // Look for closing `]`
            if (!p.requireToken(TOK.rightBracket))
                goto Lerror;
        }

        // Look for the constraint string.
        Expression constraint = p.parseAsmString();
        if (constraint.isErrorExp())
            goto Lerror;

        // Look for the opening `(`
        if (!p.requireToken(TOK.leftParenthesis))
            goto Lerror;

        // Parse the assign expression
        arg = p.parseAssignment();
        if (arg.isErrorExp())
            goto Lerror;

        // Look for the closing `)`
        if (!p.requireToken(TOK.rightParenthesis))
            goto Lerror;

        // Add this operand to the list.
        if (!s.args)
        {
            s.names = new Identifiers();
            s.constraints = new Expressions();
            s.args = new Expressions();
        }
        s.names.push(name);
        s.args.push(arg);
        s.constraints.push(constraint);
        numargs++;

        // If the next token is not a `,`, there are no more operands.
        if (p.token.value != TOK.comma)
            return numargs;

        // Skip over the `,` token.
        p.nextToken();
    }
Lerror:
    while (p.token.value != TOK.semicolon &&
           p.token.value != TOK.endOfFile)
        p.nextToken();

    return 0;
}

/***********************************
 * Parse list of extended asm clobbers.
 * Grammar:
 *      | Clobbers:
 *      |     AsmStringExpr
 *      |     AsmStringExpr , Clobbers
 * Params:
 *      p = parser state
 * Returns:
 *      array of parsed clobber expressions
 */
Expressions* parseExtAsmClobbers(Parser)(Parser p)
{
    Expressions* clobbers;

    if (p.token.value == TOK.colon ||
        p.token.value == TOK.colonColon ||
        p.token.value == TOK.semicolon ||
        p.token.value == TOK.endOfFile)
        return clobbers;

    while (1)
    {
        // Look for the clobbers string
        Expression clobber = p.parseAsmString();
        if (clobber.isErrorExp())
            goto Lerror;

        // Add it to the list.
        if (!clobbers)
            clobbers = new Expressions();
        clobbers.push(clobber);

        // If the next token is not a `,`, there are no more clobbers.
        if (p.token.value != TOK.comma)
            return clobbers;

        // Skip over the `,` token.
        p.nextToken();
    }
Lerror:
    while (p.token.value != TOK.semicolon &&
           p.token.value != TOK.endOfFile)
        p.nextToken();

    return null;
}

/***********************************
 * Parse list of extended asm goto labels.
 * Grammar:
 *      | GotoLabels:
 *      |     Identifier
 *      |     Identifier , GotoLabels
 * Params:
 *      p = parser state
 * Returns:
 *      array of parsed goto labels
 */
Identifiers* parseExtAsmGotoLabels(Parser)(Parser p)
{
    Identifiers* labels;

    while (1)
    {
        if (p.token.value == TOK.identifier)
        {
            if (!labels)
                labels = new Identifiers();
            labels.push(p.token.ident);

            // If the next token is not a `,`, there are no more labels.
            if (p.nextToken() != TOK.comma)
                return labels;

            // Skip over the `,` token.
            p.nextToken();
        }
        else
        {
            p.eSink.error(p.token.loc, "identifier expected for goto label name, not `%s`",
                          p.token.toChars());
            goto Lerror;
        }
    }
Lerror:
    while (p.token.value != TOK.semicolon &&
           p.token.value != TOK.endOfFile)
        p.nextToken();

    return null;
}

/***********************************
 * Parse a gcc asm statement.
 * There are three forms of inline asm statements, basic, extended, and goto.
 * Grammar:
 *      | AsmInstruction:
 *      |     BasicAsmInstruction
 *      |     ExtAsmInstruction
 *      |     GotoAsmInstruction
 *      |
 *      | BasicAsmInstruction:
 *      |     AsmStringExpr
 *      |
 *      | ExtAsmInstruction:
 *      |     AsmStringExpr : Operands(opt) : Operands(opt) : Clobbers(opt)
 *      |
 *      | GotoAsmInstruction:
 *      |     AsmStringExpr : : Operands(opt) : Clobbers(opt) : GotoLabels(opt)
 * Params:
 *      p = parser state
 *      s = asm statement to parse
 * Returns:
 *      the parsed gcc asm statement
 */
GccAsmStatement parseGccAsm(Parser)(Parser p, GccAsmStatement s)
{
    s.insn = p.parseAsmString();
    if (s.insn.isErrorExp())
        return s;

    // No semicolon followed after instruction template, treat as extended asm.
    if (p.token.value == TOK.colon || p.token.value == TOK.colonColon)
    {
        bool inputs;
        bool clobbers;
        bool labels;

        // Look for outputs.
        if (p.token.value == TOK.colon)
        {
            // Skip over the `:` token.
            p.nextToken();
            // Parse the output operands.
            s.outputargs = p.parseExtAsmOperands(s);
        }
        else if (p.token.value == TOK.colonColon)
            inputs = true;

        // Look for inputs.
        if (inputs || p.token.value == TOK.colon)
        {
            // Skip over the `:` or `::` token.
            p.nextToken();
            // Parse the input operands.
            p.parseExtAsmOperands(s);
        }
        else if (p.token.value == TOK.colonColon)
            clobbers = true;

        // Look for clobbers.
        if (clobbers || p.token.value == TOK.colon)
        {
            // Skip over the `:` or `::` token.
            p.nextToken();
            // Parse the clobbers.
            s.clobbers = p.parseExtAsmClobbers();
        }
        else if (p.token.value == TOK.colonColon)
            labels = true;

        // Look for labels.
        if (labels || p.token.value == TOK.colon)
        {
            // Skip over the `:` or `::` token.
            p.nextToken();
            // Parse the labels.
            s.labels = p.parseExtAsmGotoLabels();
        }
    }

    if (p.token.value == TOK.endOfFile)
        assert(global.errors);
    else
        p.requireToken(TOK.semicolon);

    return s;
}

unittest
{
    import dmd.mtype : TypeBasic;

    if (!global.errorSink)
        global.errorSink = new ErrorSinkCompiler;

    const errors = global.startGagging();
    scope(exit) global.endGagging(errors);

    // If this check fails, then Type._init() was called before reaching here,
    // and the entire chunk of code that follows can be removed.
    assert(ASTCodegen.Type.tint32 is null);
    // Minimally initialize the cached types in ASTCodegen.Type, as they are
    // dependencies for some fail asm tests to succeed.
    ASTCodegen.Type.stringtable._init();
    scope(exit)
    {
        ASTCodegen.Type.deinitialize();
        ASTCodegen.Type.tint32 = null;
        ASTCodegen.Type.tchar = null;
    }
    scope tint32 = new TypeBasic(ASTCodegen.Tint32);
    ASTCodegen.Type.tint32 = tint32;
    scope tchar = new TypeBasic(ASTCodegen.Tchar);
    ASTCodegen.Type.tchar = tchar;

    // Imitates asmSemantic if version = IN_GCC.
    static int semanticAsm(Token* tokens, bool importC)
    {
        const errors = global.errors;
        scope gas = new GccAsmStatement(Loc.initial, tokens);
        const bool doUnittests = false;
        scope p = importC
            ? new CParser!ASTCodegen(null, ";", false, global.errorSink, target.c, null, &global.compileEnv)
            : new Parser!ASTCodegen(null, ";", false, global.errorSink, &global.compileEnv, doUnittests);
        p.token = *tokens;
        p.parseGccAsm(gas);
        return global.errors - errors;
    }

    // Imitates parseStatement for asm statements.
    static void parseAsm(string input, bool expectError, bool importC = false)
    {
        // Generate tokens from input test.
        const bool doUnittests = false;
        scope p = new Parser!ASTCodegen(null, input, false, global.errorSink, &global.compileEnv, doUnittests);
        p.nextToken();

        Token* toklist = null;
        Token** ptoklist = &toklist;
        p.check(TOK.asm_);
        p.check(TOK.leftCurly);
        while (1)
        {
            if (p.token.value == TOK.rightCurly || p.token.value == TOK.endOfFile)
                break;
            if (p.token.value == TOK.colonColon)
            {
                *ptoklist = p.allocateToken();
                memcpy(*ptoklist, &p.token, Token.sizeof);
                (*ptoklist).value = TOK.colon;
                ptoklist = &(*ptoklist).next;

                *ptoklist = p.allocateToken();
                memcpy(*ptoklist, &p.token, Token.sizeof);
                (*ptoklist).value = TOK.colon;
                ptoklist = &(*ptoklist).next;
            }
            else
            {
                *ptoklist = p.allocateToken();
                memcpy(*ptoklist, &p.token, Token.sizeof);
                ptoklist = &(*ptoklist).next;
            }
            *ptoklist = null;
            p.nextToken();
        }
        p.check(TOK.rightCurly);

        auto res = semanticAsm(toklist, importC);
        // Checks for both unexpected passes and failures.
        assert((res == 0) != expectError, input);
    }

    /// Assembly Tests, all should pass.
    /// Note: Frontend is not initialized, use only strings and identifiers.
    immutable string[] passAsmTests = [
        // Basic asm statement
        q{ asm { "nop";
        } },

        // Extended asm statement
        q{ asm { "cpuid"
               : "=a" (a), "=b" (b), "=c" (c), "=d" (d)
               : "a" (input);
        } },

        // Assembly with symbolic names
        q{ asm { "bts %[base], %[offset]"
               : [base] "+rm" (*ptr)
               : [offset] "Ir" (bitnum);
        } },

        // Assembly with clobbers
        q{ asm { "cpuid"
               : "=a" (a)
               : "a" (input)
               : "ebx", "ecx", "edx";
        } },

        // Goto asm statement
        q{ asm { "jmp %l0"
               :
               :
               :
               : Ljmplabel;
        } },

        // Any CTFE-able string allowed as instruction template.
        q{ asm { (generateAsm);
        } },

        // Likewise mixins, permissible so long as the result is a string.
        q{ asm { (mixin(`"repne"`, `~ "scasb"`));
        } },

        // :: token tests
        q{ asm { "" : : : "memory"; } },
        q{ asm { "" :: : "memory"; } },
        q{ asm { "" : :: "memory"; } },
        q{ asm { "" ::: "memory"; } },
        q{ asm { "" :::: label; } },

        // https://github.com/dlang/dmd/issues/21299
        q{ asm { (insn) : (output) (a) : (input) (1) : (clobber); } },
        q{ asm { (['t','e','s','t']) : (['=','r']) (a) : (['r']) (1) : (['m','e','m','o','r','y']); } },

        // https://github.com/dlang/dmd/issues/21679
        q{ asm { "" : "=r" (s.x); } },
    ];

    immutable string[] failAsmTests = [
        // Found 'h' when expecting ';'
        q{ asm { ""h;
        } },

        // https://issues.dlang.org/show_bug.cgi?id=20592
        q{ asm { "nop" : [name] string (expr); } },

        // Expression expected, not ';'
        q{ asm { ""[;
        } },

        // Expression expected, not ':'
        q{ asm { ""
               :
               : "g" (a ? b : : c);
        } },

        // Found ',' when expecting ':'
        q{ asm { "", "";
        } },

        // Identifier expected, not ';'
        q{ asm { "" : ::: ; } },
        q{ asm { "" :: :: ; } },
        q{ asm { "" ::: : ; } },
        q{ asm { "" :::: ; } },

        // https://issues.dlang.org/show_bug.cgi?id=20593
        q{ asm { "instruction" : : "operand" 123; } },

        // https://github.com/dlang/dmd/issues/21298
        q{ asm { 1; }   },
        q{ asm { int; } },
        q{ asm { : "=r" (i); } },
        q{ asm { (; } },
        q{ asm { (""; } },
        q{ asm { "" ,; } },
        q{ asm { "" d; } },
        q{ asm { "" : (; } },
        q{ asm { "" : (""; } },
        q{ asm { "" : ""; } },
        q{ asm { "" : "" (; } },
        q{ asm { "" : "" (a; } },
        q{ asm { "" : "" (a) ,; } },
        q{ asm { "" : "" (a) d; } },
        q{ asm { "" : "" (a) : (; } },
        q{ asm { "" : "" (a) : (""; } },
        q{ asm { "" : "" (a) : ""; } },
        q{ asm { "" : "" (a) : "" (; } },
        q{ asm { "" : "" (a) : "" (b; } },
        q{ asm { "" : "" (a) : "" (b) ,; } },
        q{ asm { "" : "" (a) : "" (b) d; } },
        q{ asm { "" : "" (a) : "" (b) : (; } },
        q{ asm { "" : "" (a) : "" (b) : (""; } },
        q{ asm { "" : "" (a) : "" (b) : "" ,; } },
        q{ asm { "" : "" (a) : "" (b) : "" d; } },
        q{ asm { "" : "" (a) : "" (b) : "" : (; } },
        q{ asm { "" : "" (a) : "" (b) : "" : c ,; } },
        q{ asm { "" : "" (a) : "" (b) : "" : c d; } },
        q{ asm { "" : "" (a) : "" (b) : "" : c :; } },
        q{ asm { "" : "" (a) : "" (b) : "" : c : (; } },
        q{ asm { "" : "" (a) : "" (b) : "" : c : (""; } },
        q{ asm { "" : "" (a) : "" (b) : "" : c : "" (; } },
        q{ asm { "" : "" (a) : "" (b) : "" : c : "" (d; } },
        q{ asm { "" : "" (a) : "" (b) : "" : c : "" (d); } },

        // https://github.com/dlang/dmd/issues/21679
        q{ asm { "" : "=r" (s->x); } },
    ];

    immutable string[] passCAsmTests = [
        // https://github.com/dlang/dmd/issues/21679
        q{ asm { "" : "=r" (s->x); } },
        q{ asm { "" : "=r" (s.x); } }
    ];

    foreach (test; passAsmTests)
        parseAsm(test, false);

    foreach (test; failAsmTests)
        parseAsm(test, true);

    foreach (test; passCAsmTests)
        parseAsm(test, false, /*importC*/true);
}
