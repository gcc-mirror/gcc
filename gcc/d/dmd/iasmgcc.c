
/* Compiler implementation of the D programming language
 * Copyright (C) 2018-2021 by The D Language Foundation, All Rights Reserved
 * written by Iain Buclaw
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/iasmgcc.c
 */

/* Inline assembler for the GCC D compiler.
 */

#include "scope.h"
#include "expression.h"
#include "declaration.h"
#include "errors.h"
#include "parse.h"
#include "statement.h"

/***********************************
 * Parse list of extended asm input or output operands.
 * Grammar:
 *      | Operands:
 *      |     SymbolicName(opt) StringLiteral ( AssignExpression )
 *      |     SymbolicName(opt) StringLiteral ( AssignExpression ), Operands
 *      |
 *      | SymbolicName:
 *      |     [ Identifier ]
 * Params:
 *      p = parser state
 *      s = asm statement to parse
 * Returns:
 *      number of operands added to the gcc asm statement
 */
static int parseExtAsmOperands(Parser *p, GccAsmStatement *s)
{
    int numargs = 0;

    while (1)
    {
        Expression *arg = NULL;
        Identifier *name = NULL;
        Expression *constraint = NULL;

        switch (p->token.value)
        {
            case TOKsemicolon:
            case TOKcolon:
            case TOKeof:
                return numargs;

            case TOKlbracket:
                if (p->peekNext() == TOKidentifier)
                {
                    // Skip over openings `[`
                    p->nextToken();
                    // Store the symbolic name
                    name = p->token.ident;
                    p->nextToken();
                }
                else
                {
                    p->error(s->loc, "expected identifier after `[`");
                    goto Lerror;
                }
                // Look for closing `]`
                p->check(TOKrbracket);
                // Look for the string literal and fall through
                if (p->token.value != TOKstring)
                    goto Ldefault;
                // fall through

            case TOKstring:
                constraint = p->parsePrimaryExp();
                // @@@DEPRECATED@@@
                // Old parser allowed omitting parentheses around the expression.
                // Deprecated in 2.091. Can be made permanent error after 2.100
                if (p->token.value != TOKlparen)
                {
                    arg = p->parseAssignExp();
                    deprecation(arg->loc, "`%s` must be surrounded by parentheses", arg->toChars());
                }
                else
                {
                    // Look for the opening `(`
                    p->check(TOKlparen);
                    // Parse the assign expression
                    arg = p->parseAssignExp();
                    // Look for the closing `)`
                    p->check(TOKrparen);
                }

                if (!s->args)
                {
                    s->names = new Identifiers();
                    s->constraints = new Expressions();
                    s->args = new Expressions();
                }
                s->names->push(name);
                s->args->push(arg);
                s->constraints->push(constraint);
                numargs++;

                if (p->token.value == TOKcomma)
                    p->nextToken();
                break;

            default:
            Ldefault:
                p->error("expected constant string constraint for operand, not `%s`",
                        p->token.toChars());
                goto Lerror;
        }
    }
Lerror:
    while (p->token.value != TOKrcurly &&
           p->token.value != TOKsemicolon &&
           p->token.value != TOKeof)
        p->nextToken();

    return numargs;
}

/***********************************
 * Parse list of extended asm clobbers.
 * Grammar:
 *      | Clobbers:
 *      |     StringLiteral
 *      |     StringLiteral , Clobbers
 * Params:
 *      p = parser state
 * Returns:
 *      array of parsed clobber expressions
 */
static Expressions *parseExtAsmClobbers(Parser *p)
{
    Expressions *clobbers = NULL;

    while (1)
    {
        Expression *clobber;

        switch (p->token.value)
        {
            case TOKsemicolon:
            case TOKcolon:
            case TOKeof:
                return clobbers;

            case TOKstring:
                clobber = p->parsePrimaryExp();
                if (!clobbers)
                    clobbers = new Expressions();
                clobbers->push(clobber);

                if (p->token.value == TOKcomma)
                    p->nextToken();
                break;

            default:
                p->error("expected constant string constraint for clobber name, not `%s`",
                        p->token.toChars());
                goto Lerror;
        }
    }
Lerror:
    while (p->token.value != TOKrcurly &&
           p->token.value != TOKsemicolon &&
           p->token.value != TOKeof)
        p->nextToken();

    return clobbers;
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
static Identifiers *parseExtAsmGotoLabels(Parser *p)
{
    Identifiers *labels = NULL;

    while (1)
    {
        switch (p->token.value)
        {
            case TOKsemicolon:
            case TOKeof:
                return labels;

            case TOKidentifier:
                if (!labels)
                    labels = new Identifiers();
                labels->push(p->token.ident);

                if (p->nextToken() == TOKcomma)
                    p->nextToken();
                break;

            default:
                p->error("expected identifier for goto label name, not `%s`",
                        p->token.toChars());
                goto Lerror;
        }
    }
Lerror:
    while (p->token.value != TOKrcurly &&
           p->token.value != TOKsemicolon &&
           p->token.value != TOKeof)
        p->nextToken();

    return labels;
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
 *      |     Expression
 *      |
 *      | ExtAsmInstruction:
 *      |     Expression : Operands(opt) : Operands(opt) : Clobbers(opt)
 *      |
 *      | GotoAsmInstruction:
 *      |     Expression : : Operands(opt) : Clobbers(opt) : GotoLabels(opt)
 * Params:
 *      p = parser state
 *      s = asm statement to parse
 * Returns:
 *      the parsed gcc asm statement
 */
static GccAsmStatement *parseGccAsm(Parser *p, GccAsmStatement *s)
{
    s->insn = p->parseExpression();
    if (p->token.value == TOKsemicolon || p->token.value == TOKeof)
        goto Ldone;

    // No semicolon followed after instruction template, treat as extended asm.
    for (int section = 0; section < 4; ++section)
    {
        p->check(TOKcolon);

        switch (section)
        {
            case 0:
                s->outputargs = parseExtAsmOperands(p, s);
                break;

            case 1:
                parseExtAsmOperands(p, s);
                break;

            case 2:
                s->clobbers = parseExtAsmClobbers(p);
                break;

            case 3:
                s->labels = parseExtAsmGotoLabels(p);
                break;

            default:
                assert(0);
        }

        if (p->token.value == TOKsemicolon || p->token.value == TOKeof)
            goto Ldone;
    }
Ldone:
    p->check(TOKsemicolon);

    return s;
}

/***********************************
 * Parse and run semantic analysis on a GccAsmStatement.
 * Params:
 *      s  = gcc asm statement being parsed
 *      sc = the scope where the asm statement is located
 * Returns:
 *      the completed gcc asm statement, or null if errors occurred
 */
Statement *gccAsmSemantic(GccAsmStatement *s, Scope *sc)
{
    //printf("GccAsmStatement::semantic()\n");
    Parser p(sc->_module, (const utf8_t *)";", 1, false);

    // Make a safe copy of the token list before parsing.
    Token *toklist = NULL;
    Token **ptoklist = &toklist;

    for (Token *token = s->tokens; token; token = token->next)
    {
        *ptoklist = Token::alloc();
        memcpy(*ptoklist, token, sizeof(Token));
        ptoklist = &(*ptoklist)->next;
        *ptoklist = NULL;
    }
    p.token = *toklist;
    p.scanloc = s->loc;

    // Parse the gcc asm statement.
    s = parseGccAsm(&p, s);
    if (p.errors)
        return NULL;
    s->stc = sc->stc;

    // Fold the instruction template string.
    s->insn = expressionSemantic(s->insn, sc);
    s->insn = s->insn->ctfeInterpret();

    if (s->insn->op != TOKstring || ((StringExp *) s->insn)->sz != 1)
        s->insn->error("asm instruction template must be a constant char string");

    if (s->labels && s->outputargs)
        s->error("extended asm statements with labels cannot have output constraints");

    // Analyse all input and output operands.
    if (s->args)
    {
        for (size_t i = 0; i < s->args->length; i++)
        {
            Expression *e = (*s->args)[i];
            e = expressionSemantic(e, sc);
            // Check argument is a valid lvalue/rvalue.
            if (i < s->outputargs)
                e = e->modifiableLvalue(sc, NULL);
            else if (e->checkValue())
                e = new ErrorExp();
            (*s->args)[i] = e;

            e = (*s->constraints)[i];
            e = expressionSemantic(e, sc);
            assert(e->op == TOKstring && ((StringExp *) e)->sz == 1);
            (*s->constraints)[i] = e;
        }
    }

    // Analyse all clobbers.
    if (s->clobbers)
    {
        for (size_t i = 0; i < s->clobbers->length; i++)
        {
            Expression *e = (*s->clobbers)[i];
            e = expressionSemantic(e, sc);
            assert(e->op == TOKstring && ((StringExp *) e)->sz == 1);
            (*s->clobbers)[i] = e;
        }
    }

    // Analyse all goto labels.
    if (s->labels)
    {
        for (size_t i = 0; i < s->labels->length; i++)
        {
            Identifier *ident = (*s->labels)[i];
            GotoStatement *gs = new GotoStatement(s->loc, ident);
            if (!s->gotos)
                s->gotos = new GotoStatements();
            s->gotos->push(gs);
            statementSemantic(gs, sc);
        }
    }

    return s;
}
