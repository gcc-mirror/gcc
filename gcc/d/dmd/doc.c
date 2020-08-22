
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/doc.c
 */

// This implements the Ddoc capability.

#include "root/dsystem.h"
#include "root/rmem.h"
#include "root/root.h"
#include "root/port.h"
#include "root/aav.h"

#include "attrib.h"
#include "cond.h"
#include "mars.h"
#include "dsymbol.h"
#include "macro.h"
#include "template.h"
#include "lexer.h"
#include "aggregate.h"
#include "declaration.h"
#include "statement.h"
#include "enum.h"
#include "id.h"
#include "module.h"
#include "scope.h"
#include "hdrgen.h"
#include "doc.h"
#include "mtype.h"
#include "utf.h"

void emitMemberComments(ScopeDsymbol *sds, OutBuffer *buf, Scope *sc);
void toDocBuffer(Dsymbol *s, OutBuffer *buf, Scope *sc);
void emitComment(Dsymbol *s, OutBuffer *buf, Scope *sc);

struct Escape
{
    const char *strings[256];

    const char *escapeChar(unsigned c);
};

class Section
{
public:
    const utf8_t *name;
    size_t namelen;

    const utf8_t *body;
    size_t bodylen;

    int nooutput;

    virtual void write(Loc loc, DocComment *dc, Scope *sc, Dsymbols *a, OutBuffer *buf);
};

class ParamSection : public Section
{
public:
    void write(Loc loc, DocComment *dc, Scope *sc, Dsymbols *a, OutBuffer *buf);
};

class MacroSection : public Section
{
public:
    void write(Loc loc, DocComment *dc, Scope *sc, Dsymbols *a, OutBuffer *buf);
};

typedef Array<Section *> Sections;

struct DocComment
{
    Sections sections;             // Section*[]

    Section *summary;
    Section *copyright;
    Section *macros;
    Macro **pmacrotable;
    Escape **pescapetable;

    Dsymbols a;

    DocComment() :
       summary(NULL), copyright(NULL), macros(NULL), pmacrotable(NULL), pescapetable(NULL)
    { }

    static DocComment *parse(Dsymbol *s, const utf8_t *comment);
    static void parseMacros(Escape **pescapetable, Macro **pmacrotable, const utf8_t *m, size_t mlen);
    static void parseEscapes(Escape **pescapetable, const utf8_t *textstart, size_t textlen);

    void parseSections(const utf8_t *comment);
    void writeSections(Scope *sc, Dsymbols *a, OutBuffer *buf);
};


int cmp(const char *stringz, const void *s, size_t slen);
int icmp(const char *stringz, const void *s, size_t slen);
bool isDitto(const utf8_t *comment);
const utf8_t *skipwhitespace(const utf8_t *p);
size_t skiptoident(OutBuffer *buf, size_t i);
size_t skippastident(OutBuffer *buf, size_t i);
size_t skippastURL(OutBuffer *buf, size_t i);
void highlightText(Scope *sc, Dsymbols *a, OutBuffer *buf, size_t offset);
void highlightCode(Scope *sc, Dsymbol *s, OutBuffer *buf, size_t offset);
void highlightCode(Scope *sc, Dsymbols *a, OutBuffer *buf, size_t offset);
void highlightCode2(Scope *sc, Dsymbols *a, OutBuffer *buf, size_t offset);
void highlightCode3(Scope *sc, OutBuffer *buf, const utf8_t *p, const utf8_t *pend);
TypeFunction *isTypeFunction(Dsymbol *s);
Parameter *isFunctionParameter(Dsymbols *a, const utf8_t *p, size_t len);
TemplateParameter *isTemplateParameter(Dsymbols *a, const utf8_t *p, size_t len);

bool isIdStart(const utf8_t *p);
bool isCVariadicArg(const utf8_t *p, size_t len);
bool isIdTail(const utf8_t *p);
bool isIndentWS(const utf8_t *p);
int utfStride(const utf8_t *p);

// Workaround for missing Parameter instance for variadic params. (it's unnecessary to instantiate one).
bool isCVariadicParameter(Dsymbols *a, const utf8_t *p, size_t len)
{
    for (size_t i = 0; i < a->length; i++)
    {
        TypeFunction *tf = isTypeFunction((*a)[i]);
        if (tf && tf->parameterList.varargs == VARARGvariadic && cmp("...", p, len) == 0)
            return true;
    }
    return false;
}

/****************************************************
 */
static Parameter *isFunctionParameter(Dsymbol *s, const utf8_t *p, size_t len)
{
    TypeFunction *tf = isTypeFunction(s);
    if (tf && tf->parameterList.parameters)
    {
        for (size_t k = 0; k < tf->parameterList.parameters->length; k++)
        {
            Parameter *fparam = (*tf->parameterList.parameters)[k];
            if (fparam->ident && cmp(fparam->ident->toChars(), p, len) == 0)
            {
                return fparam;
            }
        }
    }
    return NULL;
}

static Dsymbol *getEponymousMember(TemplateDeclaration *td)
{
    if (!td->onemember)
        return NULL;

    if (AggregateDeclaration *ad = td->onemember->isAggregateDeclaration())
        return ad;
    if (FuncDeclaration *fd = td->onemember->isFuncDeclaration())
        return fd;
    if (td->onemember->isEnumMember())
        return NULL;    // Keep backward compatibility. See compilable/ddoc9.d
    if (VarDeclaration *vd = td->onemember->isVarDeclaration())
        return td->constraint ? NULL : vd;

    return NULL;
}

/****************************************************
 */
static Parameter *isEponymousFunctionParameter(Dsymbols *a, const utf8_t *p, size_t len)
{
    for (size_t i = 0; i < a->length; i++)
    {
        TemplateDeclaration *td = (*a)[i]->isTemplateDeclaration();
        if (td && td->onemember)
        {
            /* Case 1: we refer to a template declaration inside the template

               /// ...ddoc...
               template case1(T) {
                 void case1(R)() {}
               }
             */
            td = td->onemember->isTemplateDeclaration();
        }
        if (!td)
        {
            /* Case 2: we're an alias to a template declaration

               /// ...ddoc...
               alias case2 = case1!int;
             */
            AliasDeclaration *ad = (*a)[i]->isAliasDeclaration();
            if (ad && ad->aliassym)
            {
                td = ad->aliassym->isTemplateDeclaration();
            }
        }
        while (td)
        {
            Dsymbol *sym = getEponymousMember(td);
            if (sym)
            {
                Parameter *fparam = isFunctionParameter(sym, p, len);
                if (fparam)
                {
                    return fparam;
                }
            }
            td = td->overnext;
        }
    }
    return NULL;
}

static TemplateDeclaration *getEponymousParent(Dsymbol *s)
{
    if (!s->parent)
        return NULL;
    TemplateDeclaration *td = s->parent->isTemplateDeclaration();
    return (td && getEponymousMember(td)) ? td : NULL;
}

static const char ddoc_default[] = "\
DDOC =  <html><head>\n\
        <META http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">\n\
        <title>$(TITLE)</title>\n\
        </head><body>\n\
        <h1>$(TITLE)</h1>\n\
        $(BODY)\n\
        <hr>$(SMALL Page generated by $(LINK2 http://dlang.org/ddoc.html, Ddoc). $(COPYRIGHT))\n\
        </body></html>\n\
\n\
B =     <b>$0</b>\n\
I =     <i>$0</i>\n\
U =     <u>$0</u>\n\
P =     <p>$0</p>\n\
DL =    <dl>$0</dl>\n\
DT =    <dt>$0</dt>\n\
DD =    <dd>$0</dd>\n\
TABLE = <table>$0</table>\n\
TR =    <tr>$0</tr>\n\
TH =    <th>$0</th>\n\
TD =    <td>$0</td>\n\
OL =    <ol>$0</ol>\n\
UL =    <ul>$0</ul>\n\
LI =    <li>$0</li>\n\
BIG =   <big>$0</big>\n\
SMALL = <small>$0</small>\n\
BR =    <br>\n\
LINK =  <a href=\"$0\">$0</a>\n\
LINK2 = <a href=\"$1\">$+</a>\n\
LPAREN= (\n\
RPAREN= )\n\
BACKTICK= `\n\
DOLLAR= $\n\
DEPRECATED= $0\n\
\n\
RED =   <font color=red>$0</font>\n\
BLUE =  <font color=blue>$0</font>\n\
GREEN = <font color=green>$0</font>\n\
YELLOW =<font color=yellow>$0</font>\n\
BLACK = <font color=black>$0</font>\n\
WHITE = <font color=white>$0</font>\n\
\n\
D_CODE = <pre class=\"d_code\">$0</pre>\n\
DDOC_BACKQUOTED = $(D_INLINECODE $0)\n\
D_INLINECODE = <pre style=\"display:inline;\" class=\"d_inline_code\">$0</pre>\n\
D_COMMENT = $(GREEN $0)\n\
D_STRING  = $(RED $0)\n\
D_KEYWORD = $(BLUE $0)\n\
D_PSYMBOL = $(U $0)\n\
D_PARAM   = $(I $0)\n\
\n\
DDOC_COMMENT   = <!-- $0 -->\n\
DDOC_DECL      = $(DT $(BIG $0))\n\
DDOC_DECL_DD   = $(DD $0)\n\
DDOC_DITTO     = $(BR)$0\n\
DDOC_SECTIONS  = $0\n\
DDOC_SUMMARY   = $0$(BR)$(BR)\n\
DDOC_DESCRIPTION = $0$(BR)$(BR)\n\
DDOC_AUTHORS   = $(B Authors:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_BUGS      = $(RED BUGS:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_COPYRIGHT = $(B Copyright:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_DATE      = $(B Date:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_DEPRECATED = $(RED Deprecated:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_EXAMPLES  = $(B Examples:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_HISTORY   = $(B History:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_LICENSE   = $(B License:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_RETURNS   = $(B Returns:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_SEE_ALSO  = $(B See Also:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_STANDARDS = $(B Standards:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_THROWS    = $(B Throws:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_VERSION   = $(B Version:)$(BR)\n$0$(BR)$(BR)\n\
DDOC_SECTION_H = $(B $0)$(BR)\n\
DDOC_SECTION   = $0$(BR)$(BR)\n\
DDOC_MEMBERS   = $(DL $0)\n\
DDOC_MODULE_MEMBERS = $(DDOC_MEMBERS $0)\n\
DDOC_CLASS_MEMBERS  = $(DDOC_MEMBERS $0)\n\
DDOC_STRUCT_MEMBERS = $(DDOC_MEMBERS $0)\n\
DDOC_ENUM_MEMBERS   = $(DDOC_MEMBERS $0)\n\
DDOC_TEMPLATE_MEMBERS = $(DDOC_MEMBERS $0)\n\
DDOC_ENUM_BASETYPE = $0\n\
DDOC_PARAMS    = $(B Params:)$(BR)\n$(TABLE $0)$(BR)\n\
DDOC_PARAM_ROW = $(TR $0)\n\
DDOC_PARAM_ID  = $(TD $0)\n\
DDOC_PARAM_DESC = $(TD $0)\n\
DDOC_BLANKLINE  = $(BR)$(BR)\n\
\n\
DDOC_ANCHOR     = <a name=\"$1\"></a>\n\
DDOC_PSYMBOL    = $(U $0)\n\
DDOC_PSUPER_SYMBOL = $(U $0)\n\
DDOC_KEYWORD    = $(B $0)\n\
DDOC_PARAM      = $(I $0)\n\
\n\
ESCAPES = /</&lt;/\n\
          />/&gt;/\n\
          /&/&amp;/\n\
";

static const char ddoc_decl_s[] = "$(DDOC_DECL ";
static const char ddoc_decl_e[] = ")\n";

static const char ddoc_decl_dd_s[] = "$(DDOC_DECL_DD ";
static const char ddoc_decl_dd_e[] = ")\n";


/****************************************************
 */

void gendocfile(Module *m)
{
    static OutBuffer mbuf;
    static int mbuf_done;

    OutBuffer buf;

    //printf("Module::gendocfile()\n");

    if (!mbuf_done)             // if not already read the ddoc files
    {
        mbuf_done = 1;

        // Use our internal default
        mbuf.write(ddoc_default, strlen(ddoc_default));

        // Override with DDOCFILE specified in the sc.ini file
        char *p = getenv("DDOCFILE");
        if (p)
            global.params.ddocfiles.shift(p);

        // Override with the ddoc macro files from the command line
        for (size_t i = 0; i < global.params.ddocfiles.length; i++)
        {
            FileName f(global.params.ddocfiles[i]);
            File file(&f);
            readFile(m->loc, &file);
            // BUG: convert file contents to UTF-8 before use

            //printf("file: '%.*s'\n", file.len, file.buffer);
            mbuf.write(file.buffer, file.len);
        }
    }
    DocComment::parseMacros(&m->escapetable, &m->macrotable, (utf8_t *)mbuf.slice().ptr, mbuf.length());

    Scope *sc = Scope::createGlobal(m);      // create root scope

    DocComment *dc = DocComment::parse(m, m->comment);
    dc->pmacrotable = &m->macrotable;
    dc->pescapetable = &m->escapetable;
    sc->lastdc = dc;

    // Generate predefined macros

    // Set the title to be the name of the module
    {
        const char *p = m->toPrettyChars();
        Macro::define(&m->macrotable, (const utf8_t *)"TITLE", 5, (const utf8_t *)p, strlen(p));
    }

    // Set time macros
    {
        time_t t;
        time(&t);
        char *p = ctime(&t);
        p = mem.xstrdup(p);
        Macro::define(&m->macrotable, (const utf8_t *)"DATETIME", 8, (const utf8_t *)p, strlen(p));
        Macro::define(&m->macrotable, (const utf8_t *)"YEAR", 4, (const utf8_t *)p + 20, 4);
    }

    const char *srcfilename = m->srcfile->toChars();
    Macro::define(&m->macrotable, (const utf8_t *)"SRCFILENAME", 11, (const utf8_t *)srcfilename, strlen(srcfilename));

    const char *docfilename = m->docfile->toChars();
    Macro::define(&m->macrotable, (const utf8_t *)"DOCFILENAME", 11, (const utf8_t *)docfilename, strlen(docfilename));

    if (dc->copyright)
    {
        dc->copyright->nooutput = 1;
        Macro::define(&m->macrotable, (const utf8_t *)"COPYRIGHT", 9, dc->copyright->body, dc->copyright->bodylen);
    }

    buf.printf("$(DDOC_COMMENT Generated by Ddoc from %s)\n", m->srcfile->toChars());
    if (m->isDocFile)
    {
        Loc loc = m->md ? m->md->loc : m->loc;
        size_t commentlen = strlen((const char *)m->comment);
        Dsymbols a;
        // Bugzilla 9764: Don't push m in a, to prevent emphasize ddoc file name.
        if (dc->macros)
        {
            commentlen = dc->macros->name - m->comment;
            dc->macros->write(loc, dc, sc, &a, &buf);
        }
        buf.write(m->comment, commentlen);
        highlightText(sc, &a, &buf, 0);
    }
    else
    {
        Dsymbols a;
        a.push(m);
        dc->writeSections(sc, &a, &buf);
        emitMemberComments(m, &buf, sc);
    }

    //printf("BODY= '%.*s'\n", buf.length(), buf.slice().ptr);
    Macro::define(&m->macrotable, (const utf8_t *)"BODY", 4, (const utf8_t *)buf.slice().ptr, buf.length());

    OutBuffer buf2;
    buf2.writestring("$(DDOC)\n");
    size_t end = buf2.length();
    m->macrotable->expand(&buf2, 0, &end, NULL, 0);

    /* Remove all the escape sequences from buf2,
     * and make CR-LF the newline.
     */
    {
        buf.setsize(0);
        buf.reserve(buf2.length());
        utf8_t *p = (utf8_t *)buf2.slice().ptr;
        for (size_t j = 0; j < buf2.length(); j++)
        {
            utf8_t c = p[j];
            if (c == 0xFF && j + 1 < buf2.length())
            {
                j++;
                continue;
            }
            if (c == '\n')
                buf.writeByte('\r');
            else if (c == '\r')
            {
                buf.writestring("\r\n");
                if (j + 1 < buf2.length() && p[j + 1] == '\n')
                {
                    j++;
                }
                continue;
            }
            buf.writeByte(c);
        }
    }

    // Transfer image to file
    assert(m->docfile);
    m->docfile->setbuffer(buf.slice().ptr, buf.length());
    m->docfile->ref = 1;
    ensurePathToNameExists(Loc(), m->docfile->toChars());
    writeFile(m->loc, m->docfile);
}

/****************************************************
 * Having unmatched parentheses can hose the output of Ddoc,
 * as the macros depend on properly nested parentheses.
 * This function replaces all ( with $(LPAREN) and ) with $(RPAREN)
 * to preserve text literally. This also means macros in the
 * text won't be expanded.
 */
void escapeDdocString(OutBuffer *buf, size_t start)
{
    for (size_t u = start; u < buf->length(); u++)
    {
        utf8_t c = buf->slice().ptr[u];
        switch(c)
        {
            case '$':
                buf->remove(u, 1);
                buf->insert(u, (const char *)"$(DOLLAR)", 9);
                u += 8;
                break;

            case '(':
                buf->remove(u, 1); //remove the (
                buf->insert(u, (const char *)"$(LPAREN)", 9); //insert this instead
                u += 8; //skip over newly inserted macro
                break;

            case ')':
                buf->remove(u, 1); //remove the )
                buf->insert(u, (const char *)"$(RPAREN)", 9); //insert this instead
                u += 8; //skip over newly inserted macro
                break;
        }
    }
}

/****************************************************
 * Having unmatched parentheses can hose the output of Ddoc,
 * as the macros depend on properly nested parentheses.

 * Fix by replacing unmatched ( with $(LPAREN) and unmatched ) with $(RPAREN).
 */
void escapeStrayParenthesis(Loc loc, OutBuffer *buf, size_t start)
{
    unsigned par_open = 0;

    for (size_t u = start; u < buf->length(); u++)
    {
        utf8_t c = buf->slice().ptr[u];
        switch(c)
        {
            case '(':
                par_open++;
                break;

            case ')':
                if (par_open == 0)
                {
                    //stray ')'
                    warning(loc, "Ddoc: Stray ')'. This may cause incorrect Ddoc output."
                        " Use $(RPAREN) instead for unpaired right parentheses.");
                    buf->remove(u, 1); //remove the )
                    buf->insert(u, (const char *)"$(RPAREN)", 9); //insert this instead
                    u += 8; //skip over newly inserted macro
                }
                else
                    par_open--;
                break;
        }
    }

    if (par_open)                       // if any unmatched lparens
    {
        par_open = 0;
        for (size_t u = buf->length(); u > start;)
        {
            u--;
            utf8_t c = buf->slice().ptr[u];
            switch(c)
            {
                case ')':
                    par_open++;
                    break;

                case '(':
                    if (par_open == 0)
                    {
                        //stray '('
                        warning(loc, "Ddoc: Stray '('. This may cause incorrect Ddoc output."
                            " Use $(LPAREN) instead for unpaired left parentheses.");
                        buf->remove(u, 1); //remove the (
                        buf->insert(u, (const char *)"$(LPAREN)", 9); //insert this instead
                    }
                    else
                        par_open--;
                    break;
            }
        }
    }
}

// Basically, this is to skip over things like private{} blocks in a struct or
// class definition that don't add any components to the qualified name.
static Scope *skipNonQualScopes(Scope *sc)
{
    while (sc && !sc->scopesym)
        sc = sc->enclosing;
    return sc;
}

static bool emitAnchorName(OutBuffer *buf, Dsymbol *s, Scope *sc)
{
    if (!s || s->isPackage() || s->isModule())
        return false;

    // Add parent names first
    bool dot = false;
    if (s->parent)
        dot = emitAnchorName(buf, s->parent, sc);
    else if (sc)
        dot = emitAnchorName(buf, sc->scopesym, skipNonQualScopes(sc->enclosing));

    // Eponymous template members can share the parent anchor name
    if (getEponymousParent(s))
        return dot;
    if (dot)
        buf->writeByte('.');

    // Use "this" not "__ctor"
    TemplateDeclaration *td;
    if (s->isCtorDeclaration() || ((td = s->isTemplateDeclaration()) != NULL &&
        td->onemember && td->onemember->isCtorDeclaration()))
    {
        buf->writestring("this");
    }
    else
    {
        /* We just want the identifier, not overloads like TemplateDeclaration::toChars.
         * We don't want the template parameter list and constraints. */
        buf->writestring(s->Dsymbol::toChars());
    }
    return true;
}

static void emitAnchor(OutBuffer *buf, Dsymbol *s, Scope *sc)
{
    Identifier *ident;
    {
        OutBuffer anc;
        emitAnchorName(&anc, s, skipNonQualScopes(sc));
        ident = Identifier::idPool(anc.peekChars());
    }
    size_t *count = (size_t*)dmd_aaGet(&sc->anchorCounts, (void *)ident);
    TemplateDeclaration *td = getEponymousParent(s);
    // don't write an anchor for matching consecutive ditto symbols
    if (*count > 0 && sc->prevAnchor == ident &&
        sc->lastdc && (isDitto(s->comment) || (td && isDitto(td->comment))))
        return;

    (*count)++;
    // cache anchor name
    sc->prevAnchor = ident;

    buf->writestring("$(DDOC_ANCHOR ");
    buf->writestring(ident->toChars());
    // only append count once there's a duplicate
    if (*count != 1)
        buf->printf(".%u", *count);
    buf->writeByte(')');
}

/******************************* emitComment **********************************/

/** Get leading indentation from 'src' which represents lines of code. */
static size_t getCodeIndent(const char *src)
{
    while (src && (*src == '\r' || *src == '\n'))
        ++src;  // skip until we find the first non-empty line

    size_t codeIndent = 0;
    while (src && (*src == ' ' || *src == '\t'))
    {
        codeIndent++;
        src++;
    }
    return codeIndent;
}

/** Recursively expand template mixin member docs into the scope. */
static void expandTemplateMixinComments(TemplateMixin *tm, OutBuffer *buf, Scope *sc)
{
    if (!tm->semanticRun) tm->semantic(sc);
    TemplateDeclaration *td = (tm && tm->tempdecl) ?
        tm->tempdecl->isTemplateDeclaration() : NULL;
    if (td && td->members)
    {
        for (size_t i = 0; i < td->members->length; i++)
        {
            Dsymbol *sm = (*td->members)[i];
            TemplateMixin *tmc = sm->isTemplateMixin();
            if (tmc && tmc->comment)
                expandTemplateMixinComments(tmc, buf, sc);
            else
                emitComment(sm, buf, sc);
        }
    }
}

void emitMemberComments(ScopeDsymbol *sds, OutBuffer *buf, Scope *sc)
{
    if (!sds->members)
        return;

    //printf("ScopeDsymbol::emitMemberComments() %s\n", toChars());

    const char *m = "$(DDOC_MEMBERS ";
    if (sds->isTemplateDeclaration())
        m = "$(DDOC_TEMPLATE_MEMBERS ";
    else if (sds->isClassDeclaration())
        m = "$(DDOC_CLASS_MEMBERS ";
    else if (sds->isStructDeclaration())
        m = "$(DDOC_STRUCT_MEMBERS ";
    else if (sds->isEnumDeclaration())
        m = "$(DDOC_ENUM_MEMBERS ";
    else if (sds->isModule())
        m = "$(DDOC_MODULE_MEMBERS ";

    size_t offset1 = buf->length();         // save starting offset
    buf->writestring(m);
    size_t offset2 = buf->length();         // to see if we write anything

    sc = sc->push(sds);

    for (size_t i = 0; i < sds->members->length; i++)
    {
        Dsymbol *s = (*sds->members)[i];
        //printf("\ts = '%s'\n", s->toChars());

        // only expand if parent is a non-template (semantic won't work)
        if (s->comment && s->isTemplateMixin() && s->parent && !s->parent->isTemplateDeclaration())
            expandTemplateMixinComments((TemplateMixin *)s, buf, sc);

        emitComment(s, buf, sc);
    }
    emitComment(NULL, buf, sc);

    sc->pop();

    if (buf->length() == offset2)
    {
        /* Didn't write out any members, so back out last write
         */
        buf->setsize(offset1);
    }
    else
        buf->writestring(")\n");
}

void emitProtection(OutBuffer *buf, Prot prot)
{
    if (prot.kind != Prot::undefined && prot.kind != Prot::public_)
    {
        protectionToBuffer(buf, prot);
        buf->writeByte(' ');
    }
}

void emitComment(Dsymbol *s, OutBuffer *buf, Scope *sc)
{
    class EmitComment : public Visitor
    {
    public:
        OutBuffer *buf;
        Scope *sc;

        EmitComment(OutBuffer *buf, Scope *sc)
            : buf(buf), sc(sc)
        {
        }

        void visit(Dsymbol *) {}
        void visit(InvariantDeclaration *) {}
        void visit(UnitTestDeclaration *) {}
        void visit(PostBlitDeclaration *) {}
        void visit(DtorDeclaration *) {}
        void visit(StaticCtorDeclaration *) {}
        void visit(StaticDtorDeclaration *) {}
        void visit(TypeInfoDeclaration *) {}

        void emit(Scope *sc, Dsymbol *s, const utf8_t *com)
        {
            if (s && sc->lastdc && isDitto(com))
            {
                sc->lastdc->a.push(s);
                return;
            }

            // Put previous doc comment if exists
            if (DocComment *dc = sc->lastdc)
            {
                // Put the declaration signatures as the document 'title'
                buf->writestring(ddoc_decl_s);
                for (size_t i = 0; i < dc->a.length; i++)
                {
                    Dsymbol *sx = dc->a[i];

                    if (i == 0)
                    {
                        size_t o = buf->length();
                        toDocBuffer(sx, buf, sc);
                        highlightCode(sc, sx, buf, o);
                        continue;
                    }

                    buf->writestring("$(DDOC_DITTO ");
                    {
                        size_t o = buf->length();
                        toDocBuffer(sx, buf, sc);
                        highlightCode(sc, sx, buf, o);
                    }
                    buf->writeByte(')');
                }
                buf->writestring(ddoc_decl_e);

                // Put the ddoc comment as the document 'description'
                buf->writestring(ddoc_decl_dd_s);
                {
                    dc->writeSections(sc, &dc->a, buf);
                    if (ScopeDsymbol *sds = dc->a[0]->isScopeDsymbol())
                        emitMemberComments(sds, buf, sc);
                }
                buf->writestring(ddoc_decl_dd_e);
                //printf("buf.2 = [[%.*s]]\n", buf->length() - o0, buf->slice().ptr + o0);
            }

            if (s)
            {
                DocComment *dc = DocComment::parse(s, com);
                dc->pmacrotable = &sc->_module->macrotable;
                sc->lastdc = dc;
            }
        }

        void visit(Declaration *d)
        {
            //printf("Declaration::emitComment(%p '%s'), comment = '%s'\n", d, d->toChars(), d->comment);
            //printf("type = %p\n", d->type);
            const utf8_t *com = d->comment;
            if (TemplateDeclaration *td = getEponymousParent(d))
            {
                if (isDitto(td->comment))
                    com = td->comment;
                else
                    com = Lexer::combineComments(td->comment, com);
            }
            else
            {
                if (!d->ident)
                    return;
                if (!d->type && !d->isCtorDeclaration() && !d->isAliasDeclaration())
                    return;
                if (d->protection.kind == Prot::private_ || sc->protection.kind == Prot::private_)
                    return;
            }
            if (!com)
                return;

            emit(sc, d, com);
        }

        void visit(AggregateDeclaration *ad)
        {
            //printf("AggregateDeclaration::emitComment() '%s'\n", ad->toChars());
            const utf8_t *com = ad->comment;
            if (TemplateDeclaration *td = getEponymousParent(ad))
            {
                if (isDitto(td->comment))
                    com = td->comment;
                else
                    com = Lexer::combineComments(td->comment, com);
            }
            else
            {
                if (ad->prot().kind == Prot::private_ || sc->protection.kind == Prot::private_)
                    return;
                if (!ad->comment)
                    return;
            }
            if (!com)
                return;

            emit(sc, ad, com);
        }

        void visit(TemplateDeclaration *td)
        {
            //printf("TemplateDeclaration::emitComment() '%s', kind = %s\n", td->toChars(), td->kind());
            if (td->prot().kind == Prot::private_ || sc->protection.kind == Prot::private_)
                return;
            if (!td->comment)
                return;

            if (Dsymbol *ss = getEponymousMember(td))
            {
                ss->accept(this);
                return;
            }
            emit(sc, td, td->comment);
        }

        void visit(EnumDeclaration *ed)
        {
            if (ed->prot().kind == Prot::private_ || sc->protection.kind == Prot::private_)
                return;
            if (ed->isAnonymous() && ed->members)
            {
                for (size_t i = 0; i < ed->members->length; i++)
                {
                    Dsymbol *s = (*ed->members)[i];
                    emitComment(s, buf, sc);
                }
                return;
            }
            if (!ed->comment)
                return;
            if (ed->isAnonymous())
                return;

            emit(sc, ed, ed->comment);
        }

        void visit(EnumMember *em)
        {
            //printf("EnumMember::emitComment(%p '%s'), comment = '%s'\n", em, em->toChars(), em->comment);
            if (em->prot().kind == Prot::private_ || sc->protection.kind == Prot::private_)
                return;
            if (!em->comment)
                return;

            emit(sc, em, em->comment);
        }

        void visit(AttribDeclaration *ad)
        {
            //printf("AttribDeclaration::emitComment(sc = %p)\n", sc);

            /* A general problem with this, illustrated by BUGZILLA 2516,
             * is that attributes are not transmitted through to the underlying
             * member declarations for template bodies, because semantic analysis
             * is not done for template declaration bodies
             * (only template instantiations).
             * Hence, Ddoc omits attributes from template members.
             */

            Dsymbols *d = ad->include(NULL);

            if (d)
            {
                for (size_t i = 0; i < d->length; i++)
                {
                    Dsymbol *s = (*d)[i];
                    //printf("AttribDeclaration::emitComment %s\n", s->toChars());
                    emitComment(s, buf, sc);
                }
            }
        }

        void visit(ProtDeclaration *pd)
        {
            if (pd->decl)
            {
                Scope *scx = sc;
                sc = sc->copy();
                sc->protection = pd->protection;
                visit((AttribDeclaration *)pd);
                scx->lastdc = sc->lastdc;
                sc = sc->pop();
            }
        }

        void visit(ConditionalDeclaration *cd)
        {
            //printf("ConditionalDeclaration::emitComment(sc = %p)\n", sc);
            if (cd->condition->inc)
            {
                visit((AttribDeclaration *)cd);
                return;
            }

            /* If generating doc comment, be careful because if we're inside
             * a template, then include(NULL) will fail.
             */
            Dsymbols *d = cd->decl ? cd->decl : cd->elsedecl;
            for (size_t i = 0; i < d->length; i++)
            {
                Dsymbol *s = (*d)[i];
                emitComment(s, buf, sc);
            }
        }
    };

    EmitComment v(buf, sc);

    if (!s)
        v.emit(sc, NULL, NULL);
    else
        s->accept(&v);
}

/******************************* toDocBuffer **********************************/

void toDocBuffer(Dsymbol *s, OutBuffer *buf, Scope *sc)
{
    class ToDocBuffer : public Visitor
    {
    public:
        OutBuffer *buf;
        Scope *sc;

        ToDocBuffer(OutBuffer *buf, Scope *sc)
            : buf(buf), sc(sc)
        {
        }

        void visit(Dsymbol *s)
        {
            //printf("Dsymbol::toDocbuffer() %s\n", s->toChars());
            HdrGenState hgs;
            hgs.ddoc = true;
            ::toCBuffer(s, buf, &hgs);
        }

        void prefix(Dsymbol *s)
        {
            if (s->isDeprecated())
                buf->writestring("deprecated ");

            if (Declaration *d = s->isDeclaration())
            {
                emitProtection(buf, d->protection);

                if (d->isStatic())
                    buf->writestring("static ");
                else if (d->isFinal())
                    buf->writestring("final ");
                else if (d->isAbstract())
                    buf->writestring("abstract ");

                if (!d->isFuncDeclaration())  // functionToBufferFull handles this
                {
                    if (d->isConst())
                        buf->writestring("const ");
                    if (d->isImmutable())
                        buf->writestring("immutable ");
                    if (d->isSynchronized())
                        buf->writestring("synchronized ");

                    if (d->storage_class & STCmanifest)
                        buf->writestring("enum ");
                }
            }
        }

        void visit(Declaration *d)
        {
            if (!d->ident)
                return;

            TemplateDeclaration *td = getEponymousParent(d);
            //printf("Declaration::toDocbuffer() %s, originalType = %s, td = %s\n", d->toChars(), d->originalType ? d->originalType->toChars() : "--", td ? td->toChars() : "--");

            HdrGenState hgs;
            hgs.ddoc = true;

            if (d->isDeprecated())
                buf->writestring("$(DEPRECATED ");

            prefix(d);

            if (d->type)
            {
                Type *origType = d->originalType ? d->originalType : d->type;
                if (origType->ty == Tfunction)
                {
                    functionToBufferFull((TypeFunction *)origType, buf, d->ident, &hgs, td);
                }
                else
                    ::toCBuffer(origType, buf, d->ident, &hgs);
            }
            else
                buf->writestring(d->ident->toChars());

            if (d->isVarDeclaration() && td)
            {
                buf->writeByte('(');
                if (td->origParameters && td->origParameters->length)
                {
                    for (size_t i = 0; i < td->origParameters->length; i++)
                    {
                        if (i)
                            buf->writestring(", ");
                        toCBuffer((*td->origParameters)[i], buf, &hgs);
                    }
                }
                buf->writeByte(')');
            }

            // emit constraints if declaration is a templated declaration
            if (td && td->constraint)
            {
                buf->writestring(" if (");
                ::toCBuffer(td->constraint, buf, &hgs);
                buf->writeByte(')');
            }

            if (d->isDeprecated())
                buf->writestring(")");

            buf->writestring(";\n");
        }

        void visit(AliasDeclaration *ad)
        {
            //printf("AliasDeclaration::toDocbuffer() %s\n", ad->toChars());
            if (!ad->ident)
                return;

            if (ad->isDeprecated())
                buf->writestring("deprecated ");

            emitProtection(buf, ad->protection);
            buf->printf("alias %s = ", ad->toChars());

            if (Dsymbol *s = ad->aliassym)  // ident alias
            {
                prettyPrintDsymbol(s, ad->parent);
            }
            else if (Type *type = ad->getType())  // type alias
            {
                if (type->ty == Tclass || type->ty == Tstruct || type->ty == Tenum)
                {
                    if (Dsymbol *s = type->toDsymbol(NULL))  // elaborate type
                        prettyPrintDsymbol(s, ad->parent);
                    else
                        buf->writestring(type->toChars());
                }
                else
                {
                    // simple type
                    buf->writestring(type->toChars());
                }
            }

            buf->writestring(";\n");
        }

        void parentToBuffer(Dsymbol *s)
        {
            if (s && !s->isPackage() && !s->isModule())
            {
                parentToBuffer(s->parent);
                buf->writestring(s->toChars());
                buf->writestring(".");
            }
        }

        static bool inSameModule(Dsymbol *s, Dsymbol *p)
        {
            for ( ; s ; s = s->parent)
            {
                if (s->isModule())
                    break;
            }

            for ( ; p ; p = p->parent)
            {
                if (p->isModule())
                    break;
            }

            return s == p;
        }

        void prettyPrintDsymbol(Dsymbol *s, Dsymbol *parent)
        {
            if (s->parent && (s->parent == parent))  // in current scope -> naked name
            {
                buf->writestring(s->toChars());
            }
            else if (!inSameModule(s, parent))  // in another module -> full name
            {
                buf->writestring(s->toPrettyChars());
            }
            else  // nested in a type in this module -> full name w/o module name
            {
                // if alias is nested in a user-type use module-scope lookup
                if (!parent->isModule() && !parent->isPackage())
                    buf->writestring(".");

                parentToBuffer(s->parent);
                buf->writestring(s->toChars());
            }
        }

        void visit(AggregateDeclaration *ad)
        {
            if (!ad->ident)
                return;

            buf->printf("%s %s", ad->kind(), ad->toChars());
            buf->writestring(";\n");
        }

        void visit(StructDeclaration *sd)
        {
            //printf("StructDeclaration::toDocbuffer() %s\n", sd->toChars());
            if (!sd->ident)
                return;

            if (TemplateDeclaration *td = getEponymousParent(sd))
            {
                toDocBuffer(td, buf, sc);
            }
            else
            {
                buf->printf("%s %s", sd->kind(), sd->toChars());
            }
            buf->writestring(";\n");
        }

        void visit(ClassDeclaration *cd)
        {
            //printf("ClassDeclaration::toDocbuffer() %s\n", cd->toChars());
            if (!cd->ident)
                return;

            if (TemplateDeclaration *td = getEponymousParent(cd))
            {
                toDocBuffer(td, buf, sc);
            }
            else
            {
                if (!cd->isInterfaceDeclaration() && cd->isAbstract())
                    buf->writestring("abstract ");
                buf->printf("%s %s", cd->kind(), cd->toChars());
            }
            int any = 0;
            for (size_t i = 0; i < cd->baseclasses->length; i++)
            {
                BaseClass *bc = (*cd->baseclasses)[i];

                if (bc->sym && bc->sym->ident == Id::Object)
                    continue;

                if (any)
                    buf->writestring(", ");
                else
                {
                    buf->writestring(": ");
                    any = 1;
                }
                emitProtection(buf, Prot(Prot::public_));
                if (bc->sym)
                {
                    buf->printf("$(DDOC_PSUPER_SYMBOL %s)", bc->sym->toPrettyChars());
                }
                else
                {
                    HdrGenState hgs;
                    ::toCBuffer(bc->type, buf, NULL, &hgs);
                }
            }
            buf->writestring(";\n");
        }

        void visit(EnumDeclaration *ed)
        {
            if (!ed->ident)
                return;

            buf->printf("%s %s", ed->kind(), ed->toChars());
            if (ed->memtype)
            {
                buf->writestring(": $(DDOC_ENUM_BASETYPE ");
                HdrGenState hgs;
                ::toCBuffer(ed->memtype, buf, NULL, &hgs);
                buf->writestring(")");
            }
            buf->writestring(";\n");
        }

        void visit(EnumMember *em)
        {
            if (!em->ident)
                return;

            buf->writestring(em->toChars());
        }
    };

    ToDocBuffer v(buf, sc);
    s->accept(&v);
}

/********************************* DocComment *********************************/

DocComment *DocComment::parse(Dsymbol *s, const utf8_t *comment)
{
    //printf("parse(%s): '%s'\n", s->toChars(), comment);
    DocComment *dc = new DocComment();
    dc->a.push(s);
    if (!comment)
        return dc;

    dc->parseSections(comment);

    for (size_t i = 0; i < dc->sections.length; i++)
    {
        Section *sec = dc->sections[i];

        if (icmp("copyright", sec->name, sec->namelen) == 0)
        {
            dc->copyright = sec;
        }
        if (icmp("macros", sec->name, sec->namelen) == 0)
        {
            dc->macros = sec;
        }
    }

    return dc;
}

/*****************************************
 * Parse next paragraph out of *pcomment.
 * Update *pcomment to point past paragraph.
 * Returns NULL if no more paragraphs.
 * If paragraph ends in 'identifier:',
 * then (*pcomment)[0 .. idlen] is the identifier.
 */

void DocComment::parseSections(const utf8_t *comment)
{
    const utf8_t *p;
    const utf8_t *pstart;
    const utf8_t *pend;
    const utf8_t *idstart = NULL;       // dead-store to prevent spurious warning
    size_t idlen;

    const utf8_t *name = NULL;
    size_t namelen = 0;

    //printf("parseSections('%s')\n", comment);
    p = comment;
    while (*p)
    {
        const utf8_t *pstart0 = p;
        p = skipwhitespace(p);
        pstart = p;
        pend = p;

        /* Find end of section, which is ended by one of:
         *      'identifier:' (but not inside a code section)
         *      '\0'
         */
        idlen = 0;
        int inCode = 0;
        while (1)
        {
            // Check for start/end of a code section
            if (*p == '-')
            {
                if (!inCode)
                {
                    // restore leading indentation
                    while (pstart0 < pstart && isIndentWS(pstart-1)) --pstart;
                }

                int numdash = 0;
                while (*p == '-')
                {
                    ++numdash;
                    p++;
                }
                // BUG: handle UTF PS and LS too
                if ((!*p || *p == '\r' || *p == '\n') && numdash >= 3)
                    inCode ^= 1;
                pend = p;
            }

            if (!inCode && isIdStart(p))
            {
                const utf8_t *q = p + utfStride(p);
                while (isIdTail(q))
                    q += utfStride(q);
                // Detected tag ends it
                if (*q == ':' && isupper(*p)
                        && (isspace(q[1]) || q[1] == 0))
                {
                    idlen = q - p;
                    idstart = p;
                    for (pend = p; pend > pstart; pend--)
                    {
                        if (pend[-1] == '\n')
                            break;
                    }
                    p = q + 1;
                    break;
                }
            }
            while (1)
            {
                if (!*p)
                    goto L1;
                if (*p == '\n')
                {
                    p++;
                    if (*p == '\n' && !summary && !namelen && !inCode)
                    {
                        pend = p;
                        p++;
                        goto L1;
                    }
                    break;
                }
                p++;
                pend = p;
            }
            p = skipwhitespace(p);
        }
      L1:

        if (namelen || pstart < pend)
        {
            Section *s;
            if (icmp("Params", name, namelen) == 0)
                s = new ParamSection();
            else if (icmp("Macros", name, namelen) == 0)
                s = new MacroSection();
            else
                s = new Section();
            s->name = name;
            s->namelen = namelen;
            s->body = pstart;
            s->bodylen = pend - pstart;
            s->nooutput = 0;

            //printf("Section: '%.*s' = '%.*s'\n", s->namelen, s->name, s->bodylen, s->body);

            sections.push(s);

            if (!summary && !namelen)
                summary = s;
        }

        if (idlen)
        {
            name = idstart;
            namelen = idlen;
        }
        else
        {
            name = NULL;
            namelen = 0;
            if (!*p)
                break;
        }
    }
}

void DocComment::writeSections(Scope *sc, Dsymbols *a, OutBuffer *buf)
{
    assert(a->length);

    //printf("DocComment::writeSections()\n");
    Loc loc = (*a)[0]->loc;
    if (Module *m = (*a)[0]->isModule())
    {
        if (m->md)
            loc = m->md->loc;
    }

    size_t offset1 = buf->length();
    buf->writestring("$(DDOC_SECTIONS ");
    size_t offset2 = buf->length();

    for (size_t i = 0; i < sections.length; i++)
    {
        Section *sec = sections[i];
        if (sec->nooutput)
            continue;

        //printf("Section: '%.*s' = '%.*s'\n", sec->namelen, sec->name, sec->bodylen, sec->body);
        if (!sec->namelen && i == 0)
        {
            buf->writestring("$(DDOC_SUMMARY ");
            size_t o = buf->length();
            buf->write(sec->body, sec->bodylen);
            escapeStrayParenthesis(loc, buf, o);
            highlightText(sc, a, buf, o);
            buf->writestring(")\n");
        }
        else
            sec->write(loc, this, sc, a, buf);
    }

    for (size_t i = 0; i < a->length; i++)
    {
        Dsymbol *s = (*a)[i];
        if (Dsymbol *td = getEponymousParent(s))
            s = td;

        for (UnitTestDeclaration *utd = s->ddocUnittest; utd; utd = utd->ddocUnittest)
        {
            if (utd->protection.kind == Prot::private_ || !utd->comment || !utd->fbody)
                continue;

            // Strip whitespaces to avoid showing empty summary
            const utf8_t *c = utd->comment;
            while (*c == ' ' || *c == '\t' || *c == '\n' || *c == '\r') ++c;

            buf->writestring("$(DDOC_EXAMPLES ");

            size_t o = buf->length();
            buf->writestring((const char *)c);

            if (utd->codedoc)
            {
                size_t n = getCodeIndent(utd->codedoc);
                while (n--) buf->writeByte(' ');
                buf->writestring("----\n");
                buf->writestring(utd->codedoc);
                buf->writestring("----\n");
                highlightText(sc, a, buf, o);
            }

            buf->writestring(")");
        }
    }

    if (buf->length() == offset2)
    {
        /* Didn't write out any sections, so back out last write
         */
        buf->setsize(offset1);
        buf->writestring("$(DDOC_BLANKLINE)\n");
    }
    else
        buf->writestring(")\n");
}

/***************************************************
 */

void Section::write(Loc loc, DocComment *, Scope *sc, Dsymbols *a, OutBuffer *buf)
{
    assert(a->length);

    if (namelen)
    {
        static const char *table[] =
        {
            "AUTHORS", "BUGS", "COPYRIGHT", "DATE",
            "DEPRECATED", "EXAMPLES", "HISTORY", "LICENSE",
            "RETURNS", "SEE_ALSO", "STANDARDS", "THROWS",
            "VERSION", NULL
        };

        for (size_t i = 0; table[i]; i++)
        {
            if (icmp(table[i], name, namelen) == 0)
            {
                buf->printf("$(DDOC_%s ", table[i]);
                goto L1;
            }
        }

        buf->writestring("$(DDOC_SECTION ");

            // Replace _ characters with spaces
            buf->writestring("$(DDOC_SECTION_H ");
            size_t o = buf->length();
            for (size_t u = 0; u < namelen; u++)
            {
                utf8_t c = name[u];
                buf->writeByte((c == '_') ? ' ' : c);
            }
            escapeStrayParenthesis(loc, buf, o);
            buf->writestring(":)\n");
    }
    else
    {
        buf->writestring("$(DDOC_DESCRIPTION ");
    }
  L1:
    size_t o = buf->length();
    buf->write(body, bodylen);
    escapeStrayParenthesis(loc, buf, o);
    highlightText(sc, a, buf, o);
    buf->writestring(")\n");
}

/***************************************************
 */

void ParamSection::write(Loc loc, DocComment *, Scope *sc, Dsymbols *a, OutBuffer *buf)
{
    assert(a->length);
    Dsymbol *s = (*a)[0];   // test

    const utf8_t *p = body;
    size_t len = bodylen;
    const utf8_t *pend = p + len;

    const utf8_t *tempstart = NULL;
    size_t templen = 0;

    const utf8_t *namestart = NULL;
    size_t namelen = 0;       // !=0 if line continuation

    const utf8_t *textstart = NULL;
    size_t textlen = 0;

    size_t paramcount = 0;

    buf->writestring("$(DDOC_PARAMS ");
    while (p < pend)
    {
        // Skip to start of macro
        while (1)
        {
            switch (*p)
            {
                case ' ':
                case '\t':
                    p++;
                    continue;

                case '\n':
                    p++;
                    goto Lcont;

                default:
                    if (isIdStart(p) || isCVariadicArg(p, pend - p))
                        break;
                    if (namelen)
                        goto Ltext;             // continuation of prev macro
                    goto Lskipline;
            }
            break;
        }
        tempstart = p;

        while (isIdTail(p))
            p += utfStride(p);
        if (isCVariadicArg(p, pend - p))
            p += 3;

        templen = p - tempstart;

        while (*p == ' ' || *p == '\t')
            p++;

        if (*p != '=')
        {
            if (namelen)
                goto Ltext;             // continuation of prev macro
            goto Lskipline;
        }
        p++;

        if (namelen)
        {
            // Output existing param

        L1:
            //printf("param '%.*s' = '%.*s'\n", namelen, namestart, textlen, textstart);
            ++paramcount;
            HdrGenState hgs;
            buf->writestring("$(DDOC_PARAM_ROW ");
            {
                buf->writestring("$(DDOC_PARAM_ID ");
                {
                    size_t o = buf->length();
                    Parameter *fparam = isFunctionParameter(a, namestart, namelen);
                    if (!fparam)
                    {
                        // Comments on a template might refer to function parameters within.
                        // Search the parameters of nested eponymous functions (with the same name.)
                        fparam = isEponymousFunctionParameter(a, namestart, namelen);
                    }
                    bool isCVariadic = isCVariadicParameter(a, namestart, namelen);
                    if (isCVariadic)
                    {
                        buf->writestring("...");
                    }
                    else if (fparam && fparam->type && fparam->ident)
                    {
                        ::toCBuffer(fparam->type, buf, fparam->ident, &hgs);
                    }
                    else
                    {
                        if (isTemplateParameter(a, namestart, namelen))
                        {
                            // 10236: Don't count template parameters for params check
                            --paramcount;
                        }
                        else if (!fparam)
                        {
                            warning(s->loc, "Ddoc: function declaration has no parameter '%.*s'", (int)namelen, namestart);
                        }
                        buf->write(namestart, namelen);
                    }
                    escapeStrayParenthesis(loc, buf, o);
                    highlightCode(sc, a, buf, o);
                }
                buf->writestring(")\n");

                buf->writestring("$(DDOC_PARAM_DESC ");
                {
                    size_t o = buf->length();
                    buf->write(textstart, textlen);
                    escapeStrayParenthesis(loc, buf, o);
                    highlightText(sc, a, buf, o);
                }
                buf->writestring(")");
            }
            buf->writestring(")\n");
            namelen = 0;
            if (p >= pend)
                break;
        }

        namestart = tempstart;
        namelen = templen;

        while (*p == ' ' || *p == '\t')
            p++;
        textstart = p;

      Ltext:
        while (*p != '\n')
            p++;
        textlen = p - textstart;
        p++;

     Lcont:
        continue;

     Lskipline:
        // Ignore this line
        while (*p++ != '\n')
            ;
    }
    if (namelen)
        goto L1;                // write out last one
    buf->writestring(")\n");

    TypeFunction *tf = a->length == 1 ? isTypeFunction(s) : NULL;
    if (tf)
    {
        size_t pcount = (tf->parameterList.parameters ? tf->parameterList.parameters->length : 0) +
                        (int)(tf->parameterList.varargs == VARARGvariadic);
        if (pcount != paramcount)
        {
            warning(s->loc, "Ddoc: parameter count mismatch");
        }
    }
}

/***************************************************
 */

void MacroSection::write(Loc, DocComment *dc, Scope *, Dsymbols *, OutBuffer *)
{
    //printf("MacroSection::write()\n");
    DocComment::parseMacros(dc->pescapetable, dc->pmacrotable, body, bodylen);
}

/************************************************
 * Parse macros out of Macros: section.
 * Macros are of the form:
 *      name1 = value1
 *
 *      name2 = value2
 */

void DocComment::parseMacros(Escape **pescapetable, Macro **pmacrotable, const utf8_t *m, size_t mlen)
{
    const utf8_t *p = m;
    size_t len = mlen;
    const utf8_t *pend = p + len;

    const utf8_t *tempstart = NULL;
    size_t templen = 0;

    const utf8_t *namestart = NULL;
    size_t namelen = 0;       // !=0 if line continuation

    const utf8_t *textstart = NULL;
    size_t textlen = 0;

    while (p < pend)
    {
        // Skip to start of macro
        while (1)
        {
            if (p >= pend)
                goto Ldone;
            switch (*p)
            {
                case ' ':
                case '\t':
                    p++;
                    continue;

                case '\r':
                case '\n':
                    p++;
                    goto Lcont;

                default:
                    if (isIdStart(p))
                        break;
                    if (namelen)
                        goto Ltext;             // continuation of prev macro
                    goto Lskipline;
            }
            break;
        }
        tempstart = p;

        while (1)
        {
            if (p >= pend)
                goto Ldone;
            if (!isIdTail(p))
                break;
            p += utfStride(p);
        }
        templen = p - tempstart;

        while (1)
        {
            if (p >= pend)
                goto Ldone;
            if (!(*p == ' ' || *p == '\t'))
                break;
            p++;
        }

        if (*p != '=')
        {
            if (namelen)
                goto Ltext;             // continuation of prev macro
            goto Lskipline;
        }
        p++;
        if (p >= pend)
            goto Ldone;

        if (namelen)
        {
            // Output existing macro
        L1:
            //printf("macro '%.*s' = '%.*s'\n", namelen, namestart, textlen, textstart);
            if (icmp("ESCAPES", namestart, namelen) == 0)
                parseEscapes(pescapetable, textstart, textlen);
            else
                Macro::define(pmacrotable, namestart, namelen, textstart, textlen);
            namelen = 0;
            if (p >= pend)
                break;
        }

        namestart = tempstart;
        namelen = templen;

        while (p < pend && (*p == ' ' || *p == '\t'))
            p++;
        textstart = p;

      Ltext:
        while (p < pend && *p != '\r' && *p != '\n')
            p++;
        textlen = p - textstart;

        p++;
        //printf("p = %p, pend = %p\n", p, pend);

     Lcont:
        continue;

     Lskipline:
        // Ignore this line
        while (p < pend && *p != '\r' && *p != '\n')
            p++;
    }
Ldone:
    if (namelen)
        goto L1;                // write out last one
}

/**************************************
 * Parse escapes of the form:
 *      /c/string/
 * where c is a single character.
 * Multiple escapes can be separated
 * by whitespace and/or commas.
 */

void DocComment::parseEscapes(Escape **pescapetable, const utf8_t *textstart, size_t textlen)
{
    Escape *escapetable = *pescapetable;

    if (!escapetable)
    {
        escapetable = new Escape;
        memset(escapetable, 0, sizeof(Escape));
        *pescapetable = escapetable;
    }
    //printf("parseEscapes('%.*s') pescapetable = %p\n", textlen, textstart, pescapetable);
    const utf8_t *p = textstart;
    const utf8_t *pend = p + textlen;

    while (1)
    {
        while (1)
        {
            if (p + 4 >= pend)
                return;
            if (!(*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n' || *p == ','))
                break;
            p++;
        }
        if (p[0] != '/' || p[2] != '/')
            return;
        utf8_t c = p[1];
        p += 3;
        const utf8_t *start = p;
        while (1)
        {
            if (p >= pend)
                return;
            if (*p == '/')
                break;
            p++;
        }
        size_t len = p - start;
        char *s = (char *)memcpy(mem.xmalloc(len + 1), start, len);
        s[len] = 0;
        escapetable->strings[c] = s;
        //printf("\t%c = '%s'\n", c, s);
        p++;
    }
}


/******************************************
 * Compare 0-terminated string with length terminated string.
 * Return < 0, ==0, > 0
 */

int cmp(const char *stringz, const void *s, size_t slen)
{
    size_t len1 = strlen(stringz);

    if (len1 != slen)
        return (int)(len1 - slen);
    return memcmp(stringz, s, slen);
}

int icmp(const char *stringz, const void *s, size_t slen)
{
    size_t len1 = strlen(stringz);

    if (len1 != slen)
        return (int)(len1 - slen);
    return Port::memicmp(stringz, (const char *)s, slen);
}

/*****************************************
 * Return true if comment consists entirely of "ditto".
 */

bool isDitto(const utf8_t *comment)
{
    if (comment)
    {
        const utf8_t *p = skipwhitespace(comment);

        if (Port::memicmp((const char *)p, "ditto", 5) == 0 && *skipwhitespace(p + 5) == 0)
            return true;
    }
    return false;
}

/**********************************************
 * Skip white space.
 */

const utf8_t *skipwhitespace(const utf8_t *p)
{
    for (; 1; p++)
    {
        switch (*p)
        {
            case ' ':
            case '\t':
            case '\n':
                continue;
        }
        break;
    }
    return p;
}


/************************************************
 * Scan forward to one of:
 *      start of identifier
 *      beginning of next line
 *      end of buf
 */

size_t skiptoident(OutBuffer *buf, size_t i)
{
    while (i < buf->length())
    {
        dchar_t c;

        size_t oi = i;
        if (utf_decodeChar((utf8_t *)buf->slice().ptr, buf->length(), &i, &c))
        {
            /* Ignore UTF errors, but still consume input
             */
            break;
        }
        if (c >= 0x80)
        {
            if (!isUniAlpha(c))
                continue;
        }
        else if (!(isalpha(c) || c == '_' || c == '\n'))
            continue;
        i = oi;
        break;
    }
    return i;
}

/************************************************
 * Scan forward past end of identifier.
 */

size_t skippastident(OutBuffer *buf, size_t i)
{
    while (i < buf->length())
    {
        dchar_t c;

        size_t oi = i;
        if (utf_decodeChar((utf8_t *)buf->slice().ptr, buf->length(), &i, &c))
        {
            /* Ignore UTF errors, but still consume input
             */
            break;
        }
        if (c >= 0x80)
        {
            if (isUniAlpha(c))
                continue;
        }
        else if (isalnum(c) || c == '_')
            continue;
        i = oi;
        break;
    }
    return i;
}


/************************************************
 * Scan forward past URL starting at i.
 * We don't want to highlight parts of a URL.
 * Returns:
 *      i if not a URL
 *      index just past it if it is a URL
 */

size_t skippastURL(OutBuffer *buf, size_t i)
{
    size_t length = buf->length() - i;
    utf8_t *p = (utf8_t *)&buf->slice().ptr[i];
    size_t j;
    unsigned sawdot = 0;

    if (length > 7 && Port::memicmp((char *)p, "http://", 7) == 0)
    {
        j = 7;
    }
    else if (length > 8 && Port::memicmp((char *)p, "https://", 8) == 0)
    {
        j = 8;
    }
    else
        goto Lno;

    for (; j < length; j++)
    {
        utf8_t c = p[j];
        if (isalnum(c))
            continue;
        if (c == '-' || c == '_' || c == '?' ||
            c == '=' || c == '%' || c == '&' ||
            c == '/' || c == '+' || c == '#' ||
            c == '~')
            continue;
        if (c == '.')
        {
            sawdot = 1;
            continue;
        }
        break;
    }
    if (sawdot)
        return i + j;

Lno:
    return i;
}


/****************************************************
 */

bool isIdentifier(Dsymbols *a, const utf8_t *p, size_t len)
{
    for (size_t i = 0; i < a->length; i++)
    {
        const char *s = (*a)[i]->ident->toChars();
        if (cmp(s, p, len) == 0)
            return true;
    }
    return false;
}

/****************************************************
 */

bool isKeyword(utf8_t *p, size_t len)
{
    static const char *table[] = { "true", "false", "null", NULL };

    for (int i = 0; table[i]; i++)
    {
        if (cmp(table[i], p, len) == 0)
            return true;
    }
    return false;
}

/****************************************************
 */

TypeFunction *isTypeFunction(Dsymbol *s)
{
    FuncDeclaration *f = s->isFuncDeclaration();

    /* f->type may be NULL for template members.
     */
    if (f && f->type)
    {
        Type *t = f->originalType ? f->originalType : f->type;
        if (t->ty == Tfunction)
            return (TypeFunction *)t;
    }
    return NULL;
}

/****************************************************
 */

Parameter *isFunctionParameter(Dsymbols *a, const utf8_t *p, size_t len)
{
    for (size_t i = 0; i < a->length; i++)
    {
        Parameter *fparam = isFunctionParameter((*a)[i], p, len);
        if (fparam)
        {
            return fparam;
        }
    }
    return NULL;
}

/****************************************************
 */

TemplateParameter *isTemplateParameter(Dsymbols *a, const utf8_t *p, size_t len)
{
    for (size_t i = 0; i < a->length; i++)
    {
        TemplateDeclaration *td = (*a)[i]->isTemplateDeclaration();
        // Check for the parent, if the current symbol is not a template declaration.
        if (!td)
            td = getEponymousParent((*a)[i]);
        if (td && td->origParameters)
        {
            for (size_t k = 0; k < td->origParameters->length; k++)
            {
                TemplateParameter *tp = (*td->origParameters)[k];
                if (tp->ident && cmp(tp->ident->toChars(), p, len) == 0)
                {
                    return tp;
                }
            }
        }
    }
    return NULL;
}

/****************************************************
 * Return true if str is a reserved symbol name
 * that starts with a double underscore.
 */

bool isReservedName(utf8_t *str, size_t len)
{
    static const char *table[] = {
        "__ctor", "__dtor", "__postblit", "__invariant", "__unitTest",
        "__require", "__ensure", "__dollar", "__ctfe", "__withSym", "__result",
        "__returnLabel", "__vptr", "__monitor", "__gate", "__xopEquals", "__xopCmp",
        "__LINE__", "__FILE__", "__MODULE__", "__FUNCTION__", "__PRETTY_FUNCTION__",
        "__DATE__", "__TIME__", "__TIMESTAMP__", "__VENDOR__", "__VERSION__",
        "__EOF__", "__LOCAL_SIZE", "___tls_get_addr", "__entrypoint", NULL };

    for (int i = 0; table[i]; i++)
    {
        if (cmp(table[i], str, len) == 0)
            return true;
    }
    return false;
}

/**************************************************
 * Highlight text section.
 */

void highlightText(Scope *sc, Dsymbols *a, OutBuffer *buf, size_t offset)
{
    Dsymbol *s = a->length ? (*a)[0] : NULL;   // test

    //printf("highlightText()\n");

    int leadingBlank = 1;
    int inCode = 0;
    int inBacktick = 0;
    //int inComment = 0;                  // in <!-- ... --> comment
    size_t iCodeStart = 0;                    // start of code section
    size_t codeIndent = 0;

    size_t iLineStart = offset;

    for (size_t i = offset; i < buf->length(); i++)
    {
        utf8_t c = buf->slice().ptr[i];

     Lcont:
        switch (c)
        {
            case ' ':
            case '\t':
                break;

            case '\n':
                if (inBacktick)
                {
                    // `inline code` is only valid if contained on a single line
                    // otherwise, the backticks should be output literally.
                    //
                    // This lets things like `output from the linker' display
                    // unmolested while keeping the feature consistent with GitHub.

                    inBacktick = false;
                    inCode = false; // the backtick also assumes we're in code

                    // Nothing else is necessary since the DDOC_BACKQUOTED macro is
                    // inserted lazily at the close quote, meaning the rest of the
                    // text is already OK.
                }

                if (!sc->_module->isDocFile &&
                    !inCode && i == iLineStart && i + 1 < buf->length())    // if "\n\n"
                {
                    static const char blankline[] = "$(DDOC_BLANKLINE)\n";

                    i = buf->insert(i, blankline, strlen(blankline));
                }
                leadingBlank = 1;
                iLineStart = i + 1;
                break;

            case '<':
            {
                leadingBlank = 0;
                if (inCode)
                    break;
                utf8_t *p = (utf8_t *)&buf->slice().ptr[i];
                const char *se = sc->_module->escapetable->escapeChar('<');
                if (se && strcmp(se, "&lt;") == 0)
                {
                    // Generating HTML
                    // Skip over comments
                    if (p[1] == '!' && p[2] == '-' && p[3] == '-')
                    {
                        size_t j = i + 4;
                        p += 4;
                        while (1)
                        {
                            if (j == buf->length())
                                goto L1;
                            if (p[0] == '-' && p[1] == '-' && p[2] == '>')
                            {
                                i = j + 2;  // place on closing '>'
                                break;
                            }
                            j++;
                            p++;
                        }
                        break;
                    }

                    // Skip over HTML tag
                    if (isalpha(p[1]) || (p[1] == '/' && isalpha(p[2])))
                    {
                        size_t j = i + 2;
                        p += 2;
                        while (1)
                        {
                            if (j == buf->length())
                                break;
                            if (p[0] == '>')
                            {
                                i = j;      // place on closing '>'
                                break;
                            }
                            j++;
                            p++;
                        }
                        break;
                    }
                }
            L1:
                // Replace '<' with '&lt;' character entity
                if (se)
                {
                    size_t len = strlen(se);
                    buf->remove(i, 1);
                    i = buf->insert(i, se, len);
                    i--;        // point to ';'
                }
                break;
            }
            case '>':
            {
                leadingBlank = 0;
                if (inCode)
                    break;
                // Replace '>' with '&gt;' character entity
                const char *se = sc->_module->escapetable->escapeChar('>');
                if (se)
                {
                    size_t len = strlen(se);
                    buf->remove(i, 1);
                    i = buf->insert(i, se, len);
                    i--;        // point to ';'
                }
                break;
            }
            case '&':
            {
                leadingBlank = 0;
                if (inCode)
                    break;
                utf8_t *p = (utf8_t *)&buf->slice().ptr[i];
                if (p[1] == '#' || isalpha(p[1]))
                    break;                      // already a character entity
                // Replace '&' with '&amp;' character entity
                const char *se = sc->_module->escapetable->escapeChar('&');
                if (se)
                {
                    size_t len = strlen(se);
                    buf->remove(i, 1);
                    i = buf->insert(i, se, len);
                    i--;        // point to ';'
                }
                break;
            }
            case '`':
            {
                if (inBacktick)
                {
                    inBacktick = 0;
                    inCode = 0;

                    OutBuffer codebuf;

                    codebuf.write(buf->slice().ptr + iCodeStart + 1, i - (iCodeStart + 1));

                    // escape the contents, but do not perform highlighting except for DDOC_PSYMBOL
                    highlightCode(sc, a, &codebuf, 0);

                    buf->remove(iCodeStart, i - iCodeStart + 1); // also trimming off the current `

                    static const char pre[] = "$(DDOC_BACKQUOTED ";
                    i = buf->insert(iCodeStart, pre, strlen(pre));
                    i = buf->insert(i, (char *)codebuf.slice().ptr, codebuf.length());
                    i = buf->insert(i, ")", 1);

                    i--; // point to the ending ) so when the for loop does i++, it will see the next character

                    break;
                }

                if (inCode)
                    break;

                inCode = 1;
                inBacktick = 1;
                codeIndent = 0; // inline code is not indented

                // All we do here is set the code flags and record
                // the location. The macro will be inserted lazily
                // so we can easily cancel the inBacktick if we come
                // across a newline character.
                iCodeStart = i;

                break;
            }
            case '-':
                /* A line beginning with --- delimits a code section.
                 * inCode tells us if it is start or end of a code section.
                 */
                if (leadingBlank)
                {
                    size_t istart = i;
                    size_t eollen = 0;

                    leadingBlank = 0;
                    while (1)
                    {
                        ++i;
                        if (i >= buf->length())
                            break;
                        c = buf->slice().ptr[i];
                        if (c == '\n')
                        {
                            eollen = 1;
                            break;
                        }
                        if (c == '\r')
                        {
                            eollen = 1;
                            if (i + 1 >= buf->length())
                                break;
                            if (buf->slice().ptr[i + 1] == '\n')
                            {
                                eollen = 2;
                                break;
                            }
                        }
                        // BUG: handle UTF PS and LS too
                        if (c != '-')
                            goto Lcont;
                    }
                    if (i - istart < 3)
                        goto Lcont;

                    // We have the start/end of a code section

                    // Remove the entire --- line, including blanks and \n
                    buf->remove(iLineStart, i - iLineStart + eollen);
                    i = iLineStart;

                    if (inCode && (i <= iCodeStart))
                    {
                        // Empty code section, just remove it completely.
                        inCode = 0;
                        break;
                    }

                    if (inCode)
                    {
                        inCode = 0;
                        // The code section is from iCodeStart to i
                        OutBuffer codebuf;

                        codebuf.write(buf->slice().ptr + iCodeStart, i - iCodeStart);
                        codebuf.writeByte(0);

                        // Remove leading indentations from all lines
                        bool lineStart = true;
                        utf8_t *endp = (utf8_t *)codebuf.slice().ptr + codebuf.length();
                        for (utf8_t *p = (utf8_t *)codebuf.slice().ptr; p < endp; )
                        {
                            if (lineStart)
                            {
                                size_t j = codeIndent;
                                utf8_t *q = p;
                                while (j-- > 0 && q < endp && isIndentWS(q))
                                    ++q;
                                codebuf.remove(p - (utf8_t *)codebuf.slice().ptr, q - p);
                                assert((utf8_t *)codebuf.slice().ptr <= p);
                                assert(p < (utf8_t *)codebuf.slice().ptr + codebuf.length());
                                lineStart = false;
                                endp = (utf8_t *)codebuf.slice().ptr + codebuf.length(); // update
                                continue;
                            }
                            if (*p == '\n')
                                lineStart = true;
                            ++p;
                        }

                        highlightCode2(sc, a, &codebuf, 0);
                        buf->remove(iCodeStart, i - iCodeStart);
                        i = buf->insert(iCodeStart, codebuf.slice().ptr, codebuf.length());
                        i = buf->insert(i, (const char *)")\n", 2);
                        i -= 2; // in next loop, c should be '\n'
                    }
                    else
                    {
                        static const char d_code[] = "$(D_CODE ";

                        inCode = 1;
                        codeIndent = istart - iLineStart;  // save indent count
                        i = buf->insert(i, d_code, strlen(d_code));
                        iCodeStart = i;
                        i--;            // place i on >
                        leadingBlank = true;
                    }
                }
                break;

            default:
                leadingBlank = 0;
                if (sc->_module->isDocFile || inCode)
                    break;

                utf8_t *start = (utf8_t *)buf->slice().ptr + i;
                if (isIdStart(start))
                {
                    size_t j = skippastident(buf, i);
                    if (i < j)
                    {
                        size_t k = skippastURL(buf, i);
                        if (i < k)
                        {
                            i = k - 1;
                            break;
                        }
                    }
                    else
                        break;
                    size_t len = j - i;

                    // leading '_' means no highlight unless it's a reserved symbol name
                    if (c == '_' &&
                        (i == 0 || !isdigit(*(start - 1))) &&
                        (i == buf->length() - 1 || !isReservedName(start, len)))
                    {
                        buf->remove(i, 1);
                        i = j - 1;
                        break;
                    }
                    if (isIdentifier(a, start, len))
                    {
                        i = buf->bracket(i, "$(DDOC_PSYMBOL ", j, ")") - 1;
                        break;
                    }
                    if (isKeyword(start, len))
                    {
                        i = buf->bracket(i, "$(DDOC_KEYWORD ", j, ")") - 1;
                        break;
                    }
                    if (isFunctionParameter(a, start, len))
                    {
                        //printf("highlighting arg '%s', i = %d, j = %d\n", arg->ident->toChars(), i, j);
                        i = buf->bracket(i, "$(DDOC_PARAM ", j, ")") - 1;
                        break;
                    }

                    i = j - 1;
                }
                break;
        }
    }
    if (inCode)
        error(s ? s->loc : Loc(), "unmatched --- in DDoc comment");
}

/**************************************************
 * Highlight code for DDOC section.
 */

void highlightCode(Scope *sc, Dsymbol *s, OutBuffer *buf, size_t offset)
{
    //printf("highlightCode(s = %s '%s')\n", s->kind(), s->toChars());
    OutBuffer ancbuf;
    emitAnchor(&ancbuf, s, sc);
    buf->insert(offset, (char *)ancbuf.slice().ptr, ancbuf.length());
    offset += ancbuf.length();

    Dsymbols a;
    a.push(s);
    highlightCode(sc, &a, buf, offset);
}

/****************************************************
 */

void highlightCode(Scope *sc, Dsymbols *a, OutBuffer *buf, size_t offset)
{
    //printf("highlightCode(a = '%s')\n", a->toChars());

    for (size_t i = offset; i < buf->length(); i++)
    {
        utf8_t c = buf->slice().ptr[i];
        const char *se = sc->_module->escapetable->escapeChar(c);
        if (se)
        {
            size_t len = strlen(se);
            buf->remove(i, 1);
            i = buf->insert(i, se, len);
            i--;                // point to ';'
            continue;
        }

        utf8_t *start = (utf8_t *)buf->slice().ptr + i;
        if (isIdStart(start))
        {
            size_t j = skippastident(buf, i);
            if (i < j)
            {
                size_t len = j - i;
                if (isIdentifier(a, start, len))
                {
                    i = buf->bracket(i, "$(DDOC_PSYMBOL ", j, ")") - 1;
                    continue;
                }
                if (isFunctionParameter(a, start, len))
                {
                    //printf("highlighting arg '%s', i = %d, j = %d\n", arg->ident->toChars(), i, j);
                    i = buf->bracket(i, "$(DDOC_PARAM ", j, ")") - 1;
                    continue;
                }
                i = j - 1;
            }
        }
    }
}

/****************************************
 */

void highlightCode3(Scope *sc, OutBuffer *buf, const utf8_t *p, const utf8_t *pend)
{
    for (; p < pend; p++)
    {
        const char *s = sc->_module->escapetable->escapeChar(*p);
        if (s)
            buf->writestring(s);
        else
            buf->writeByte(*p);
    }
}

/**************************************************
 * Highlight code for CODE section.
 */

void highlightCode2(Scope *sc, Dsymbols *a, OutBuffer *buf, size_t offset)
{
    unsigned errorsave = global.errors;
    Lexer lex(NULL, (utf8_t *)buf->slice().ptr, 0, buf->length() - 1, 0, 1);
    OutBuffer res;
    const utf8_t *lastp = (utf8_t *)buf->slice().ptr;

    //printf("highlightCode2('%.*s')\n", buf->length() - 1, buf->slice().ptr);
    res.reserve(buf->length());
    while (1)
    {
        Token tok;
        lex.scan(&tok);
        highlightCode3(sc, &res, lastp, tok.ptr);

        const char *highlight = NULL;
        switch (tok.value)
        {
            case TOKidentifier:
            {
                if (!sc)
                    break;
                size_t len = lex.p - tok.ptr;
                if (isIdentifier(a, tok.ptr, len))
                {
                    highlight = "$(D_PSYMBOL ";
                    break;
                }
                if (isFunctionParameter(a, tok.ptr, len))
                {
                    //printf("highlighting arg '%s', i = %d, j = %d\n", arg->ident->toChars(), i, j);
                    highlight = "$(D_PARAM ";
                    break;
                }
                break;
            }
            case TOKcomment:
                highlight = "$(D_COMMENT ";
                break;

            case TOKstring:
                highlight = "$(D_STRING ";
                break;

            default:
                if (tok.isKeyword())
                    highlight = "$(D_KEYWORD ";
                break;
        }
        if (highlight)
        {
            res.writestring(highlight);
            size_t o = res.length();
            highlightCode3(sc, &res, tok.ptr, lex.p);
            if (tok.value == TOKcomment || tok.value == TOKstring)
                escapeDdocString(&res, o);  // Bugzilla 7656, 7715, and 10519
            res.writeByte(')');
        }
        else
            highlightCode3(sc, &res, tok.ptr, lex.p);
        if (tok.value == TOKeof)
            break;
        lastp = lex.p;
    }
    buf->setsize(offset);
    buf->write(&res);
    global.errors = errorsave;
}

/***************************************
 * Find character string to replace c with.
 */

const char *Escape::escapeChar(unsigned c)
{
    assert(c < 256);
    //printf("escapeChar('%c') => %p, %p\n", c, strings, strings[c]);
    return strings[c];
}

/****************************************
 * Determine if p points to the start of a "..." parameter identifier.
 */

bool isCVariadicArg(const utf8_t *p, size_t len)
{
    return len >= 3 && cmp("...", p, 3) == 0;
}

/****************************************
 * Determine if p points to the start of an identifier.
 */

bool isIdStart(const utf8_t *p)
{
    unsigned c = *p;
    if (isalpha(c) || c == '_')
        return true;
    if (c >= 0x80)
    {
        size_t i = 0;
        if (utf_decodeChar(p, 4, &i, &c))
            return false;   // ignore errors
        if (isUniAlpha(c))
            return true;
    }
    return false;
}

/****************************************
 * Determine if p points to the rest of an identifier.
 */

bool isIdTail(const utf8_t *p)
{
    unsigned c = *p;
    if (isalnum(c) || c == '_')
        return true;
    if (c >= 0x80)
    {
        size_t i = 0;
        if (utf_decodeChar(p, 4, &i, &c))
            return false;   // ignore errors
        if (isUniAlpha(c))
            return true;
    }
    return false;
}

/****************************************
 * Determine if p points to the indentation space.
 */

bool isIndentWS(const utf8_t *p)
{
    return (*p == ' ') || (*p == '\t');
}

/*****************************************
 * Return number of bytes in UTF character.
 */

int utfStride(const utf8_t *p)
{
    unsigned c = *p;
    if (c < 0x80)
        return 1;
    size_t i = 0;
    utf_decodeChar(p, 4, &i, &c);       // ignore errors, but still consume input
    return (int)i;
}
