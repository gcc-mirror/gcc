/**
 * Ddoc documentation generation.
 *
 * Specification: $(LINK2 https://dlang.org/spec/ddoc.html, Documentation Generator)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/doc.d, _doc.d)
 * Documentation:  https://dlang.org/phobos/dmd_doc.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/doc.d
 */

module dmd.doc;

import core.stdc.ctype;
import core.stdc.stdlib;
import core.stdc.stdio;
import core.stdc.string;
import core.stdc.time;
import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.cond;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dimport;
import dmd.dmacro;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.func;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.lexer;
import dmd.location;
import dmd.mtype;
import dmd.root.array;
import dmd.root.file;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.port;
import dmd.root.rmem;
import dmd.root.string;
import dmd.root.utf;
import dmd.tokens;
import dmd.utils;
import dmd.visitor;

struct Escape
{
    const(char)[][char.max] strings;

    /***************************************
     * Find character string to replace c with.
     */
    const(char)[] escapeChar(char c)
    {
        version (all)
        {
            //printf("escapeChar('%c') => %p, %p\n", c, strings, strings[c].ptr);
            return strings[c];
        }
        else
        {
            const(char)[] s;
            switch (c)
            {
            case '<':
                s = "&lt;";
                break;
            case '>':
                s = "&gt;";
                break;
            case '&':
                s = "&amp;";
                break;
            default:
                s = null;
                break;
            }
            return s;
        }
    }
}

/***********************************************************
 */
private class Section
{
    const(char)[] name;
    const(char)[] body_;
    int nooutput;

    override string toString() const
    {
        assert(0);
    }

    void write(Loc loc, DocComment* dc, Scope* sc, Dsymbols* a, OutBuffer* buf)
    {
        assert(a.length);
        if (name.length)
        {
            static immutable table =
            [
                "AUTHORS",
                "BUGS",
                "COPYRIGHT",
                "DATE",
                "DEPRECATED",
                "EXAMPLES",
                "HISTORY",
                "LICENSE",
                "RETURNS",
                "SEE_ALSO",
                "STANDARDS",
                "THROWS",
                "VERSION",
            ];
            foreach (entry; table)
            {
                if (iequals(entry, name))
                {
                    buf.printf("$(DDOC_%s ", entry.ptr);
                    goto L1;
                }
            }
            buf.writestring("$(DDOC_SECTION ");
            // Replace _ characters with spaces
            buf.writestring("$(DDOC_SECTION_H ");
            size_t o = buf.length;
            foreach (char c; name)
                buf.writeByte((c == '_') ? ' ' : c);
            escapeStrayParenthesis(loc, buf, o, false);
            buf.writestring(")");
        }
        else
        {
            buf.writestring("$(DDOC_DESCRIPTION ");
        }
    L1:
        size_t o = buf.length;
        buf.write(body_);
        escapeStrayParenthesis(loc, buf, o, true);
        highlightText(sc, a, loc, *buf, o);
        buf.writestring(")");
    }
}

/***********************************************************
 */
private final class ParamSection : Section
{
    override void write(Loc loc, DocComment* dc, Scope* sc, Dsymbols* a, OutBuffer* buf)
    {
        assert(a.length);
        Dsymbol s = (*a)[0]; // test
        const(char)* p = body_.ptr;
        size_t len = body_.length;
        const(char)* pend = p + len;
        const(char)* tempstart = null;
        size_t templen = 0;
        const(char)* namestart = null;
        size_t namelen = 0; // !=0 if line continuation
        const(char)* textstart = null;
        size_t textlen = 0;
        size_t paramcount = 0;
        buf.writestring("$(DDOC_PARAMS ");
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
                    if (isIdStart(p) || isCVariadicArg(p[0 .. cast(size_t)(pend - p)]))
                        break;
                    if (namelen)
                        goto Ltext;
                    // continuation of prev macro
                    goto Lskipline;
                }
                break;
            }
            tempstart = p;
            while (isIdTail(p))
                p += utfStride(p);
            if (isCVariadicArg(p[0 .. cast(size_t)(pend - p)]))
                p += 3;
            templen = p - tempstart;
            while (*p == ' ' || *p == '\t')
                p++;
            if (*p != '=')
            {
                if (namelen)
                    goto Ltext;
                // continuation of prev macro
                goto Lskipline;
            }
            p++;
            if (namelen)
            {
                // Output existing param
            L1:
                //printf("param '%.*s' = '%.*s'\n", cast(int)namelen, namestart, cast(int)textlen, textstart);
                ++paramcount;
                HdrGenState hgs;
                buf.writestring("$(DDOC_PARAM_ROW ");
                {
                    buf.writestring("$(DDOC_PARAM_ID ");
                    {
                        size_t o = buf.length;
                        Parameter fparam = isFunctionParameter(a, namestart[0 .. namelen]);
                        if (!fparam)
                        {
                            // Comments on a template might refer to function parameters within.
                            // Search the parameters of nested eponymous functions (with the same name.)
                            fparam = isEponymousFunctionParameter(a, namestart[0 ..  namelen]);
                        }
                        bool isCVariadic = isCVariadicParameter(a, namestart[0 .. namelen]);
                        if (isCVariadic)
                        {
                            buf.writestring("...");
                        }
                        else if (fparam && fparam.type && fparam.ident)
                        {
                            .toCBuffer(fparam.type, buf, fparam.ident, &hgs);
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
                                warning(s.loc, "Ddoc: function declaration has no parameter '%.*s'", cast(int)namelen, namestart);
                            }
                            buf.write(namestart[0 .. namelen]);
                        }
                        escapeStrayParenthesis(loc, buf, o, true);
                        highlightCode(sc, a, *buf, o);
                    }
                    buf.writestring(")");
                    buf.writestring("$(DDOC_PARAM_DESC ");
                    {
                        size_t o = buf.length;
                        buf.write(textstart[0 .. textlen]);
                        escapeStrayParenthesis(loc, buf, o, true);
                        highlightText(sc, a, loc, *buf, o);
                    }
                    buf.writestring(")");
                }
                buf.writestring(")");
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
            {
            }
        }
        if (namelen)
            goto L1;
        // write out last one
        buf.writestring(")");
        TypeFunction tf = a.length == 1 ? isTypeFunction(s) : null;
        if (tf)
        {
            size_t pcount = (tf.parameterList.parameters ? tf.parameterList.parameters.length : 0) +
                            cast(int)(tf.parameterList.varargs == VarArg.variadic);
            if (pcount != paramcount)
            {
                warning(s.loc, "Ddoc: parameter count mismatch, expected %llu, got %llu",
                        cast(ulong) pcount, cast(ulong) paramcount);
                if (paramcount == 0)
                {
                    // Chances are someone messed up the format
                    warningSupplemental(s.loc, "Note that the format is `param = description`");
                }
            }
        }
    }
}

/***********************************************************
 */
private final class MacroSection : Section
{
    override void write(Loc loc, DocComment* dc, Scope* sc, Dsymbols* a, OutBuffer* buf)
    {
        //printf("MacroSection::write()\n");
        DocComment.parseMacros(dc.escapetable, *dc.pmacrotable, body_);
    }
}

private alias Sections = Array!(Section);

// Workaround for missing Parameter instance for variadic params. (it's unnecessary to instantiate one).
private bool isCVariadicParameter(Dsymbols* a, const(char)[] p) @safe
{
    foreach (member; *a)
    {
        TypeFunction tf = isTypeFunction(member);
        if (tf && tf.parameterList.varargs == VarArg.variadic && p == "...")
            return true;
    }
    return false;
}

private Dsymbol getEponymousMember(TemplateDeclaration td) @safe
{
    if (!td.onemember)
        return null;
    if (AggregateDeclaration ad = td.onemember.isAggregateDeclaration())
        return ad;
    if (FuncDeclaration fd = td.onemember.isFuncDeclaration())
        return fd;
    if (auto em = td.onemember.isEnumMember())
        return null;    // Keep backward compatibility. See compilable/ddoc9.d
    if (VarDeclaration vd = td.onemember.isVarDeclaration())
        return td.constraint ? null : vd;
    return null;
}

private TemplateDeclaration getEponymousParent(Dsymbol s)
{
    if (!s.parent)
        return null;
    TemplateDeclaration td = s.parent.isTemplateDeclaration();
    return (td && getEponymousMember(td)) ? td : null;
}

private immutable ddoc_default = import("default_ddoc_theme." ~ ddoc_ext);
private immutable ddoc_decl_s = "$(DDOC_DECL ";
private immutable ddoc_decl_e = ")\n";
private immutable ddoc_decl_dd_s = "$(DDOC_DECL_DD ";
private immutable ddoc_decl_dd_e = ")\n";

/****************************************************
 */
extern(C++) void gendocfile(Module m)
{
    __gshared OutBuffer mbuf;
    __gshared int mbuf_done;
    OutBuffer buf;
    //printf("Module::gendocfile()\n");
    if (!mbuf_done) // if not already read the ddoc files
    {
        mbuf_done = 1;
        // Use our internal default
        mbuf.writestring(ddoc_default);
        // Override with DDOCFILE specified in the sc.ini file
        char* p = getenv("DDOCFILE");
        if (p)
            global.params.ddoc.files.shift(p);
        // Override with the ddoc macro files from the command line
        for (size_t i = 0; i < global.params.ddoc.files.length; i++)
        {
            auto buffer = readFile(m.loc, global.params.ddoc.files[i]);
            // BUG: convert file contents to UTF-8 before use
            const data = buffer.data;
            //printf("file: '%.*s'\n", cast(int)data.length, data.ptr);
            mbuf.write(data);
        }
    }
    DocComment.parseMacros(m.escapetable, m.macrotable, mbuf[]);
    Scope* sc = Scope.createGlobal(m); // create root scope
    DocComment* dc = DocComment.parse(m, m.comment);
    dc.pmacrotable = &m.macrotable;
    dc.escapetable = m.escapetable;
    sc.lastdc = dc;
    // Generate predefined macros
    // Set the title to be the name of the module
    {
        const p = m.toPrettyChars().toDString;
        m.macrotable.define("TITLE", p);
    }
    // Set time macros
    {
        time_t t;
        time(&t);
        char* p = ctime(&t);
        p = mem.xstrdup(p);
        m.macrotable.define("DATETIME", p.toDString());
        m.macrotable.define("YEAR", p[20 .. 20 + 4]);
    }
    const srcfilename = m.srcfile.toString();
    m.macrotable.define("SRCFILENAME", srcfilename);
    const docfilename = m.docfile.toString();
    m.macrotable.define("DOCFILENAME", docfilename);
    if (dc.copyright)
    {
        dc.copyright.nooutput = 1;
        m.macrotable.define("COPYRIGHT", dc.copyright.body_);
    }
    if (m.filetype == FileType.ddoc)
    {
        const ploc = m.md ? &m.md.loc : &m.loc;
        const loc = Loc(ploc.filename ? ploc.filename : srcfilename.ptr,
                        ploc.linnum,
                        ploc.charnum);

        size_t commentlen = strlen(cast(char*)m.comment);
        Dsymbols a;
        // https://issues.dlang.org/show_bug.cgi?id=9764
        // Don't push m in a, to prevent emphasize ddoc file name.
        if (dc.macros)
        {
            commentlen = dc.macros.name.ptr - m.comment;
            dc.macros.write(loc, dc, sc, &a, &buf);
        }
        buf.write(m.comment[0 .. commentlen]);
        highlightText(sc, &a, loc, buf, 0);
    }
    else
    {
        Dsymbols a;
        a.push(m);
        dc.writeSections(sc, &a, &buf);
        emitMemberComments(m, buf, sc);
    }
    //printf("BODY= '%.*s'\n", cast(int)buf.length, buf.data);
    m.macrotable.define("BODY", buf[]);
    OutBuffer buf2;
    buf2.writestring("$(DDOC)");
    size_t end = buf2.length;

    const success = m.macrotable.expand(buf2, 0, end, null, global.recursionLimit);
    if (!success)
        error(Loc.initial, "DDoc macro expansion limit exceeded; more than %d expansions.", global.recursionLimit);

    version (all)
    {
        /* Remove all the escape sequences from buf2,
         * and make CR-LF the newline.
         */
        {
            const slice = buf2[];
            buf.setsize(0);
            buf.reserve(slice.length);
            auto p = slice.ptr;
            for (size_t j = 0; j < slice.length; j++)
            {
                char c = p[j];
                if (c == 0xFF && j + 1 < slice.length)
                {
                    j++;
                    continue;
                }
                if (c == '\n')
                    buf.writeByte('\r');
                else if (c == '\r')
                {
                    buf.writestring("\r\n");
                    if (j + 1 < slice.length && p[j + 1] == '\n')
                    {
                        j++;
                    }
                    continue;
                }
                buf.writeByte(c);
            }
        }
        writeFile(m.loc, m.docfile.toString(), buf[]);
    }
    else
    {
        /* Remove all the escape sequences from buf2
         */
        {
            size_t i = 0;
            char* p = buf2.data;
            for (size_t j = 0; j < buf2.length; j++)
            {
                if (p[j] == 0xFF && j + 1 < buf2.length)
                {
                    j++;
                    continue;
                }
                p[i] = p[j];
                i++;
            }
            buf2.setsize(i);
        }
        writeFile(m.loc, m.docfile.toString(), buf2[]);
    }
}

/****************************************************
 * Having unmatched parentheses can hose the output of Ddoc,
 * as the macros depend on properly nested parentheses.
 * This function replaces all ( with $(LPAREN) and ) with $(RPAREN)
 * to preserve text literally. This also means macros in the
 * text won't be expanded.
 */
void escapeDdocString(OutBuffer* buf, size_t start)
{
    for (size_t u = start; u < buf.length; u++)
    {
        char c = (*buf)[u];
        switch (c)
        {
        case '$':
            buf.remove(u, 1);
            buf.insert(u, "$(DOLLAR)");
            u += 8;
            break;
        case '(':
            buf.remove(u, 1); //remove the (
            buf.insert(u, "$(LPAREN)"); //insert this instead
            u += 8; //skip over newly inserted macro
            break;
        case ')':
            buf.remove(u, 1); //remove the )
            buf.insert(u, "$(RPAREN)"); //insert this instead
            u += 8; //skip over newly inserted macro
            break;
        default:
            break;
        }
    }
}

/****************************************************
 * Having unmatched parentheses can hose the output of Ddoc,
 * as the macros depend on properly nested parentheses.
 *
 * Fix by replacing unmatched ( with $(LPAREN) and unmatched ) with $(RPAREN).
 *
 * Params:
 *  loc   = source location of start of text. It is a mutable copy to allow incrementing its linenum, for printing the correct line number when an error is encountered in a multiline block of ddoc.
 *  buf   = an OutBuffer containing the DDoc
 *  start = the index within buf to start replacing unmatched parentheses
 *  respectBackslashEscapes = if true, always replace parentheses that are
 *    directly preceeded by a backslash with $(LPAREN) or $(RPAREN) instead of
 *    counting them as stray parentheses
 */
private void escapeStrayParenthesis(Loc loc, OutBuffer* buf, size_t start, bool respectBackslashEscapes)
{
    uint par_open = 0;
    char inCode = 0;
    bool atLineStart = true;
    for (size_t u = start; u < buf.length; u++)
    {
        char c = (*buf)[u];
        switch (c)
        {
        case '(':
            if (!inCode)
                par_open++;
            atLineStart = false;
            break;
        case ')':
            if (!inCode)
            {
                if (par_open == 0)
                {
                    //stray ')'
                    warning(loc, "Ddoc: Stray ')'. This may cause incorrect Ddoc output. Use $(RPAREN) instead for unpaired right parentheses.");
                    buf.remove(u, 1); //remove the )
                    buf.insert(u, "$(RPAREN)"); //insert this instead
                    u += 8; //skip over newly inserted macro
                }
                else
                    par_open--;
            }
            atLineStart = false;
            break;
        case '\n':
            atLineStart = true;
            version (none)
            {
                // For this to work, loc must be set to the beginning of the passed
                // text which is currently not possible
                // (loc is set to the Loc of the Dsymbol)
                loc.linnum++;
            }
            break;
        case ' ':
        case '\r':
        case '\t':
            break;
        case '-':
        case '`':
        case '~':
            // Issue 15465: don't try to escape unbalanced parens inside code
            // blocks.
            int numdash = 1;
            for (++u; u < buf.length && (*buf)[u] == c; ++u)
                ++numdash;
            --u;
            if (c == '`' || (atLineStart && numdash >= 3))
            {
                if (inCode == c)
                    inCode = 0;
                else if (!inCode)
                    inCode = c;
            }
            atLineStart = false;
            break;
        case '\\':
            // replace backslash-escaped parens with their macros
            if (!inCode && respectBackslashEscapes && u+1 < buf.length)
            {
                if ((*buf)[u+1] == '(' || (*buf)[u+1] == ')')
                {
                    const paren = (*buf)[u+1] == '(' ? "$(LPAREN)" : "$(RPAREN)";
                    buf.remove(u, 2); //remove the \)
                    buf.insert(u, paren); //insert this instead
                    u += 8; //skip over newly inserted macro
                }
                else if ((*buf)[u+1] == '\\')
                    ++u;
            }
            break;
        default:
            atLineStart = false;
            break;
        }
    }
    if (par_open) // if any unmatched lparens
    {
        par_open = 0;
        for (size_t u = buf.length; u > start;)
        {
            u--;
            char c = (*buf)[u];
            switch (c)
            {
            case ')':
                par_open++;
                break;
            case '(':
                if (par_open == 0)
                {
                    //stray '('
                    warning(loc, "Ddoc: Stray '('. This may cause incorrect Ddoc output. Use $(LPAREN) instead for unpaired left parentheses.");
                    buf.remove(u, 1); //remove the (
                    buf.insert(u, "$(LPAREN)"); //insert this instead
                }
                else
                    par_open--;
                break;
            default:
                break;
            }
        }
    }
}

// Basically, this is to skip over things like private{} blocks in a struct or
// class definition that don't add any components to the qualified name.
private Scope* skipNonQualScopes(Scope* sc)
{
    while (sc && !sc.scopesym)
        sc = sc.enclosing;
    return sc;
}

private bool emitAnchorName(ref OutBuffer buf, Dsymbol s, Scope* sc, bool includeParent)
{
    if (!s || s.isPackage() || s.isModule())
        return false;
    // Add parent names first
    bool dot = false;
    auto eponymousParent = getEponymousParent(s);
    if (includeParent && s.parent || eponymousParent)
        dot = emitAnchorName(buf, s.parent, sc, includeParent);
    else if (includeParent && sc)
        dot = emitAnchorName(buf, sc.scopesym, skipNonQualScopes(sc.enclosing), includeParent);
    // Eponymous template members can share the parent anchor name
    if (eponymousParent)
        return dot;
    if (dot)
        buf.writeByte('.');
    // Use "this" not "__ctor"
    TemplateDeclaration td;
    if (s.isCtorDeclaration() || ((td = s.isTemplateDeclaration()) !is null && td.onemember && td.onemember.isCtorDeclaration()))
    {
        buf.writestring("this");
    }
    else
    {
        /* We just want the identifier, not overloads like TemplateDeclaration::toChars.
         * We don't want the template parameter list and constraints. */
        buf.writestring(s.Dsymbol.toChars());
    }
    return true;
}

private void emitAnchor(ref OutBuffer buf, Dsymbol s, Scope* sc, bool forHeader = false)
{
    Identifier ident;
    {
        OutBuffer anc;
        emitAnchorName(anc, s, skipNonQualScopes(sc), true);
        ident = Identifier.idPool(anc[]);
    }

    auto pcount = cast(void*)ident in sc.anchorCounts;
    typeof(*pcount) count;
    if (!forHeader)
    {
        if (pcount)
        {
            // Existing anchor,
            // don't write an anchor for matching consecutive ditto symbols
            TemplateDeclaration td = getEponymousParent(s);
            if (sc.prevAnchor == ident && sc.lastdc && (isDitto(s.comment) || (td && isDitto(td.comment))))
                return;

            count = ++*pcount;
        }
        else
        {
            sc.anchorCounts[cast(void*)ident] = 1;
            count = 1;
        }
    }

    // cache anchor name
    sc.prevAnchor = ident;
    auto macroName = forHeader ? "DDOC_HEADER_ANCHOR" : "DDOC_ANCHOR";

    if (auto imp = s.isImport())
    {
        // For example: `public import core.stdc.string : memcpy, memcmp;`
        if (imp.aliases.length > 0)
        {
            for(int i = 0; i < imp.aliases.length; i++)
            {
                // Need to distinguish between
                // `public import core.stdc.string : memcpy, memcmp;` and
                // `public import core.stdc.string : copy = memcpy, compare = memcmp;`
                auto a = imp.aliases[i];
                auto id = a ? a : imp.names[i];
                auto loc = Loc.init;
                if (auto symFromId = sc.search(loc, id, null))
                {
                    emitAnchor(buf, symFromId, sc, forHeader);
                }
            }
        }
        else
        {
            // For example: `public import str = core.stdc.string;`
            if (imp.aliasId)
            {
                auto symbolName = imp.aliasId.toString();

                buf.printf("$(%.*s %.*s", cast(int) macroName.length, macroName.ptr,
                    cast(int) symbolName.length, symbolName.ptr);

                if (forHeader)
                {
                    buf.printf(", %.*s", cast(int) symbolName.length, symbolName.ptr);
                }
            }
            else
            {
                // The general case:  `public import core.stdc.string;`

                // fully qualify imports so `core.stdc.string` doesn't appear as `core`
                void printFullyQualifiedImport()
                {
                    foreach (const pid; imp.packages)
                    {
                        buf.printf("%s.", pid.toChars());
                    }
                    buf.writestring(imp.id.toString());
                }

                buf.printf("$(%.*s ", cast(int) macroName.length, macroName.ptr);
                printFullyQualifiedImport();

                if (forHeader)
                {
                    buf.printf(", ");
                    printFullyQualifiedImport();
                }
            }

            buf.writeByte(')');
        }
    }
    else
    {
        auto symbolName = ident.toString();
        buf.printf("$(%.*s %.*s", cast(int) macroName.length, macroName.ptr,
            cast(int) symbolName.length, symbolName.ptr);

        // only append count once there's a duplicate
        if (count > 1)
            buf.printf(".%u", count);

        if (forHeader)
        {
            Identifier shortIdent;
            {
                OutBuffer anc;
                emitAnchorName(anc, s, skipNonQualScopes(sc), false);
                shortIdent = Identifier.idPool(anc[]);
            }

            auto shortName = shortIdent.toString();
            buf.printf(", %.*s", cast(int) shortName.length, shortName.ptr);
        }

        buf.writeByte(')');
    }
}

/******************************* emitComment **********************************/

/** Get leading indentation from 'src' which represents lines of code. */
private size_t getCodeIndent(const(char)* src)
{
    while (src && (*src == '\r' || *src == '\n'))
        ++src; // skip until we find the first non-empty line
    size_t codeIndent = 0;
    while (src && (*src == ' ' || *src == '\t'))
    {
        codeIndent++;
        src++;
    }
    return codeIndent;
}

/** Recursively expand template mixin member docs into the scope. */
private void expandTemplateMixinComments(TemplateMixin tm, ref OutBuffer buf, Scope* sc)
{
    if (!tm.semanticRun)
        tm.dsymbolSemantic(sc);
    TemplateDeclaration td = (tm && tm.tempdecl) ? tm.tempdecl.isTemplateDeclaration() : null;
    if (td && td.members)
    {
        for (size_t i = 0; i < td.members.length; i++)
        {
            Dsymbol sm = (*td.members)[i];
            TemplateMixin tmc = sm.isTemplateMixin();
            if (tmc && tmc.comment)
                expandTemplateMixinComments(tmc, buf, sc);
            else
                emitComment(sm, buf, sc);
        }
    }
}

private void emitMemberComments(ScopeDsymbol sds, ref OutBuffer buf, Scope* sc)
{
    if (!sds.members)
        return;
    //printf("ScopeDsymbol::emitMemberComments() %s\n", toChars());
    const(char)[] m = "$(DDOC_MEMBERS ";
    if (sds.isTemplateDeclaration())
        m = "$(DDOC_TEMPLATE_MEMBERS ";
    else if (sds.isClassDeclaration())
        m = "$(DDOC_CLASS_MEMBERS ";
    else if (sds.isStructDeclaration())
        m = "$(DDOC_STRUCT_MEMBERS ";
    else if (sds.isEnumDeclaration())
        m = "$(DDOC_ENUM_MEMBERS ";
    else if (sds.isModule())
        m = "$(DDOC_MODULE_MEMBERS ";
    size_t offset1 = buf.length; // save starting offset
    buf.writestring(m);
    size_t offset2 = buf.length; // to see if we write anything
    sc = sc.push(sds);
    for (size_t i = 0; i < sds.members.length; i++)
    {
        Dsymbol s = (*sds.members)[i];
        //printf("\ts = '%s'\n", s.toChars());
        // only expand if parent is a non-template (semantic won't work)
        if (s.comment && s.isTemplateMixin() && s.parent && !s.parent.isTemplateDeclaration())
            expandTemplateMixinComments(cast(TemplateMixin)s, buf, sc);
        emitComment(s, buf, sc);
    }
    emitComment(null, buf, sc);
    sc.pop();
    if (buf.length == offset2)
    {
        /* Didn't write out any members, so back out last write
         */
        buf.setsize(offset1);
    }
    else
        buf.writestring(")");
}

private void emitVisibility(ref OutBuffer buf, Import i)
{
    // imports are private by default, which is different from other declarations
    // so they should explicitly show their visibility
    emitVisibility(buf, i.visibility);
}

private void emitVisibility(ref OutBuffer buf, Declaration d)
{
    auto vis = d.visibility;
    if (vis.kind != Visibility.Kind.undefined && vis.kind != Visibility.Kind.public_)
    {
        emitVisibility(buf, vis);
    }
}

private void emitVisibility(ref OutBuffer buf, Visibility vis)
{
    visibilityToBuffer(&buf, vis);
    buf.writeByte(' ');
}

private void emitComment(Dsymbol s, ref OutBuffer buf, Scope* sc)
{
    extern (C++) final class EmitComment : Visitor
    {
        alias visit = Visitor.visit;
    public:
        OutBuffer* buf;
        Scope* sc;

        extern (D) this(ref OutBuffer buf, Scope* sc) scope
        {
            this.buf = &buf;
            this.sc = sc;
        }

        override void visit(Dsymbol)
        {
        }

        override void visit(InvariantDeclaration)
        {
        }

        override void visit(UnitTestDeclaration)
        {
        }

        override void visit(PostBlitDeclaration)
        {
        }

        override void visit(DtorDeclaration)
        {
        }

        override void visit(StaticCtorDeclaration)
        {
        }

        override void visit(StaticDtorDeclaration)
        {
        }

        override void visit(TypeInfoDeclaration)
        {
        }

        void emit(Scope* sc, Dsymbol s, const(char)* com)
        {
            if (s && sc.lastdc && isDitto(com))
            {
                sc.lastdc.a.push(s);
                return;
            }
            // Put previous doc comment if exists
            if (DocComment* dc = sc.lastdc)
            {
                assert(dc.a.length > 0, "Expects at least one declaration for a" ~
                    "documentation comment");

                auto symbol = dc.a[0];

                buf.writestring("$(DDOC_MEMBER");
                buf.writestring("$(DDOC_MEMBER_HEADER");
                emitAnchor(*buf, symbol, sc, true);
                buf.writeByte(')');

                // Put the declaration signatures as the document 'title'
                buf.writestring(ddoc_decl_s);
                for (size_t i = 0; i < dc.a.length; i++)
                {
                    Dsymbol sx = dc.a[i];
                    // the added linebreaks in here make looking at multiple
                    // signatures more appealing
                    if (i == 0)
                    {
                        size_t o = buf.length;
                        toDocBuffer(sx, *buf, sc);
                        highlightCode(sc, sx, *buf, o);
                        buf.writestring("$(DDOC_OVERLOAD_SEPARATOR)");
                        continue;
                    }
                    buf.writestring("$(DDOC_DITTO ");
                    {
                        size_t o = buf.length;
                        toDocBuffer(sx, *buf, sc);
                        highlightCode(sc, sx, *buf, o);
                    }
                    buf.writestring("$(DDOC_OVERLOAD_SEPARATOR)");
                    buf.writeByte(')');
                }
                buf.writestring(ddoc_decl_e);
                // Put the ddoc comment as the document 'description'
                buf.writestring(ddoc_decl_dd_s);
                {
                    dc.writeSections(sc, &dc.a, buf);
                    if (ScopeDsymbol sds = dc.a[0].isScopeDsymbol())
                        emitMemberComments(sds, *buf, sc);
                }
                buf.writestring(ddoc_decl_dd_e);
                buf.writeByte(')');
                //printf("buf.2 = [[%.*s]]\n", cast(int)(buf.length - o0), buf.data + o0);
            }
            if (s)
            {
                DocComment* dc = DocComment.parse(s, com);
                dc.pmacrotable = &sc._module.macrotable;
                sc.lastdc = dc;
            }
        }

        override void visit(Import imp)
        {
            if (imp.visible().kind != Visibility.Kind.public_ && sc.visibility.kind != Visibility.Kind.export_)
                return;

            if (imp.comment)
                emit(sc, imp, imp.comment);
        }

        override void visit(Declaration d)
        {
            //printf("Declaration::emitComment(%p '%s'), comment = '%s'\n", d, d.toChars(), d.comment);
            //printf("type = %p\n", d.type);
            const(char)* com = d.comment;
            if (TemplateDeclaration td = getEponymousParent(d))
            {
                if (isDitto(td.comment))
                    com = td.comment;
                else
                    com = Lexer.combineComments(td.comment.toDString(), com.toDString(), true);
            }
            else
            {
                if (!d.ident)
                    return;
                if (!d.type)
                {
                    if (!d.isCtorDeclaration() &&
                        !d.isAliasDeclaration() &&
                        !d.isVarDeclaration())
                    {
                        return;
                    }
                }
                if (d.visibility.kind == Visibility.Kind.private_ || sc.visibility.kind == Visibility.Kind.private_)
                    return;
            }
            if (!com)
                return;
            emit(sc, d, com);
        }

        override void visit(AggregateDeclaration ad)
        {
            //printf("AggregateDeclaration::emitComment() '%s'\n", ad.toChars());
            const(char)* com = ad.comment;
            if (TemplateDeclaration td = getEponymousParent(ad))
            {
                if (isDitto(td.comment))
                    com = td.comment;
                else
                    com = Lexer.combineComments(td.comment.toDString(), com.toDString(), true);
            }
            else
            {
                if (ad.visible().kind == Visibility.Kind.private_ || sc.visibility.kind == Visibility.Kind.private_)
                    return;
                if (!ad.comment)
                    return;
            }
            if (!com)
                return;
            emit(sc, ad, com);
        }

        override void visit(TemplateDeclaration td)
        {
            //printf("TemplateDeclaration::emitComment() '%s', kind = %s\n", td.toChars(), td.kind());
            if (td.visible().kind == Visibility.Kind.private_ || sc.visibility.kind == Visibility.Kind.private_)
                return;
            if (!td.comment)
                return;
            if (Dsymbol ss = getEponymousMember(td))
            {
                ss.accept(this);
                return;
            }
            emit(sc, td, td.comment);
        }

        override void visit(EnumDeclaration ed)
        {
            if (ed.visible().kind == Visibility.Kind.private_ || sc.visibility.kind == Visibility.Kind.private_)
                return;
            if (ed.isAnonymous() && ed.members)
            {
                for (size_t i = 0; i < ed.members.length; i++)
                {
                    Dsymbol s = (*ed.members)[i];
                    emitComment(s, *buf, sc);
                }
                return;
            }
            if (!ed.comment)
                return;
            if (ed.isAnonymous())
                return;
            emit(sc, ed, ed.comment);
        }

        override void visit(EnumMember em)
        {
            //printf("EnumMember::emitComment(%p '%s'), comment = '%s'\n", em, em.toChars(), em.comment);
            if (em.visible().kind == Visibility.Kind.private_ || sc.visibility.kind == Visibility.Kind.private_)
                return;
            if (!em.comment)
                return;
            emit(sc, em, em.comment);
        }

        override void visit(AttribDeclaration ad)
        {
            //printf("AttribDeclaration::emitComment(sc = %p)\n", sc);
            /* A general problem with this,
             * illustrated by https://issues.dlang.org/show_bug.cgi?id=2516
             * is that attributes are not transmitted through to the underlying
             * member declarations for template bodies, because semantic analysis
             * is not done for template declaration bodies
             * (only template instantiations).
             * Hence, Ddoc omits attributes from template members.
             */
            Dsymbols* d = ad.include(null);
            if (d)
            {
                for (size_t i = 0; i < d.length; i++)
                {
                    Dsymbol s = (*d)[i];
                    //printf("AttribDeclaration::emitComment %s\n", s.toChars());
                    emitComment(s, *buf, sc);
                }
            }
        }

        override void visit(VisibilityDeclaration pd)
        {
            if (pd.decl)
            {
                Scope* scx = sc;
                sc = sc.copy();
                sc.visibility = pd.visibility;
                visit(cast(AttribDeclaration)pd);
                scx.lastdc = sc.lastdc;
                sc = sc.pop();
            }
        }

        override void visit(ConditionalDeclaration cd)
        {
            //printf("ConditionalDeclaration::emitComment(sc = %p)\n", sc);
            if (cd.condition.inc != Include.notComputed)
            {
                visit(cast(AttribDeclaration)cd);
                return;
            }
            /* If generating doc comment, be careful because if we're inside
             * a template, then include(null) will fail.
             */
            Dsymbols* d = cd.decl ? cd.decl : cd.elsedecl;
            for (size_t i = 0; i < d.length; i++)
            {
                Dsymbol s = (*d)[i];
                emitComment(s, *buf, sc);
            }
        }
    }

    scope EmitComment v = new EmitComment(buf, sc);
    if (!s)
        v.emit(sc, null, null);
    else
        s.accept(v);
}

private void toDocBuffer(Dsymbol s, ref OutBuffer buf, Scope* sc)
{
    extern (C++) final class ToDocBuffer : Visitor
    {
        alias visit = Visitor.visit;
    public:
        OutBuffer* buf;
        Scope* sc;

        extern (D) this(ref OutBuffer buf, Scope* sc) scope
        {
            this.buf = &buf;
            this.sc = sc;
        }

        override void visit(Dsymbol s)
        {
            //printf("Dsymbol::toDocbuffer() %s\n", s.toChars());
            HdrGenState hgs;
            hgs.ddoc = true;
            .toCBuffer(s, buf, &hgs);
        }

        void prefix(Dsymbol s)
        {
            if (s.isDeprecated())
                buf.writestring("deprecated ");
            if (Declaration d = s.isDeclaration())
            {
                emitVisibility(*buf, d);
                if (d.isStatic())
                    buf.writestring("static ");
                else if (d.isFinal())
                    buf.writestring("final ");
                else if (d.isAbstract())
                    buf.writestring("abstract ");

                if (d.isFuncDeclaration())      // functionToBufferFull handles this
                    return;

                if (d.isImmutable())
                    buf.writestring("immutable ");
                if (d.storage_class & STC.shared_)
                    buf.writestring("shared ");
                if (d.isWild())
                    buf.writestring("inout ");
                if (d.isConst())
                    buf.writestring("const ");

                if (d.isSynchronized())
                    buf.writestring("synchronized ");

                if (d.storage_class & STC.manifest)
                    buf.writestring("enum ");

                // Add "auto" for the untyped variable in template members
                if (!d.type && d.isVarDeclaration() &&
                    !d.isImmutable() && !(d.storage_class & STC.shared_) && !d.isWild() && !d.isConst() &&
                    !d.isSynchronized())
                {
                    buf.writestring("auto ");
                }
            }
        }

        override void visit(Import i)
        {
            HdrGenState hgs;
            hgs.ddoc = true;
            emitVisibility(*buf, i);
            .toCBuffer(i, buf, &hgs);
        }

        override void visit(Declaration d)
        {
            if (!d.ident)
                return;
            TemplateDeclaration td = getEponymousParent(d);
            //printf("Declaration::toDocbuffer() %s, originalType = %s, td = %s\n", d.toChars(), d.originalType ? d.originalType.toChars() : "--", td ? td.toChars() : "--");
            HdrGenState hgs;
            hgs.ddoc = true;
            if (d.isDeprecated())
                buf.writestring("$(DEPRECATED ");
            prefix(d);
            if (d.type)
            {
                Type origType = d.originalType ? d.originalType : d.type;
                if (origType.ty == Tfunction)
                {
                    functionToBufferFull(cast(TypeFunction)origType, buf, d.ident, &hgs, td);
                }
                else
                    .toCBuffer(origType, buf, d.ident, &hgs);
            }
            else
                buf.writestring(d.ident.toString());
            if (d.isVarDeclaration() && td)
            {
                buf.writeByte('(');
                if (td.origParameters && td.origParameters.length)
                {
                    for (size_t i = 0; i < td.origParameters.length; i++)
                    {
                        if (i)
                            buf.writestring(", ");
                        toCBuffer((*td.origParameters)[i], buf, &hgs);
                    }
                }
                buf.writeByte(')');
            }
            // emit constraints if declaration is a templated declaration
            if (td && td.constraint)
            {
                bool noFuncDecl = td.isFuncDeclaration() is null;
                if (noFuncDecl)
                {
                    buf.writestring("$(DDOC_CONSTRAINT ");
                }

                .toCBuffer(td.constraint, buf, &hgs);

                if (noFuncDecl)
                {
                    buf.writestring(")");
                }
            }
            if (d.isDeprecated())
                buf.writestring(")");
            buf.writestring(";\n");
        }

        override void visit(AliasDeclaration ad)
        {
            //printf("AliasDeclaration::toDocbuffer() %s\n", ad.toChars());
            if (!ad.ident)
                return;
            if (ad.isDeprecated())
                buf.writestring("deprecated ");
            emitVisibility(*buf, ad);
            buf.printf("alias %s = ", ad.toChars());
            if (Dsymbol s = ad.aliassym) // ident alias
            {
                prettyPrintDsymbol(s, ad.parent);
            }
            else if (Type type = ad.getType()) // type alias
            {
                if (type.ty == Tclass || type.ty == Tstruct || type.ty == Tenum)
                {
                    if (Dsymbol s = type.toDsymbol(null)) // elaborate type
                        prettyPrintDsymbol(s, ad.parent);
                    else
                        buf.writestring(type.toChars());
                }
                else
                {
                    // simple type
                    buf.writestring(type.toChars());
                }
            }
            buf.writestring(";\n");
        }

        void parentToBuffer(Dsymbol s)
        {
            if (s && !s.isPackage() && !s.isModule())
            {
                parentToBuffer(s.parent);
                buf.writestring(s.toChars());
                buf.writestring(".");
            }
        }

        static bool inSameModule(Dsymbol s, Dsymbol p)
        {
            for (; s; s = s.parent)
            {
                if (s.isModule())
                    break;
            }
            for (; p; p = p.parent)
            {
                if (p.isModule())
                    break;
            }
            return s == p;
        }

        void prettyPrintDsymbol(Dsymbol s, Dsymbol parent)
        {
            if (s.parent && (s.parent == parent)) // in current scope -> naked name
            {
                buf.writestring(s.toChars());
            }
            else if (!inSameModule(s, parent)) // in another module -> full name
            {
                buf.writestring(s.toPrettyChars());
            }
            else // nested in a type in this module -> full name w/o module name
            {
                // if alias is nested in a user-type use module-scope lookup
                if (!parent.isModule() && !parent.isPackage())
                    buf.writestring(".");
                parentToBuffer(s.parent);
                buf.writestring(s.toChars());
            }
        }

        override void visit(AggregateDeclaration ad)
        {
            if (!ad.ident)
                return;
            version (none)
            {
                emitVisibility(buf, ad);
            }
            buf.printf("%s %s", ad.kind(), ad.toChars());
            buf.writestring(";\n");
        }

        override void visit(StructDeclaration sd)
        {
            //printf("StructDeclaration::toDocbuffer() %s\n", sd.toChars());
            if (!sd.ident)
                return;
            version (none)
            {
                emitVisibility(buf, sd);
            }
            if (TemplateDeclaration td = getEponymousParent(sd))
            {
                toDocBuffer(td, *buf, sc);
            }
            else
            {
                buf.printf("%s %s", sd.kind(), sd.toChars());
            }
            buf.writestring(";\n");
        }

        override void visit(ClassDeclaration cd)
        {
            //printf("ClassDeclaration::toDocbuffer() %s\n", cd.toChars());
            if (!cd.ident)
                return;
            version (none)
            {
                emitVisibility(*buf, cd);
            }
            if (TemplateDeclaration td = getEponymousParent(cd))
            {
                toDocBuffer(td, *buf, sc);
            }
            else
            {
                if (!cd.isInterfaceDeclaration() && cd.isAbstract())
                    buf.writestring("abstract ");
                buf.printf("%s %s", cd.kind(), cd.toChars());
            }
            int any = 0;
            for (size_t i = 0; i < cd.baseclasses.length; i++)
            {
                BaseClass* bc = (*cd.baseclasses)[i];
                if (bc.sym && bc.sym.ident == Id.Object)
                    continue;
                if (any)
                    buf.writestring(", ");
                else
                {
                    buf.writestring(": ");
                    any = 1;
                }

                if (bc.sym)
                {
                    buf.printf("$(DDOC_PSUPER_SYMBOL %s)", bc.sym.toPrettyChars());
                }
                else
                {
                    HdrGenState hgs;
                    .toCBuffer(bc.type, buf, null, &hgs);
                }
            }
            buf.writestring(";\n");
        }

        override void visit(EnumDeclaration ed)
        {
            if (!ed.ident)
                return;
            buf.printf("%s %s", ed.kind(), ed.toChars());
            if (ed.memtype)
            {
                buf.writestring(": $(DDOC_ENUM_BASETYPE ");
                HdrGenState hgs;
                .toCBuffer(ed.memtype, buf, null, &hgs);
                buf.writestring(")");
            }
            buf.writestring(";\n");
        }

        override void visit(EnumMember em)
        {
            if (!em.ident)
                return;
            buf.writestring(em.toChars());
        }
    }

    scope ToDocBuffer v = new ToDocBuffer(buf, sc);
    s.accept(v);
}

/***********************************************************
 */
struct DocComment
{
    Sections sections;      // Section*[]
    Section summary;
    Section copyright;
    Section macros;
    MacroTable* pmacrotable;
    Escape* escapetable;
    Dsymbols a;

    static DocComment* parse(Dsymbol s, const(char)* comment)
    {
        //printf("parse(%s): '%s'\n", s.toChars(), comment);
        auto dc = new DocComment();
        dc.a.push(s);
        if (!comment)
            return dc;
        dc.parseSections(comment);
        for (size_t i = 0; i < dc.sections.length; i++)
        {
            Section sec = dc.sections[i];
            if (iequals("copyright", sec.name))
            {
                dc.copyright = sec;
            }
            if (iequals("macros", sec.name))
            {
                dc.macros = sec;
            }
        }
        return dc;
    }

    /************************************************
     * Parse macros out of Macros: section.
     * Macros are of the form:
     *      name1 = value1
     *
     *      name2 = value2
     */
    extern(D) static void parseMacros(
        Escape* escapetable, ref MacroTable pmacrotable, const(char)[] m)
    {
        const(char)* p = m.ptr;
        size_t len = m.length;
        const(char)* pend = p + len;
        const(char)* tempstart = null;
        size_t templen = 0;
        const(char)* namestart = null;
        size_t namelen = 0; // !=0 if line continuation
        const(char)* textstart = null;
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
                        goto Ltext; // continuation of prev macro
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
                    goto Ltext; // continuation of prev macro
                goto Lskipline;
            }
            p++;
            if (p >= pend)
                goto Ldone;
            if (namelen)
            {
                // Output existing macro
            L1:
                //printf("macro '%.*s' = '%.*s'\n", cast(int)namelen, namestart, cast(int)textlen, textstart);
                if (iequals("ESCAPES", namestart[0 .. namelen]))
                    parseEscapes(escapetable, textstart[0 .. textlen]);
                else
                    pmacrotable.define(namestart[0 .. namelen], textstart[0 .. textlen]);
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
            goto L1; // write out last one
    }

    /**************************************
     * Parse escapes of the form:
     *      /c/string/
     * where c is a single character.
     * Multiple escapes can be separated
     * by whitespace and/or commas.
     */
    static void parseEscapes(Escape* escapetable, const(char)[] text)
    {
        if (!escapetable)
        {
            escapetable = new Escape();
            memset(escapetable, 0, Escape.sizeof);
        }
        //printf("parseEscapes('%.*s') pescapetable = %p\n", cast(int)text.length, text.ptr, escapetable);
        const(char)* p = text.ptr;
        const(char)* pend = p + text.length;
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
            char c = p[1];
            p += 3;
            const(char)* start = p;
            while (1)
            {
                if (p >= pend)
                    return;
                if (*p == '/')
                    break;
                p++;
            }
            size_t len = p - start;
            char* s = cast(char*)memcpy(mem.xmalloc(len + 1), start, len);
            s[len] = 0;
            escapetable.strings[c] = s[0 .. len];
            //printf("\t%c = '%s'\n", c, s);
            p++;
        }
    }

    /*****************************************
     * Parse next paragraph out of *pcomment.
     * Update *pcomment to point past paragraph.
     * Returns NULL if no more paragraphs.
     * If paragraph ends in 'identifier:',
     * then (*pcomment)[0 .. idlen] is the identifier.
     */
    void parseSections(const(char)* comment)
    {
        const(char)* p;
        const(char)* pstart;
        const(char)* pend;
        const(char)* idstart = null; // dead-store to prevent spurious warning
        size_t idlen;
        const(char)* name = null;
        size_t namelen = 0;
        //printf("parseSections('%s')\n", comment);
        p = comment;
        while (*p)
        {
            const(char)* pstart0 = p;
            p = skipwhitespace(p);
            pstart = p;
            pend = p;

            // Undo indent if starting with a list item
            if ((*p == '-' || *p == '+' || *p == '*') && (*(p+1) == ' ' || *(p+1) == '\t'))
                pstart = pstart0;
            else
            {
                const(char)* pitem = p;
                while (*pitem >= '0' && *pitem <= '9')
                    ++pitem;
                if (pitem > p && *pitem == '.' && (*(pitem+1) == ' ' || *(pitem+1) == '\t'))
                    pstart = pstart0;
            }

            /* Find end of section, which is ended by one of:
             *      'identifier:' (but not inside a code section)
             *      '\0'
             */
            idlen = 0;
            int inCode = 0;
            while (1)
            {
                // Check for start/end of a code section
                if (*p == '-' || *p == '`' || *p == '~')
                {
                    char c = *p;
                    int numdash = 0;
                    while (*p == c)
                    {
                        ++numdash;
                        p++;
                    }
                    // BUG: handle UTF PS and LS too
                    if ((!*p || *p == '\r' || *p == '\n' || (!inCode && c != '-')) && numdash >= 3)
                    {
                        inCode = inCode == c ? false : c;
                        if (inCode)
                        {
                            // restore leading indentation
                            while (pstart0 < pstart && isIndentWS(pstart - 1))
                                --pstart;
                        }
                    }
                    pend = p;
                }
                if (!inCode && isIdStart(p))
                {
                    const(char)* q = p + utfStride(p);
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
                Section s;
                if (iequals("Params", name[0 .. namelen]))
                    s = new ParamSection();
                else if (iequals("Macros", name[0 .. namelen]))
                    s = new MacroSection();
                else
                    s = new Section();
                s.name = name[0 .. namelen];
                s.body_ = pstart[0 .. pend - pstart];
                s.nooutput = 0;
                //printf("Section: '%.*s' = '%.*s'\n", cast(int)s.namelen, s.name, cast(int)s.bodylen, s.body);
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
                name = null;
                namelen = 0;
                if (!*p)
                    break;
            }
        }
    }

    void writeSections(Scope* sc, Dsymbols* a, OutBuffer* buf)
    {
        assert(a.length);
        //printf("DocComment::writeSections()\n");
        Loc loc = (*a)[0].loc;
        if (Module m = (*a)[0].isModule())
        {
            if (m.md)
                loc = m.md.loc;
        }
        size_t offset1 = buf.length;
        buf.writestring("$(DDOC_SECTIONS ");
        size_t offset2 = buf.length;
        for (size_t i = 0; i < sections.length; i++)
        {
            Section sec = sections[i];
            if (sec.nooutput)
                continue;
            //printf("Section: '%.*s' = '%.*s'\n", cast(int)sec.namelen, sec.name, cast(int)sec.bodylen, sec.body);
            if (!sec.name.length && i == 0)
            {
                buf.writestring("$(DDOC_SUMMARY ");
                size_t o = buf.length;
                buf.write(sec.body_);
                escapeStrayParenthesis(loc, buf, o, true);
                highlightText(sc, a, loc, *buf, o);
                buf.writestring(")");
            }
            else
                sec.write(loc, &this, sc, a, buf);
        }
        for (size_t i = 0; i < a.length; i++)
        {
            Dsymbol s = (*a)[i];
            if (Dsymbol td = getEponymousParent(s))
                s = td;
            for (UnitTestDeclaration utd = s.ddocUnittest; utd; utd = utd.ddocUnittest)
            {
                if (utd.visibility.kind == Visibility.Kind.private_ || !utd.comment || !utd.fbody)
                    continue;
                // Strip whitespaces to avoid showing empty summary
                const(char)* c = utd.comment;
                while (*c == ' ' || *c == '\t' || *c == '\n' || *c == '\r')
                    ++c;
                buf.writestring("$(DDOC_EXAMPLES ");
                size_t o = buf.length;
                buf.writestring(cast(char*)c);
                if (utd.codedoc)
                {
                    auto codedoc = utd.codedoc.stripLeadingNewlines;
                    size_t n = getCodeIndent(codedoc);
                    while (n--)
                        buf.writeByte(' ');
                    buf.writestring("----\n");
                    buf.writestring(codedoc);
                    buf.writestring("----\n");
                    highlightText(sc, a, loc, *buf, o);
                }
                buf.writestring(")");
            }
        }
        if (buf.length == offset2)
        {
            /* Didn't write out any sections, so back out last write
             */
            buf.setsize(offset1);
            buf.writestring("\n");
        }
        else
            buf.writestring(")");
    }
}

/*****************************************
 * Return true if comment consists entirely of "ditto".
 */
private bool isDitto(const(char)* comment)
{
    if (comment)
    {
        const(char)* p = skipwhitespace(comment);
        if (Port.memicmp(p, "ditto", 5) == 0 && *skipwhitespace(p + 5) == 0)
            return true;
    }
    return false;
}

/**********************************************
 * Skip white space.
 */
private const(char)* skipwhitespace(const(char)* p)
{
    return skipwhitespace(p.toDString).ptr;
}

/// Ditto
private const(char)[] skipwhitespace(const(char)[] p)
{
    foreach (idx, char c; p)
    {
        switch (c)
        {
        case ' ':
        case '\t':
        case '\n':
            continue;
        default:
            return p[idx .. $];
        }
    }
    return p[$ .. $];
}

/************************************************
 * Scan past all instances of the given characters.
 * Params:
 *  buf           = an OutBuffer containing the DDoc
 *  i             = the index within `buf` to start scanning from
 *  chars         = the characters to skip; order is unimportant
 * Returns: the index after skipping characters.
 */
private size_t skipChars(ref OutBuffer buf, size_t i, string chars)
{
    Outer:
    foreach (j, c; buf[][i..$])
    {
        foreach (d; chars)
        {
            if (d == c)
                continue Outer;
        }
        return i + j;
    }
    return buf.length;
}

unittest {
    OutBuffer buf;
    string data = "test ---\r\n\r\nend";
    buf.write(data);

    assert(skipChars(buf, 0, "-") == 0);
    assert(skipChars(buf, 4, "-") == 4);
    assert(skipChars(buf, 4, " -") == 8);
    assert(skipChars(buf, 8, "\r\n") == 12);
    assert(skipChars(buf, 12, "dne") == 15);
}

/****************************************************
 * Replace all instances of `c` with `r` in the given string
 * Params:
 *  s = the string to do replacements in
 *  c = the character to look for
 *  r = the string to replace `c` with
 * Returns: `s` with `c` replaced with `r`
 */
private inout(char)[] replaceChar(inout(char)[] s, char c, string r) pure
{
    int count = 0;
    foreach (char sc; s)
        if (sc == c)
            ++count;
    if (count == 0)
        return s;

    char[] result;
    result.reserve(s.length - count + (r.length * count));
    size_t start = 0;
    foreach (i, char sc; s)
    {
        if (sc == c)
        {
            result ~= s[start..i];
            result ~= r;
            start = i+1;
        }
    }
    result ~= s[start..$];
    return result;
}

///
unittest
{
    assert("".replaceChar(',', "$(COMMA)") == "");
    assert("ab".replaceChar(',', "$(COMMA)") == "ab");
    assert("a,b".replaceChar(',', "$(COMMA)") == "a$(COMMA)b");
    assert("a,,b".replaceChar(',', "$(COMMA)") == "a$(COMMA)$(COMMA)b");
    assert(",ab".replaceChar(',', "$(COMMA)") == "$(COMMA)ab");
    assert("ab,".replaceChar(',', "$(COMMA)") == "ab$(COMMA)");
}

/**
 * Return a lowercased copy of a string.
 * Params:
 *  s = the string to lowercase
 * Returns: the lowercase version of the string or the original if already lowercase
 */
private string toLowercase(string s) pure
{
    string lower;
    foreach (size_t i; 0..s.length)
    {
        char c = s[i];
// TODO: maybe unicode lowercase, somehow
        if (c >= 'A' && c <= 'Z')
        {
            if (!lower.length) {
                lower.reserve(s.length);
            }
            lower ~= s[lower.length..i];
            c += 'a' - 'A';
            lower ~= c;
        }
    }
    if (lower.length)
        lower ~= s[lower.length..$];
    else
        lower = s;
    return lower;
}

///
unittest
{
    assert("".toLowercase == "");
    assert("abc".toLowercase == "abc");
    assert("ABC".toLowercase == "abc");
    assert("aBc".toLowercase == "abc");
}

/************************************************
 * Get the indent from one index to another, counting tab stops as four spaces wide
 * per the Markdown spec.
 * Params:
 *  buf   = an OutBuffer containing the DDoc
 *  from  = the index within `buf` to start counting from, inclusive
 *  to    = the index within `buf` to stop counting at, exclusive
 * Returns: the indent
 */
private int getMarkdownIndent(ref OutBuffer buf, size_t from, size_t to)
{
    const slice = buf[];
    if (to > slice.length)
        to = slice.length;
    int indent = 0;
    foreach (const c; slice[from..to])
        indent += (c == '\t') ? 4 - (indent % 4) : 1;
    return indent;
}

/************************************************
 * Scan forward to one of:
 *      start of identifier
 *      beginning of next line
 *      end of buf
 */
size_t skiptoident(ref OutBuffer buf, size_t i)
{
    const slice = buf[];
    while (i < slice.length)
    {
        dchar c;
        size_t oi = i;
        if (utf_decodeChar(slice, i, c))
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
private size_t skippastident(ref OutBuffer buf, size_t i)
{
    const slice = buf[];
    while (i < slice.length)
    {
        dchar c;
        size_t oi = i;
        if (utf_decodeChar(slice, i, c))
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
 * Scan forward past end of an identifier that might
 * contain dots (e.g. `abc.def`)
 */
private size_t skipPastIdentWithDots(ref OutBuffer buf, size_t i)
{
    const slice = buf[];
    bool lastCharWasDot;
    while (i < slice.length)
    {
        dchar c;
        size_t oi = i;
        if (utf_decodeChar(slice, i, c))
        {
            /* Ignore UTF errors, but still consume input
             */
            break;
        }
        if (c == '.')
        {
            // We need to distinguish between `abc.def`, abc..def`, and `abc.`
            // Only `abc.def` is a valid identifier

            if (lastCharWasDot)
            {
                i = oi;
                break;
            }

            lastCharWasDot = true;
            continue;
        }
        else
        {
            if (c >= 0x80)
            {
                if (isUniAlpha(c))
                {
                    lastCharWasDot = false;
                    continue;
                }
            }
            else if (isalnum(c) || c == '_')
            {
                lastCharWasDot = false;
                continue;
            }
            i = oi;
            break;
        }
    }

    // if `abc.`
    if (lastCharWasDot)
        return i - 1;

    return i;
}

/************************************************
 * Scan forward past URL starting at i.
 * We don't want to highlight parts of a URL.
 * Returns:
 *      i if not a URL
 *      index just past it if it is a URL
 */
private size_t skippastURL(ref OutBuffer buf, size_t i)
{
    const slice = buf[][i .. $];
    size_t j;
    bool sawdot = false;
    if (slice.length > 7 && Port.memicmp(slice.ptr, "http://", 7) == 0)
    {
        j = 7;
    }
    else if (slice.length > 8 && Port.memicmp(slice.ptr, "https://", 8) == 0)
    {
        j = 8;
    }
    else
        goto Lno;
    for (; j < slice.length; j++)
    {
        const c = slice[j];
        if (isalnum(c))
            continue;
        if (c == '-' || c == '_' || c == '?' || c == '=' || c == '%' ||
            c == '&' || c == '/' || c == '+' || c == '#' || c == '~')
            continue;
        if (c == '.')
        {
            sawdot = true;
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
 * Remove a previously-inserted blank line macro.
 * Params:
 *  buf           = an OutBuffer containing the DDoc
 *  iAt           = the index within `buf` of the start of the `$(DDOC_BLANKLINE)`
 *                  macro. Upon function return its value is set to `0`.
 *  i             = an index within `buf`. If `i` is after `iAt` then it gets
 *                  reduced by the length of the removed macro.
 */
private void removeBlankLineMacro(ref OutBuffer buf, ref size_t iAt, ref size_t i)
{
    if (!iAt)
        return;

    enum macroLength = "$(DDOC_BLANKLINE)".length;
    buf.remove(iAt, macroLength);
    if (i > iAt)
        i -= macroLength;
    iAt = 0;
}

/****************************************************
 * Attempt to detect and replace a Markdown thematic break (HR). These are three
 * or more of the same delimiter, optionally with spaces or tabs between any of
 * them, e.g. `\n- - -\n` becomes `\n$(HR)\n`
 * Params:
 *  buf         = an OutBuffer containing the DDoc
 *  i           = the index within `buf` of the first character of a potential
 *                thematic break. If the replacement is made `i` changes to
 *                point to the closing parenthesis of the `$(HR)` macro.
 *  iLineStart  = the index within `buf` that the thematic break's line starts at
 *  loc         = the current location within the file
 * Returns: whether a thematic break was replaced
 */
private bool replaceMarkdownThematicBreak(ref OutBuffer buf, ref size_t i, size_t iLineStart, const ref Loc loc)
{

    const slice = buf[];
    const c = buf[i];
    size_t j = i + 1;
    int repeat = 1;
    for (; j < slice.length; j++)
    {
        if (buf[j] == c)
            ++repeat;
        else if (buf[j] != ' ' && buf[j] != '\t')
            break;
    }
    if (repeat >= 3)
    {
        if (j >= buf.length || buf[j] == '\n' || buf[j] == '\r')
        {
            buf.remove(iLineStart, j - iLineStart);
            i = buf.insert(iLineStart, "$(HR)") - 1;
            return true;
        }
    }
    return false;
}

/****************************************************
 * Detect the level of an ATX-style heading, e.g. `## This is a heading` would
 * have a level of `2`.
 * Params:
 *  buf   = an OutBuffer containing the DDoc
 *  i     = the index within `buf` of the first `#` character
 * Returns:
 *          the detected heading level from 1 to 6, or
 *          0 if not at an ATX heading
 */
private int detectAtxHeadingLevel(ref OutBuffer buf, const size_t i)
{
    const iHeadingStart = i;
    const iAfterHashes = skipChars(buf, i, "#");
    const headingLevel = cast(int) (iAfterHashes - iHeadingStart);
    if (headingLevel > 6)
        return 0;

    const iTextStart = skipChars(buf, iAfterHashes, " \t");
    const emptyHeading = buf[iTextStart] == '\r' || buf[iTextStart] == '\n';

    // require whitespace
    if (!emptyHeading && iTextStart == iAfterHashes)
        return 0;

    return headingLevel;
}

/****************************************************
 * Remove any trailing `##` suffix from an ATX-style heading.
 * Params:
 *  buf   = an OutBuffer containing the DDoc
 *  i     = the index within `buf` to start looking for a suffix at
 */
private void removeAnyAtxHeadingSuffix(ref OutBuffer buf, size_t i)
{
    size_t j = i;
    size_t iSuffixStart = 0;
    size_t iWhitespaceStart = j;
    const slice = buf[];
    for (; j < slice.length; j++)
    {
        switch (slice[j])
        {
        case '#':
            if (iWhitespaceStart && !iSuffixStart)
                iSuffixStart = j;
            continue;
        case ' ':
        case '\t':
            if (!iWhitespaceStart)
                iWhitespaceStart = j;
            continue;
        case '\r':
        case '\n':
            break;
        default:
            iSuffixStart = 0;
            iWhitespaceStart = 0;
            continue;
        }
        break;
    }
    if (iSuffixStart)
        buf.remove(iWhitespaceStart, j - iWhitespaceStart);
}

/****************************************************
 * Wrap text in a Markdown heading macro, e.g. `$(H2 heading text`).
 * Params:
 *  buf           = an OutBuffer containing the DDoc
 *  iStart        = the index within `buf` that the Markdown heading starts at
 *  iEnd          = the index within `buf` of the character after the last
 *                  heading character. Is incremented by the length of the
 *                  inserted heading macro when this function ends.
 *  loc           = the location of the Ddoc within the file
 *  headingLevel  = the level (1-6) of heading to end. Is set to `0` when this
 *                  function ends.
 */
private void endMarkdownHeading(ref OutBuffer buf, size_t iStart, ref size_t iEnd, const ref Loc loc, ref int headingLevel)
{
    char[5] heading = "$(H0 ";
    heading[3] = cast(char) ('0' + headingLevel);
    buf.insert(iStart, heading);
    iEnd += 5;
    size_t iBeforeNewline = iEnd;
    while (buf[iBeforeNewline-1] == '\r' || buf[iBeforeNewline-1] == '\n')
        --iBeforeNewline;
    buf.insert(iBeforeNewline, ")");
    headingLevel = 0;
}

/****************************************************
 * End all nested Markdown quotes, if inside any.
 * Params:
 *  buf         = an OutBuffer containing the DDoc
 *  i           = the index within `buf` of the character after the quote text.
 *  quoteLevel  = the current quote level. Is set to `0` when this function ends.
 * Returns: the amount that `i` was moved
 */
private size_t endAllMarkdownQuotes(ref OutBuffer buf, size_t i, ref int quoteLevel)
{
    const length = quoteLevel;
    for (; quoteLevel > 0; --quoteLevel)
        i = buf.insert(i, ")");
    return length;
}

/****************************************************
 * Convenience function to end all Markdown lists and quotes, if inside any, and
 * set `quoteMacroLevel` to `0`.
 * Params:
 *  buf         = an OutBuffer containing the DDoc
 *  i           = the index within `buf` of the character after the list and/or
 *                quote text. Is adjusted when this function ends if any lists
 *                and/or quotes were ended.
 *  nestedLists = a set of nested lists. Upon return it will be empty.
 *  quoteLevel  = the current quote level. Is set to `0` when this function ends.
 *  quoteMacroLevel   = the macro level that the quote was started at. Is set to
 *                      `0` when this function ends.
 * Returns: the amount that `i` was moved
 */
private size_t endAllListsAndQuotes(ref OutBuffer buf, ref size_t i, ref MarkdownList[] nestedLists, ref int quoteLevel, out int quoteMacroLevel)
{
    quoteMacroLevel = 0;
    const i0 = i;
    i += MarkdownList.endAllNestedLists(buf, i, nestedLists);
    i += endAllMarkdownQuotes(buf, i, quoteLevel);
    return i - i0;
}

/****************************************************
 * Replace Markdown emphasis with the appropriate macro,
 * e.g. `*very* **nice**` becomes `$(EM very) $(STRONG nice)`.
 * Params:
 *  buf               = an OutBuffer containing the DDoc
 *  loc               = the current location within the file
 *  inlineDelimiters  = the collection of delimiters found within a paragraph. When this function returns its length will be reduced to `downToLevel`.
 *  downToLevel       = the length within `inlineDelimiters`` to reduce emphasis to
 * Returns: the number of characters added to the buffer by the replacements
 */
private size_t replaceMarkdownEmphasis(ref OutBuffer buf, const ref Loc loc, ref MarkdownDelimiter[] inlineDelimiters, int downToLevel = 0)
{
    size_t replaceEmphasisPair(ref MarkdownDelimiter start, ref MarkdownDelimiter end)
    {
        immutable count = start.count == 1 || end.count == 1 ? 1 : 2;

        size_t iStart = start.iStart;
        size_t iEnd = end.iStart;
        end.count -= count;
        start.count -= count;
        iStart += start.count;

        if (!start.count)
            start.type = 0;
        if (!end.count)
            end.type = 0;

        buf.remove(iStart, count);
        iEnd -= count;
        buf.remove(iEnd, count);

        string macroName = count >= 2 ? "$(STRONG " : "$(EM ";
        buf.insert(iEnd, ")");
        buf.insert(iStart, macroName);

        const delta = 1 + macroName.length - (count + count);
        end.iStart += count;
        return delta;
    }

    size_t delta = 0;
    int start = (cast(int) inlineDelimiters.length) - 1;
    while (start >= downToLevel)
    {
        // find start emphasis
        while (start >= downToLevel &&
            (inlineDelimiters[start].type != '*' || !inlineDelimiters[start].leftFlanking))
            --start;
        if (start < downToLevel)
            break;

        // find the nearest end emphasis
        int end = start + 1;
        while (end < inlineDelimiters.length &&
            (inlineDelimiters[end].type != inlineDelimiters[start].type ||
                inlineDelimiters[end].macroLevel != inlineDelimiters[start].macroLevel ||
                !inlineDelimiters[end].rightFlanking))
            ++end;
        if (end == inlineDelimiters.length)
        {
            // the start emphasis has no matching end; if it isn't an end itself then kill it
            if (!inlineDelimiters[start].rightFlanking)
                inlineDelimiters[start].type = 0;
            --start;
            continue;
        }

        // multiple-of-3 rule
        if (((inlineDelimiters[start].leftFlanking && inlineDelimiters[start].rightFlanking) ||
                (inlineDelimiters[end].leftFlanking && inlineDelimiters[end].rightFlanking)) &&
            (inlineDelimiters[start].count + inlineDelimiters[end].count) % 3 == 0)
        {
            --start;
            continue;
        }

        immutable delta0 = replaceEmphasisPair(inlineDelimiters[start], inlineDelimiters[end]);

        for (; end < inlineDelimiters.length; ++end)
            inlineDelimiters[end].iStart += delta0;
        delta += delta0;
    }

    inlineDelimiters.length = downToLevel;
    return delta;
}

/****************************************************
 */
private bool isIdentifier(Dsymbols* a, const(char)[] s)
{
    foreach (member; *a)
    {
        if (auto imp = member.isImport())
        {
            // For example: `public import str = core.stdc.string;`
            // This checks if `s` is equal to `str`
            if (imp.aliasId)
            {
                if (s == imp.aliasId.toString())
                    return true;
            }
            else
            {
                // The general case:  `public import core.stdc.string;`

                // fully qualify imports so `core.stdc.string` doesn't appear as `core`
                string fullyQualifiedImport;
                foreach (const pid; imp.packages)
                {
                    fullyQualifiedImport ~= pid.toString() ~ ".";
                }
                fullyQualifiedImport ~= imp.id.toString();

                // Check if `s` == `core.stdc.string`
                if (s == fullyQualifiedImport)
                    return true;
            }
        }
        else if (member.ident)
        {
            if (s == member.ident.toString())
                return true;
        }

    }
    return false;
}

/****************************************************
 */
private bool isKeyword(const(char)[] str) @safe
{
    immutable string[3] table = ["true", "false", "null"];
    foreach (s; table)
    {
        if (str == s)
            return true;
    }
    return false;
}

/****************************************************
 */
private TypeFunction isTypeFunction(Dsymbol s) @safe
{
    FuncDeclaration f = s.isFuncDeclaration();
    /* f.type may be NULL for template members.
     */
    if (f && f.type)
    {
        Type t = f.originalType ? f.originalType : f.type;
        if (t.ty == Tfunction)
            return cast(TypeFunction)t;
    }
    return null;
}

/****************************************************
 */
private Parameter isFunctionParameter(Dsymbol s, const(char)[] str) @safe
{
    TypeFunction tf = isTypeFunction(s);
    if (tf && tf.parameterList.parameters)
    {
        foreach (fparam; *tf.parameterList.parameters)
        {
            if (fparam.ident && str == fparam.ident.toString())
            {
                return fparam;
            }
        }
    }
    return null;
}

/****************************************************
 */
private Parameter isFunctionParameter(Dsymbols* a, const(char)[] p) @safe
{
    foreach (Dsymbol sym; *a)
    {
        Parameter fparam = isFunctionParameter(sym, p);
        if (fparam)
        {
            return fparam;
        }
    }
    return null;
}

/****************************************************
 */
private Parameter isEponymousFunctionParameter(Dsymbols *a, const(char)[] p) @safe
{
    foreach (Dsymbol dsym; *a)
    {
        TemplateDeclaration td = dsym.isTemplateDeclaration();
        if (td && td.onemember)
        {
            /* Case 1: we refer to a template declaration inside the template

               /// ...ddoc...
               template case1(T) {
                 void case1(R)() {}
               }
             */
            td = td.onemember.isTemplateDeclaration();
        }
        if (!td)
        {
            /* Case 2: we're an alias to a template declaration

               /// ...ddoc...
               alias case2 = case1!int;
             */
            AliasDeclaration ad = dsym.isAliasDeclaration();
            if (ad && ad.aliassym)
            {
                td = ad.aliassym.isTemplateDeclaration();
            }
        }
        while (td)
        {
            Dsymbol sym = getEponymousMember(td);
            if (sym)
            {
                Parameter fparam = isFunctionParameter(sym, p);
                if (fparam)
                {
                    return fparam;
                }
            }
            td = td.overnext;
        }
    }
    return null;
}

/****************************************************
 */
private TemplateParameter isTemplateParameter(Dsymbols* a, const(char)* p, size_t len)
{
    for (size_t i = 0; i < a.length; i++)
    {
        TemplateDeclaration td = (*a)[i].isTemplateDeclaration();
        // Check for the parent, if the current symbol is not a template declaration.
        if (!td)
            td = getEponymousParent((*a)[i]);
        if (td && td.origParameters)
        {
            foreach (tp; *td.origParameters)
            {
                if (tp.ident && p[0 .. len] == tp.ident.toString())
                {
                    return tp;
                }
            }
        }
    }
    return null;
}

/****************************************************
 * Return true if str is a reserved symbol name
 * that starts with a double underscore.
 */
private bool isReservedName(const(char)[] str)
{
    immutable string[] table =
    [
        "__ctor",
        "__dtor",
        "__postblit",
        "__invariant",
        "__unitTest",
        "__require",
        "__ensure",
        "__dollar",
        "__ctfe",
        "__withSym",
        "__result",
        "__returnLabel",
        "__vptr",
        "__monitor",
        "__gate",
        "__xopEquals",
        "__xopCmp",
        "__LINE__",
        "__FILE__",
        "__MODULE__",
        "__FUNCTION__",
        "__PRETTY_FUNCTION__",
        "__DATE__",
        "__TIME__",
        "__TIMESTAMP__",
        "__VENDOR__",
        "__VERSION__",
        "__EOF__",
        "__CXXLIB__",
        "__LOCAL_SIZE",
        "__entrypoint",
    ];
    foreach (s; table)
    {
        if (str == s)
            return true;
    }
    return false;
}

/****************************************************
 * A delimiter for Markdown inline content like emphasis and links.
 */
private struct MarkdownDelimiter
{
    size_t iStart;  /// the index where this delimiter starts
    int count;      /// the length of this delimeter's start sequence
    int macroLevel; /// the count of nested DDoc macros when the delimiter is started
    bool leftFlanking;  /// whether the delimiter is left-flanking, as defined by the CommonMark spec
    bool rightFlanking; /// whether the delimiter is right-flanking, as defined by the CommonMark spec
    bool atParagraphStart;  /// whether the delimiter is at the start of a paragraph
    char type;      /// the type of delimiter, defined by its starting character

    /// whether this describes a valid delimiter
    @property bool isValid() const { return count != 0; }

    /// flag this delimiter as invalid
    void invalidate() { count = 0; }
}

/****************************************************
 * Info about a Markdown list.
 */
private struct MarkdownList
{
    string orderedStart;    /// an optional start number--if present then the list starts at this number
    size_t iStart;          /// the index where the list item starts
    size_t iContentStart;   /// the index where the content starts after the list delimiter
    int delimiterIndent;    /// the level of indent the list delimiter starts at
    int contentIndent;      /// the level of indent the content starts at
    int macroLevel;         /// the count of nested DDoc macros when the list is started
    char type;              /// the type of list, defined by its starting character

    /// whether this describes a valid list
    @property bool isValid() const { return type != type.init; }

    /****************************************************
     * Try to parse a list item, returning whether successful.
     * Params:
     *  buf           = an OutBuffer containing the DDoc
     *  iLineStart    = the index within `buf` of the first character of the line
     *  i             = the index within `buf` of the potential list item
     * Returns: the parsed list item. Its `isValid` property describes whether parsing succeeded.
     */
    static MarkdownList parseItem(ref OutBuffer buf, size_t iLineStart, size_t i)
    {
        if (buf[i] == '+' || buf[i] == '-' || buf[i] == '*')
            return parseUnorderedListItem(buf, iLineStart, i);
        else
            return parseOrderedListItem(buf, iLineStart, i);
    }

    /****************************************************
     * Return whether the context is at a list item of the same type as this list.
     * Params:
     *  buf           = an OutBuffer containing the DDoc
     *  iLineStart    = the index within `buf` of the first character of the line
     *  i             = the index within `buf` of the list item
     * Returns: whether `i` is at a list item of the same type as this list
     */
    private bool isAtItemInThisList(ref OutBuffer buf, size_t iLineStart, size_t i)
    {
        MarkdownList item = (type == '.' || type == ')') ?
            parseOrderedListItem(buf, iLineStart, i) :
            parseUnorderedListItem(buf, iLineStart, i);
        if (item.type == type)
            return item.delimiterIndent < contentIndent && item.contentIndent > delimiterIndent;
        return false;
    }

    /****************************************************
     * Start a Markdown list item by creating/deleting nested lists and starting the item.
     * Params:
     *  buf           = an OutBuffer containing the DDoc
     *  iLineStart    = the index within `buf` of the first character of the line. If this function succeeds it will be adjuested to equal `i`.
     *  i             = the index within `buf` of the list item. If this function succeeds `i` will be adjusted to fit the inserted macro.
     *  iPrecedingBlankLine = the index within `buf` of the preceeding blank line. If non-zero and a new list was started, the preceeding blank line is removed and this value is set to `0`.
     *  nestedLists   = a set of nested lists. If this function succeeds it may contain a new nested list.
     *  loc           = the location of the Ddoc within the file
     * Returns: `true` if a list was created
     */
    bool startItem(ref OutBuffer buf, ref size_t iLineStart, ref size_t i, ref size_t iPrecedingBlankLine, ref MarkdownList[] nestedLists, const ref Loc loc)
    {
        buf.remove(iStart, iContentStart - iStart);

        if (!nestedLists.length ||
            delimiterIndent >= nestedLists[$-1].contentIndent ||
            buf[iLineStart - 4..iLineStart] == "$(LI")
        {
            // start a list macro
            nestedLists ~= this;
            if (type == '.')
            {
                if (orderedStart.length)
                {
                    iStart = buf.insert(iStart, "$(OL_START ");
                    iStart = buf.insert(iStart, orderedStart);
                    iStart = buf.insert(iStart, ",\n");
                }
                else
                    iStart = buf.insert(iStart, "$(OL\n");
            }
            else
                iStart = buf.insert(iStart, "$(UL\n");

            removeBlankLineMacro(buf, iPrecedingBlankLine, iStart);
        }
        else if (nestedLists.length)
        {
            nestedLists[$-1].delimiterIndent = delimiterIndent;
            nestedLists[$-1].contentIndent = contentIndent;
        }

        iStart = buf.insert(iStart, "$(LI\n");
        i = iStart - 1;
        iLineStart = i;

        return true;
    }

    /****************************************************
     * End all nested Markdown lists.
     * Params:
     *  buf           = an OutBuffer containing the DDoc
     *  i             = the index within `buf` to end lists at.
     *  nestedLists   = a set of nested lists. Upon return it will be empty.
     * Returns: the amount that `i` changed
     */
    static size_t endAllNestedLists(ref OutBuffer buf, size_t i, ref MarkdownList[] nestedLists)
    {
        const iStart = i;
        for (; nestedLists.length; --nestedLists.length)
            i = buf.insert(i, ")\n)");
        return i - iStart;
    }

    /****************************************************
     * Look for a sibling list item or the end of nested list(s).
     * Params:
     *  buf               = an OutBuffer containing the DDoc
     *  i                 = the index within `buf` to end lists at. If there was a sibling or ending lists `i` will be adjusted to fit the macro endings.
     *  iParagraphStart   = the index within `buf` to start the next paragraph at at. May be adjusted upon return.
     *  nestedLists       = a set of nested lists. Some nested lists may have been removed from it upon return.
     */
    static void handleSiblingOrEndingList(ref OutBuffer buf, ref size_t i, ref size_t iParagraphStart, ref MarkdownList[] nestedLists)
    {
        size_t iAfterSpaces = skipChars(buf, i + 1, " \t");

        if (nestedLists[$-1].isAtItemInThisList(buf, i + 1, iAfterSpaces))
        {
            // end a sibling list item
            i = buf.insert(i, ")");
            iParagraphStart = skipChars(buf, i, " \t\r\n");
        }
        else if (iAfterSpaces >= buf.length || (buf[iAfterSpaces] != '\r' && buf[iAfterSpaces] != '\n'))
        {
            // end nested lists that are indented more than this content
            const indent = getMarkdownIndent(buf, i + 1, iAfterSpaces);
            while (nestedLists.length && nestedLists[$-1].contentIndent > indent)
            {
                i = buf.insert(i, ")\n)");
                --nestedLists.length;
                iParagraphStart = skipChars(buf, i, " \t\r\n");

                if (nestedLists.length && nestedLists[$-1].isAtItemInThisList(buf, i + 1, iParagraphStart))
                {
                    i = buf.insert(i, ")");
                    ++iParagraphStart;
                    break;
                }
            }
        }
    }

    /****************************************************
     * Parse an unordered list item at the current position
     * Params:
     *  buf           = an OutBuffer containing the DDoc
     *  iLineStart    = the index within `buf` of the first character of the line
     *  i             = the index within `buf` of the list item
     * Returns: the parsed list item, or a list item with type `.init` if no list item is available
     */
    private static MarkdownList parseUnorderedListItem(ref OutBuffer buf, size_t iLineStart, size_t i)
    {
        if (i+1 < buf.length &&
                (buf[i] == '-' ||
                buf[i] == '*' ||
                buf[i] == '+') &&
            (buf[i+1] == ' ' ||
                buf[i+1] == '\t' ||
                buf[i+1] == '\r' ||
                buf[i+1] == '\n'))
        {
            const iContentStart = skipChars(buf, i + 1, " \t");
            const delimiterIndent = getMarkdownIndent(buf, iLineStart, i);
            const contentIndent = getMarkdownIndent(buf, iLineStart, iContentStart);
            auto list = MarkdownList(null, iLineStart, iContentStart, delimiterIndent, contentIndent, 0, buf[i]);
            return list;
        }
        return MarkdownList();
    }

    /****************************************************
     * Parse an ordered list item at the current position
     * Params:
     *  buf           = an OutBuffer containing the DDoc
     *  iLineStart    = the index within `buf` of the first character of the line
     *  i             = the index within `buf` of the list item
     * Returns: the parsed list item, or a list item with type `.init` if no list item is available
     */
    private static MarkdownList parseOrderedListItem(ref OutBuffer buf, size_t iLineStart, size_t i)
    {
        size_t iAfterNumbers = skipChars(buf, i, "0123456789");
        if (iAfterNumbers - i > 0 &&
            iAfterNumbers - i <= 9 &&
            iAfterNumbers + 1 < buf.length &&
            buf[iAfterNumbers] == '.' &&
            (buf[iAfterNumbers+1] == ' ' ||
                buf[iAfterNumbers+1] == '\t' ||
                buf[iAfterNumbers+1] == '\r' ||
                buf[iAfterNumbers+1] == '\n'))
        {
            const iContentStart = skipChars(buf, iAfterNumbers + 1, " \t");
            const delimiterIndent = getMarkdownIndent(buf, iLineStart, i);
            const contentIndent = getMarkdownIndent(buf, iLineStart, iContentStart);
            size_t iNumberStart = skipChars(buf, i, "0");
            if (iNumberStart == iAfterNumbers)
                --iNumberStart;
            auto orderedStart = buf[][iNumberStart .. iAfterNumbers];
            if (orderedStart == "1")
                orderedStart = null;
            return MarkdownList(orderedStart.idup, iLineStart, iContentStart, delimiterIndent, contentIndent, 0, buf[iAfterNumbers]);
        }
        return MarkdownList();
    }
}

/****************************************************
 * A Markdown link.
 */
private struct MarkdownLink
{
    string href;    /// the link destination
    string title;   /// an optional title for the link
    string label;   /// an optional label for the link
    Dsymbol symbol; /// an optional symbol to link to

    /****************************************************
     * Replace a Markdown link or link definition in the form of:
     * - Inline link: `[foo](url/ 'optional title')`
     * - Reference link: `[foo][bar]`, `[foo][]` or `[foo]`
     * - Link reference definition: `[bar]: url/ 'optional title'`
     * Params:
     *  buf               = an OutBuffer containing the DDoc
     *  i                 = the index within `buf` that points to the `]` character of the potential link.
     *                      If this function succeeds it will be adjusted to fit the inserted link macro.
     *  loc               = the current location within the file
     *  inlineDelimiters  = previously parsed Markdown delimiters, including emphasis and link/image starts
     *  delimiterIndex    = the index within `inlineDelimiters` of the nearest link/image starting delimiter
     *  linkReferences    = previously parsed link references. When this function returns it may contain
     *                      additional previously unparsed references.
     * Returns: whether a reference link was found and replaced at `i`
     */
    static bool replaceLink(ref OutBuffer buf, ref size_t i, const ref Loc loc, ref MarkdownDelimiter[] inlineDelimiters, int delimiterIndex, ref MarkdownLinkReferences linkReferences)
    {
        const delimiter = inlineDelimiters[delimiterIndex];
        MarkdownLink link;

        size_t iEnd = link.parseReferenceDefinition(buf, i, delimiter);
        if (iEnd > i)
        {
            i = delimiter.iStart;
            link.storeAndReplaceDefinition(buf, i, iEnd, linkReferences, loc);
            inlineDelimiters.length = delimiterIndex;
            return true;
        }

        iEnd = link.parseInlineLink(buf, i);
        if (iEnd == i)
        {
            iEnd = link.parseReferenceLink(buf, i, delimiter);
            if (iEnd > i)
            {
                const label = link.label;
                link = linkReferences.lookupReference(label, buf, i, loc);
                // check rightFlanking to avoid replacing things like int[string]
                if (!link.href.length && !delimiter.rightFlanking)
                    link = linkReferences.lookupSymbol(label);
                if (!link.href.length)
                    return false;
            }
        }

        if (iEnd == i)
            return false;

        immutable delta = replaceMarkdownEmphasis(buf, loc, inlineDelimiters, delimiterIndex);
        iEnd += delta;
        i += delta;
        link.replaceLink(buf, i, iEnd, delimiter);
        return true;
    }

    /****************************************************
     * Replace a Markdown link definition in the form of `[bar]: url/ 'optional title'`
     * Params:
     *  buf               = an OutBuffer containing the DDoc
     *  i                 = the index within `buf` that points to the `]` character of the potential link.
     *                      If this function succeeds it will be adjusted to fit the inserted link macro.
     *  inlineDelimiters  = previously parsed Markdown delimiters, including emphasis and link/image starts
     *  delimiterIndex    = the index within `inlineDelimiters` of the nearest link/image starting delimiter
     *  linkReferences    = previously parsed link references. When this function returns it may contain
     *                      additional previously unparsed references.
     *  loc               = the current location in the file
     * Returns: whether a reference link was found and replaced at `i`
     */
    static bool replaceReferenceDefinition(ref OutBuffer buf, ref size_t i, ref MarkdownDelimiter[] inlineDelimiters, int delimiterIndex, ref MarkdownLinkReferences linkReferences, const ref Loc loc)
    {
        const delimiter = inlineDelimiters[delimiterIndex];
        MarkdownLink link;
        size_t iEnd = link.parseReferenceDefinition(buf, i, delimiter);
        if (iEnd == i)
            return false;

        i = delimiter.iStart;
        link.storeAndReplaceDefinition(buf, i, iEnd, linkReferences, loc);
        inlineDelimiters.length = delimiterIndex;
        return true;
    }

    /****************************************************
     * Parse a Markdown inline link in the form of `[foo](url/ 'optional title')`
     * Params:
     *  buf   = an OutBuffer containing the DDoc
     *  i     = the index within `buf` that points to the `]` character of the inline link.
     * Returns: the index at the end of parsing the link, or `i` if parsing failed.
     */
    private size_t parseInlineLink(ref OutBuffer buf, size_t i)
    {
        size_t iEnd = i + 1;
        if (iEnd >= buf.length || buf[iEnd] != '(')
            return i;
        ++iEnd;

        if (!parseHref(buf, iEnd))
            return i;

        iEnd = skipChars(buf, iEnd, " \t\r\n");
        if (buf[iEnd] != ')')
        {
            if (parseTitle(buf, iEnd))
                iEnd = skipChars(buf, iEnd, " \t\r\n");
        }

        if (buf[iEnd] != ')')
            return i;

        return iEnd + 1;
    }

    /****************************************************
     * Parse a Markdown reference link in the form of `[foo][bar]`, `[foo][]` or `[foo]`
     * Params:
     *  buf       = an OutBuffer containing the DDoc
     *  i         = the index within `buf` that points to the `]` character of the inline link.
     *  delimiter = the delimiter that starts this link
     * Returns: the index at the end of parsing the link, or `i` if parsing failed.
     */
    private size_t parseReferenceLink(ref OutBuffer buf, size_t i, MarkdownDelimiter delimiter)
    {
        size_t iStart = i + 1;
        size_t iEnd = iStart;
        if (iEnd >= buf.length || buf[iEnd] != '[' || (iEnd+1 < buf.length && buf[iEnd+1] == ']'))
        {
            // collapsed reference [foo][] or shortcut reference [foo]
            iStart = delimiter.iStart + delimiter.count - 1;
            if (buf[iEnd] == '[')
                iEnd += 2;
        }

        parseLabel(buf, iStart);
        if (!label.length)
            return i;

        if (iEnd < iStart)
            iEnd = iStart;
        return iEnd;
    }

    /****************************************************
     * Parse a Markdown reference definition in the form of `[bar]: url/ 'optional title'`
     * Params:
     *  buf               = an OutBuffer containing the DDoc
     *  i                 = the index within `buf` that points to the `]` character of the inline link.
     *  delimiter = the delimiter that starts this link
     * Returns: the index at the end of parsing the link, or `i` if parsing failed.
     */
    private size_t parseReferenceDefinition(ref OutBuffer buf, size_t i, MarkdownDelimiter delimiter)
    {
        if (!delimiter.atParagraphStart || delimiter.type != '[' ||
            i+1 >= buf.length || buf[i+1] != ':')
            return i;

        size_t iEnd = delimiter.iStart;
        parseLabel(buf, iEnd);
        if (label.length == 0 || iEnd != i + 1)
            return i;

        ++iEnd;
        iEnd = skipChars(buf, iEnd, " \t");
        skipOneNewline(buf, iEnd);

        if (!parseHref(buf, iEnd) || href.length == 0)
            return i;

        iEnd = skipChars(buf, iEnd, " \t");
        const requireNewline = !skipOneNewline(buf, iEnd);
        const iBeforeTitle = iEnd;

        if (parseTitle(buf, iEnd))
        {
            iEnd = skipChars(buf, iEnd, " \t");
            if (iEnd < buf.length && buf[iEnd] != '\r' && buf[iEnd] != '\n')
            {
                // the title must end with a newline
                title.length = 0;
                iEnd = iBeforeTitle;
            }
        }

        iEnd = skipChars(buf, iEnd, " \t");
        if (requireNewline && iEnd < buf.length-1 && buf[iEnd] != '\r' && buf[iEnd] != '\n')
            return i;

        return iEnd;
    }

    /****************************************************
     * Parse and normalize a Markdown reference label
     * Params:
     *  buf   = an OutBuffer containing the DDoc
     *  i     = the index within `buf` that points to the `[` character at the start of the label.
     *          If this function returns a non-empty label then `i` will point just after the ']' at the end of the label.
     * Returns: the parsed and normalized label, possibly empty
     */
    private bool parseLabel(ref OutBuffer buf, ref size_t i)
    {
        if (buf[i] != '[')
            return false;

        const slice = buf[];
        size_t j = i + 1;

        // Some labels have already been en-symboled; handle that
        const inSymbol = j+15 < slice.length && slice[j..j+15] == "$(DDOC_PSYMBOL ";
        if (inSymbol)
            j += 15;

        for (; j < slice.length; ++j)
        {
            const c = slice[j];
            switch (c)
            {
            case ' ':
            case '\t':
            case '\r':
            case '\n':
                if (label.length && label[$-1] != ' ')
                    label ~= ' ';
                break;
            case ')':
                if (inSymbol && j+1 < slice.length && slice[j+1] == ']')
                {
                    ++j;
                    goto case ']';
                }
                goto default;
            case '[':
                if (slice[j-1] != '\\')
                {
                    label.length = 0;
                    return false;
                }
                break;
            case ']':
                if (label.length && label[$-1] == ' ')
                    --label.length;
                if (label.length)
                {
                    i = j + 1;
                    return true;
                }
                return false;
            default:
                label ~= c;
                break;
            }
        }
        label.length = 0;
        return false;
    }

    /****************************************************
     * Parse and store a Markdown link URL, optionally enclosed in `<>` brackets
     * Params:
     *  buf   = an OutBuffer containing the DDoc
     *  i     = the index within `buf` that points to the first character of the URL.
     *          If this function succeeds `i` will point just after the end of the URL.
     * Returns: whether a URL was found and parsed
     */
    private bool parseHref(ref OutBuffer buf, ref size_t i)
    {
        size_t j = skipChars(buf, i, " \t");

        size_t iHrefStart = j;
        size_t parenDepth = 1;
        bool inPointy = false;
        const slice = buf[];
        for (; j < slice.length; j++)
        {
            switch (slice[j])
            {
            case '<':
                if (!inPointy && j == iHrefStart)
                {
                    inPointy = true;
                    ++iHrefStart;
                }
                break;
            case '>':
                if (inPointy && slice[j-1] != '\\')
                    goto LReturnHref;
                break;
            case '(':
                if (!inPointy && slice[j-1] != '\\')
                    ++parenDepth;
                break;
            case ')':
                if (!inPointy && slice[j-1] != '\\')
                {
                    --parenDepth;
                    if (!parenDepth)
                        goto LReturnHref;
                }
                break;
            case ' ':
            case '\t':
            case '\r':
            case '\n':
                if (inPointy)
                {
                    // invalid link
                    return false;
                }
                goto LReturnHref;
            default:
                break;
            }
        }
        if (inPointy)
            return false;
    LReturnHref:
        auto href = slice[iHrefStart .. j].dup;
        this.href = cast(string) percentEncode(removeEscapeBackslashes(href)).replaceChar(',', "$(COMMA)");
        i = j;
        if (inPointy)
            ++i;
        return true;
    }

    /****************************************************
     * Parse and store a Markdown link title, enclosed in parentheses or `'` or `"` quotes
     * Params:
     *  buf   = an OutBuffer containing the DDoc
     *  i     = the index within `buf` that points to the first character of the title.
     *          If this function succeeds `i` will point just after the end of the title.
     * Returns: whether a title was found and parsed
     */
    private bool parseTitle(ref OutBuffer buf, ref size_t i)
    {
        size_t j = skipChars(buf, i, " \t");
        if (j >= buf.length)
            return false;

        char type = buf[j];
        if (type != '"' && type != '\'' && type != '(')
            return false;
        if (type == '(')
            type = ')';

        const iTitleStart = j + 1;
        size_t iNewline = 0;
        const slice = buf[];
        for (j = iTitleStart; j < slice.length; j++)
        {
            const c = slice[j];
            switch (c)
            {
            case ')':
            case '"':
            case '\'':
                if (type == c && slice[j-1] != '\\')
                    goto LEndTitle;
                iNewline = 0;
                break;
            case ' ':
            case '\t':
            case '\r':
                break;
            case '\n':
                if (iNewline)
                {
                    // no blank lines in titles
                    return false;
                }
                iNewline = j;
                break;
            default:
                iNewline = 0;
                break;
            }
        }
        return false;
    LEndTitle:
        auto title = slice[iTitleStart .. j].dup;
        this.title = cast(string) removeEscapeBackslashes(title).
            replaceChar(',', "$(COMMA)").
            replaceChar('"', "$(QUOTE)");
        i = j + 1;
        return true;
    }

    /****************************************************
     * Replace a Markdown link or image with the appropriate macro
     * Params:
     *  buf       = an OutBuffer containing the DDoc
     *  i         = the index within `buf` that points to the `]` character of the inline link.
     *              When this function returns it will be adjusted to the end of the inserted macro.
     *  iLinkEnd  = the index within `buf` that points just after the last character of the link
     *  delimiter = the Markdown delimiter that started the link or image
     */
    private void replaceLink(ref OutBuffer buf, ref size_t i, size_t iLinkEnd, MarkdownDelimiter delimiter)
    {
        size_t iAfterLink = i - delimiter.count;
        string macroName;
        if (symbol)
        {
            macroName = "$(SYMBOL_LINK ";
        }
        else if (title.length)
        {
            if (delimiter.type == '[')
                macroName = "$(LINK_TITLE ";
            else
                macroName = "$(IMAGE_TITLE ";
        }
        else
        {
            if (delimiter.type == '[')
                macroName = "$(LINK2 ";
            else
                macroName = "$(IMAGE ";
        }
        buf.remove(delimiter.iStart, delimiter.count);
        buf.remove(i - delimiter.count, iLinkEnd - i);
        iLinkEnd = buf.insert(delimiter.iStart, macroName);
        iLinkEnd = buf.insert(iLinkEnd, href);
        iLinkEnd = buf.insert(iLinkEnd, ", ");
        iAfterLink += macroName.length + href.length + 2;
        if (title.length)
        {
            iLinkEnd = buf.insert(iLinkEnd, title);
            iLinkEnd = buf.insert(iLinkEnd, ", ");
            iAfterLink += title.length + 2;

            // Link macros with titles require escaping commas
            for (size_t j = iLinkEnd; j < iAfterLink; ++j)
                if (buf[j] == ',')
                {
                    buf.remove(j, 1);
                    j = buf.insert(j, "$(COMMA)") - 1;
                    iAfterLink += 7;
                }
        }
// TODO: if image, remove internal macros, leaving only text
        buf.insert(iAfterLink, ")");
        i = iAfterLink;
    }

    /****************************************************
     * Store the Markdown link definition and remove it from `buf`
     * Params:
     *  buf               = an OutBuffer containing the DDoc
     *  i                 = the index within `buf` that points to the `[` character at the start of the link definition.
     *                      When this function returns it will be adjusted to exclude the link definition.
     *  iEnd              = the index within `buf` that points just after the end of the definition
     *  linkReferences    = previously parsed link references. When this function returns it may contain
     *                      an additional reference.
     *  loc               = the current location in the file
     */
    private void storeAndReplaceDefinition(ref OutBuffer buf, ref size_t i, size_t iEnd, ref MarkdownLinkReferences linkReferences, const ref Loc loc)
    {
        // Remove the definition and trailing whitespace
        iEnd = skipChars(buf, iEnd, " \t\r\n");
        buf.remove(i, iEnd - i);
        i -= 2;

        string lowercaseLabel = label.toLowercase();
        if (lowercaseLabel !in linkReferences.references)
            linkReferences.references[lowercaseLabel] = this;
    }

    /****************************************************
     * Remove Markdown escaping backslashes from the given string
     * Params:
     *  s = the string to remove escaping backslashes from
     * Returns: `s` without escaping backslashes in it
     */
    private static char[] removeEscapeBackslashes(char[] s)
    {
        if (!s.length)
            return s;

        // avoid doing anything if there isn't anything to escape
        size_t i;
        for (i = 0; i < s.length-1; ++i)
            if (s[i] == '\\' && ispunct(s[i+1]))
                break;
        if (i == s.length-1)
            return s;

        // copy characters backwards, then truncate
        size_t j = i + 1;
        s[i] = s[j];
        for (++i, ++j; j < s.length; ++i, ++j)
        {
            if (j < s.length-1 && s[j] == '\\' && ispunct(s[j+1]))
                ++j;
            s[i] = s[j];
        }
        s.length -= (j - i);
        return s;
    }

    ///
    unittest
    {
        assert(removeEscapeBackslashes("".dup) == "");
        assert(removeEscapeBackslashes(`\a`.dup) == `\a`);
        assert(removeEscapeBackslashes(`.\`.dup) == `.\`);
        assert(removeEscapeBackslashes(`\.\`.dup) == `.\`);
        assert(removeEscapeBackslashes(`\.`.dup) == `.`);
        assert(removeEscapeBackslashes(`\.\.`.dup) == `..`);
        assert(removeEscapeBackslashes(`a\.b\.c`.dup) == `a.b.c`);
    }

    /****************************************************
     * Percent-encode (AKA URL-encode) the given string
     * Params:
     *  s = the string to percent-encode
     * Returns: `s` with special characters percent-encoded
     */
    private static inout(char)[] percentEncode(inout(char)[] s) pure
    {
        static bool shouldEncode(char c)
        {
            return ((c < '0' && c != '!' && c != '#' && c != '$' && c != '%' && c != '&' && c != '\'' && c != '(' &&
                    c != ')' && c != '*' && c != '+' && c != ',' && c != '-' && c != '.' && c != '/')
                || (c > '9' && c < 'A' && c != ':' && c != ';' && c != '=' && c != '?' && c != '@')
                || (c > 'Z' && c < 'a' && c != '[' && c != ']' && c != '_')
                || (c > 'z' && c != '~'));
        }

        for (size_t i = 0; i < s.length; ++i)
        {
            if (shouldEncode(s[i]))
            {
                immutable static hexDigits = "0123456789ABCDEF";
                immutable encoded1 = hexDigits[s[i] >> 4];
                immutable encoded2 = hexDigits[s[i] & 0x0F];
                s = s[0..i] ~ '%' ~ encoded1 ~ encoded2 ~ s[i+1..$];
                i += 2;
            }
        }
        return s;
    }

    ///
    unittest
    {
        assert(percentEncode("") == "");
        assert(percentEncode("aB12-._~/?") == "aB12-._~/?");
        assert(percentEncode("<\n>") == "%3C%0A%3E");
    }

    /**************************************************
     * Skip a single newline at `i`
     * Params:
     *  buf   = an OutBuffer containing the DDoc
     *  i     = the index within `buf` to start looking at.
     *          If this function succeeds `i` will point after the newline.
     * Returns: whether a newline was skipped
     */
    private static bool skipOneNewline(ref OutBuffer buf, ref size_t i) pure
    {
        if (i < buf.length && buf[i] == '\r')
            ++i;
        if (i < buf.length && buf[i] == '\n')
        {
            ++i;
            return true;
        }
        return false;
    }
}

/**************************************************
 * A set of Markdown link references.
 */
private struct MarkdownLinkReferences
{
    MarkdownLink[string] references;    // link references keyed by normalized label
    MarkdownLink[string] symbols;       // link symbols keyed by name
    Scope* _scope;      // the current scope
    bool extractedAll;  // the index into the buffer of the last-parsed reference

    /**************************************************
     * Look up a reference by label, searching through the rest of the buffer if needed.
     * Symbols in the current scope are searched for if the DDoc doesn't define the reference.
     * Params:
     *  label = the label to find the reference for
     *  buf   = an OutBuffer containing the DDoc
     *  i     = the index within `buf` to start searching for references at
     *  loc   = the current location in the file
     * Returns: a link. If the `href` member has a value then the reference is valid.
     */
    MarkdownLink lookupReference(string label, ref OutBuffer buf, size_t i, const ref Loc loc)
    {
        const lowercaseLabel = label.toLowercase();
        if (lowercaseLabel !in references)
            extractReferences(buf, i, loc);

        if (lowercaseLabel in references)
            return references[lowercaseLabel];

        return MarkdownLink();
    }

    /**
     * Look up the link for the D symbol with the given name.
     * If found, the link is cached in the `symbols` member.
     * Params:
     *  name  = the name of the symbol
     * Returns: the link for the symbol or a link with a `null` href
     */
    MarkdownLink lookupSymbol(string name)
    {
        if (name in symbols)
            return symbols[name];

        const ids = split(name, '.');

        MarkdownLink link;
        auto id = Identifier.lookup(ids[0].ptr, ids[0].length);
        if (id)
        {
            auto loc = Loc();
            auto symbol = _scope.search(loc, id, null, IgnoreErrors);
            for (size_t i = 1; symbol && i < ids.length; ++i)
            {
                id = Identifier.lookup(ids[i].ptr, ids[i].length);
                symbol = id !is null ? symbol.search(loc, id, IgnoreErrors) : null;
            }
            if (symbol)
                link = MarkdownLink(createHref(symbol), null, name, symbol);
        }

        symbols[name] = link;
        return link;
    }

    /**************************************************
     * Remove and store all link references from the document, in the form of
     * `[label]: href "optional title"`
     * Params:
     *  buf   = an OutBuffer containing the DDoc
     *  i     = the index within `buf` to start looking at
     *  loc   = the current location in the file
     * Returns: whether a reference was extracted
     */
    private void extractReferences(ref OutBuffer buf, size_t i, const ref Loc loc)
    {
        static bool isFollowedBySpace(ref OutBuffer buf, size_t i)
        {
            return i+1 < buf.length && (buf[i+1] == ' ' || buf[i+1] == '\t');
        }

        if (extractedAll)
            return;

        bool leadingBlank = false;
        int inCode = false;
        bool newParagraph = true;
        MarkdownDelimiter[] delimiters;
        for (; i < buf.length; ++i)
        {
            const c = buf[i];
            switch (c)
            {
            case ' ':
            case '\t':
                break;
            case '\n':
                if (leadingBlank && !inCode)
                    newParagraph = true;
                leadingBlank = true;
                break;
            case '\\':
                ++i;
                break;
            case '#':
                if (leadingBlank && !inCode)
                    newParagraph = true;
                leadingBlank = false;
                break;
            case '>':
                if (leadingBlank && !inCode)
                    newParagraph = true;
                break;
            case '+':
                if (leadingBlank && !inCode && isFollowedBySpace(buf, i))
                    newParagraph = true;
                else
                    leadingBlank = false;
                break;
            case '0':
            ..
            case '9':
                if (leadingBlank && !inCode)
                {
                    i = skipChars(buf, i, "0123456789");
                    if (i < buf.length &&
                        (buf[i] == '.' || buf[i] == ')') &&
                        isFollowedBySpace(buf, i))
                        newParagraph = true;
                    else
                        leadingBlank = false;
                }
                break;
            case '*':
                if (leadingBlank && !inCode)
                {
                    newParagraph = true;
                    if (!isFollowedBySpace(buf, i))
                        leadingBlank = false;
                }
                break;
            case '`':
            case '~':
                if (leadingBlank && i+2 < buf.length && buf[i+1] == c && buf[i+2] == c)
                {
                    inCode = inCode == c ? false : c;
                    i = skipChars(buf, i, [c]) - 1;
                    newParagraph = true;
                }
                leadingBlank = false;
                break;
            case '-':
                if (leadingBlank && !inCode && isFollowedBySpace(buf, i))
                    goto case '+';
                else
                    goto case '`';
            case '[':
                if (leadingBlank && !inCode && newParagraph)
                    delimiters ~= MarkdownDelimiter(i, 1, 0, false, false, true, c);
                break;
            case ']':
                if (delimiters.length && !inCode &&
                    MarkdownLink.replaceReferenceDefinition(buf, i, delimiters, cast(int) delimiters.length - 1, this, loc))
                    --i;
                break;
            default:
                if (leadingBlank)
                    newParagraph = false;
                leadingBlank = false;
                break;
            }
        }
        extractedAll = true;
    }

    /**
     * Split a string by a delimiter, excluding the delimiter.
     * Params:
     *  s         = the string to split
     *  delimiter = the character to split by
     * Returns: the resulting array of strings
     */
    private static string[] split(string s, char delimiter) pure
    {
        string[] result;
        size_t iStart = 0;
        foreach (size_t i; 0..s.length)
            if (s[i] == delimiter)
            {
                result ~= s[iStart..i];
                iStart = i + 1;
            }
        result ~= s[iStart..$];
        return result;
    }

    ///
    unittest
    {
        assert(split("", ',') == [""]);
        assert(split("ab", ',') == ["ab"]);
        assert(split("a,b", ',') == ["a", "b"]);
        assert(split("a,,b", ',') == ["a", "", "b"]);
        assert(split(",ab", ',') == ["", "ab"]);
        assert(split("ab,", ',') == ["ab", ""]);
    }

    /**
     * Create a HREF for the given D symbol.
     * The HREF is relative to the current location if possible.
     * Params:
     *  symbol    = the symbol to create a HREF for.
     * Returns: the resulting href
     */
    private string createHref(Dsymbol symbol)
    {
        Dsymbol root = symbol;

        const(char)[] lref;
        while (symbol && symbol.ident && !symbol.isModule())
        {
            if (lref.length)
                lref = '.' ~ lref;
            lref = symbol.ident.toString() ~ lref;
            symbol = symbol.parent;
        }

        const(char)[] path;
        if (symbol && symbol.ident && symbol.isModule() != _scope._module)
        {
            do
            {
                root = symbol;

                // If the module has a file name, we're done
                if (const m = symbol.isModule())
                    if (m.docfile)
                    {
                        path = m.docfile.toString();
                        break;
                    }

                if (path.length)
                    path = '_' ~ path;
                path = symbol.ident.toString() ~ path;
                symbol = symbol.parent;
            } while (symbol && symbol.ident);

            if (!symbol && path.length)
                path ~= "$(DOC_EXTENSION)";
        }

        // Attempt an absolute URL if not in the same package
        while (root.parent)
            root = root.parent;
        Dsymbol scopeRoot = _scope._module;
        while (scopeRoot.parent)
            scopeRoot = scopeRoot.parent;
        if (scopeRoot != root)
        {
            path = "$(DOC_ROOT_" ~ root.ident.toString() ~ ')' ~ path;
            lref = '.' ~ lref;  // remote URIs like Phobos and Mir use .prefixes
        }

        return cast(string) (path ~ '#' ~ lref);
    }
}

private enum TableColumnAlignment
{
    none,
    left,
    center,
    right
}

/****************************************************
 * Parse a Markdown table delimiter row in the form of `| -- | :-- | :--: | --: |`
 * where the example text has four columns with the following alignments:
 * default, left, center, and right. The first and last pipes are optional. If a
 * delimiter row is found it will be removed from `buf`.
 *
 * Params:
 *  buf     = an OutBuffer containing the DDoc
 *  iStart  = the index within `buf` that the delimiter row starts at
 *  inQuote   = whether the table is inside a quote
 *  columnAlignments = alignments to populate for each column
 * Returns: the index of the end of the parsed delimiter, or `0` if not found
 */
private size_t parseTableDelimiterRow(ref OutBuffer buf, const size_t iStart, bool inQuote, ref TableColumnAlignment[] columnAlignments)
{
    size_t i = skipChars(buf, iStart, inQuote ? ">| \t" : "| \t");
    while (i < buf.length && buf[i] != '\r' && buf[i] != '\n')
    {
        const leftColon = buf[i] == ':';
        if (leftColon)
            ++i;

        if (i >= buf.length || buf[i] != '-')
            break;
        i = skipChars(buf, i, "-");

        const rightColon = i < buf.length && buf[i] == ':';
        i = skipChars(buf, i, ": \t");

        if (i >= buf.length || (buf[i] != '|' && buf[i] != '\r' && buf[i] != '\n'))
            break;
        i = skipChars(buf, i, "| \t");

        columnAlignments ~= (leftColon && rightColon) ? TableColumnAlignment.center :
                leftColon ? TableColumnAlignment.left :
                rightColon ? TableColumnAlignment.right :
                TableColumnAlignment.none;
    }

    if (i < buf.length && buf[i] != '\r' && buf[i] != '\n' && buf[i] != ')')
    {
        columnAlignments.length = 0;
        return 0;
    }

    if (i < buf.length && buf[i] == '\r') ++i;
    if (i < buf.length && buf[i] == '\n') ++i;
    return i;
}

/****************************************************
 * Look for a table delimiter row, and if found parse the previous row as a
 * table header row. If both exist with a matching number of columns, start a
 * table.
 *
 * Params:
 *  buf       = an OutBuffer containing the DDoc
 *  iStart    = the index within `buf` that the table header row starts at, inclusive
 *  iEnd      = the index within `buf` that the table header row ends at, exclusive
 *  loc       = the current location in the file
 *  inQuote   = whether the table is inside a quote
 *  inlineDelimiters = delimiters containing columns separators and any inline emphasis
 *  columnAlignments = the parsed alignments for each column
 * Returns: the number of characters added by starting the table, or `0` if unchanged
 */
private size_t startTable(ref OutBuffer buf, size_t iStart, size_t iEnd, const ref Loc loc, bool inQuote, ref MarkdownDelimiter[] inlineDelimiters, out TableColumnAlignment[] columnAlignments)
{
    const iDelimiterRowEnd = parseTableDelimiterRow(buf, iEnd + 1, inQuote, columnAlignments);
    if (iDelimiterRowEnd)
    {
        size_t delta;
        if (replaceTableRow(buf, iStart, iEnd, loc, inlineDelimiters, columnAlignments, true, delta))
        {
            buf.remove(iEnd + delta, iDelimiterRowEnd - iEnd);
            buf.insert(iEnd + delta, "$(TBODY ");
            buf.insert(iStart, "$(TABLE ");
            return delta + 15;
        }
    }

    columnAlignments.length = 0;
    return 0;
}

/****************************************************
 * Replace a Markdown table row in the form of table cells delimited by pipes:
 * `| cell | cell | cell`. The first and last pipes are optional.
 *
 * Params:
 *  buf       = an OutBuffer containing the DDoc
 *  iStart    = the index within `buf` that the table row starts at, inclusive
 *  iEnd      = the index within `buf` that the table row ends at, exclusive
 *  loc       = the current location in the file
 *  inlineDelimiters = delimiters containing columns separators and any inline emphasis
 *  columnAlignments = alignments for each column
 *  headerRow = if `true` then the number of columns will be enforced to match
 *              `columnAlignments.length` and the row will be surrounded by a
 *              `THEAD` macro
 *  delta     = the number of characters added by replacing the row, or `0` if unchanged
 * Returns: `true` if a table row was found and replaced
 */
private bool replaceTableRow(ref OutBuffer buf, size_t iStart, size_t iEnd, const ref Loc loc, ref MarkdownDelimiter[] inlineDelimiters, TableColumnAlignment[] columnAlignments, bool headerRow, out size_t delta)
{
    delta = 0;

    if (!columnAlignments.length || iStart == iEnd)
        return false;

    iStart = skipChars(buf, iStart, " \t");
    int cellCount = 0;
    foreach (delimiter; inlineDelimiters)
        if (delimiter.type == '|' && !delimiter.leftFlanking)
            ++cellCount;
    bool ignoreLast = inlineDelimiters.length > 0 && inlineDelimiters[$-1].type == '|';
    if (ignoreLast)
    {
        const iLast = skipChars(buf, inlineDelimiters[$-1].iStart + inlineDelimiters[$-1].count, " \t");
        ignoreLast = iLast >= iEnd;
    }
    if (!ignoreLast)
        ++cellCount;

    if (headerRow && cellCount != columnAlignments.length)
        return false;

    void replaceTableCell(size_t iCellStart, size_t iCellEnd, int cellIndex, int di)
    {
        const eDelta = replaceMarkdownEmphasis(buf, loc, inlineDelimiters, di);
        delta += eDelta;
        iCellEnd += eDelta;

        // strip trailing whitespace and delimiter
        size_t i = iCellEnd - 1;
        while (i > iCellStart && (buf[i] == '|' || buf[i] == ' ' || buf[i] == '\t'))
            --i;
        ++i;
        buf.remove(i, iCellEnd - i);
        delta -= iCellEnd - i;
        iCellEnd = i;

        buf.insert(iCellEnd, ")");
        ++delta;

        // strip initial whitespace and delimiter
        i = skipChars(buf, iCellStart, "| \t");
        buf.remove(iCellStart, i - iCellStart);
        delta -= i - iCellStart;

        switch (columnAlignments[cellIndex])
        {
        case TableColumnAlignment.none:
            buf.insert(iCellStart, headerRow ? "$(TH " : "$(TD ");
            delta += 5;
            break;
        case TableColumnAlignment.left:
            buf.insert(iCellStart, "left, ");
            delta += 6;
            goto default;
        case TableColumnAlignment.center:
            buf.insert(iCellStart, "center, ");
            delta += 8;
            goto default;
        case TableColumnAlignment.right:
            buf.insert(iCellStart, "right, ");
            delta += 7;
            goto default;
        default:
            buf.insert(iCellStart, headerRow ? "$(TH_ALIGN " : "$(TD_ALIGN ");
            delta += 11;
            break;
        }
    }

    int cellIndex = cellCount - 1;
    size_t iCellEnd = iEnd;
    foreach_reverse (di, delimiter; inlineDelimiters)
    {
        if (delimiter.type == '|')
        {
            if (ignoreLast && di == inlineDelimiters.length-1)
            {
                ignoreLast = false;
                continue;
            }

            if (cellIndex >= columnAlignments.length)
            {
                // kill any extra cells
                buf.remove(delimiter.iStart, iEnd + delta - delimiter.iStart);
                delta -= iEnd + delta - delimiter.iStart;
                iCellEnd = iEnd + delta;
                --cellIndex;
                continue;
            }

            replaceTableCell(delimiter.iStart, iCellEnd, cellIndex, cast(int) di);
            iCellEnd = delimiter.iStart;
            --cellIndex;
        }
    }

    // if no starting pipe, replace from the start
    if (cellIndex >= 0)
        replaceTableCell(iStart, iCellEnd, cellIndex, 0);

    buf.insert(iEnd + delta, ")");
    buf.insert(iStart, "$(TR ");
    delta += 6;

    if (headerRow)
    {
        buf.insert(iEnd + delta, ")");
        buf.insert(iStart, "$(THEAD ");
        delta += 9;
    }

    return true;
}

/****************************************************
 * End a table, if in one.
 *
 * Params:
 *  buf = an OutBuffer containing the DDoc
 *  i   = the index within `buf` to end the table at
 *  columnAlignments = alignments for each column; upon return is set to length `0`
 * Returns: the number of characters added by ending the table, or `0` if unchanged
 */
private size_t endTable(ref OutBuffer buf, size_t i, ref TableColumnAlignment[] columnAlignments)
{
    if (!columnAlignments.length)
        return 0;

    buf.insert(i, "))");
    columnAlignments.length = 0;
    return 2;
}

/****************************************************
 * End a table row and then the table itself.
 *
 * Params:
 *  buf       = an OutBuffer containing the DDoc
 *  iStart    = the index within `buf` that the table row starts at, inclusive
 *  iEnd      = the index within `buf` that the table row ends at, exclusive
 *  loc       = the current location in the file
 *  inlineDelimiters = delimiters containing columns separators and any inline emphasis
 *  columnAlignments = alignments for each column; upon return is set to length `0`
 * Returns: the number of characters added by replacing the row, or `0` if unchanged
 */
private size_t endRowAndTable(ref OutBuffer buf, size_t iStart, size_t iEnd, const ref Loc loc, ref MarkdownDelimiter[] inlineDelimiters, ref TableColumnAlignment[] columnAlignments)
{
    size_t delta;
    replaceTableRow(buf, iStart, iEnd, loc, inlineDelimiters, columnAlignments, false, delta);
    delta += endTable(buf, iEnd + delta, columnAlignments);
    return delta;
}

/**************************************************
 * Highlight text section.
 *
 * Params:
 *  scope = the current parse scope
 *  a     = an array of D symbols at the current scope
 *  loc   = source location of start of text. It is a mutable copy to allow incrementing its linenum, for printing the correct line number when an error is encountered in a multiline block of ddoc.
 *  buf   = an OutBuffer containing the DDoc
 *  offset = the index within buf to start highlighting
 */
private void highlightText(Scope* sc, Dsymbols* a, Loc loc, ref OutBuffer buf, size_t offset)
{
    const incrementLoc = loc.linnum == 0 ? 1 : 0;
    loc.linnum += incrementLoc;
    loc.charnum = 0;
    //printf("highlightText()\n");
    bool leadingBlank = true;
    size_t iParagraphStart = offset;
    size_t iPrecedingBlankLine = 0;
    int headingLevel = 0;
    int headingMacroLevel = 0;
    int quoteLevel = 0;
    bool lineQuoted = false;
    int quoteMacroLevel = 0;
    MarkdownList[] nestedLists;
    MarkdownDelimiter[] inlineDelimiters;
    MarkdownLinkReferences linkReferences;
    TableColumnAlignment[] columnAlignments;
    bool tableRowDetected = false;
    int inCode = 0;
    int inBacktick = 0;
    int macroLevel = 0;
    int previousMacroLevel = 0;
    int parenLevel = 0;
    size_t iCodeStart = 0; // start of code section
    size_t codeFenceLength = 0;
    size_t codeIndent = 0;
    string codeLanguage;
    size_t iLineStart = offset;
    linkReferences._scope = sc;
    for (size_t i = offset; i < buf.length; i++)
    {
        char c = buf[i];
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
            if (headingLevel)
            {
                i += replaceMarkdownEmphasis(buf, loc, inlineDelimiters);
                endMarkdownHeading(buf, iParagraphStart, i, loc, headingLevel);
                removeBlankLineMacro(buf, iPrecedingBlankLine, i);
                ++i;
                iParagraphStart = skipChars(buf, i, " \t\r\n");
            }

            if (tableRowDetected && !columnAlignments.length)
                i += startTable(buf, iLineStart, i, loc, lineQuoted, inlineDelimiters, columnAlignments);
            else if (columnAlignments.length)
            {
                size_t delta;
                if (replaceTableRow(buf, iLineStart, i, loc, inlineDelimiters, columnAlignments, false, delta))
                    i += delta;
                else
                    i += endTable(buf, i, columnAlignments);
            }

            if (!inCode && nestedLists.length && !quoteLevel)
                MarkdownList.handleSiblingOrEndingList(buf, i, iParagraphStart, nestedLists);

            iPrecedingBlankLine = 0;
            if (!inCode && i == iLineStart && i + 1 < buf.length) // if "\n\n"
            {
                i += endTable(buf, i, columnAlignments);
                if (!lineQuoted && quoteLevel)
                    endAllListsAndQuotes(buf, i, nestedLists, quoteLevel, quoteMacroLevel);
                i += replaceMarkdownEmphasis(buf, loc, inlineDelimiters);

                // if we don't already know about this paragraph break then
                // insert a blank line and record the paragraph break
                if (iParagraphStart <= i)
                {
                    iPrecedingBlankLine = i;
                    i = buf.insert(i, "$(DDOC_BLANKLINE)");
                    iParagraphStart = i + 1;
                }
            }
            else if (inCode &&
                i == iLineStart &&
                i + 1 < buf.length &&
                !lineQuoted &&
                quoteLevel) // if "\n\n" in quoted code
            {
                inCode = false;
                i = buf.insert(i, ")");
                i += endAllMarkdownQuotes(buf, i, quoteLevel);
                quoteMacroLevel = 0;
            }
            leadingBlank = true;
            lineQuoted = false;
            tableRowDetected = false;
            iLineStart = i + 1;
            loc.linnum += incrementLoc;

            // update the paragraph start if we just entered a macro
            if (previousMacroLevel < macroLevel && iParagraphStart < iLineStart)
                iParagraphStart = iLineStart;
            previousMacroLevel = macroLevel;
            break;

        case '<':
            {
                leadingBlank = false;
                if (inCode)
                    break;
                const slice = buf[];
                auto p = &slice[i];
                const se = sc._module.escapetable.escapeChar('<');
                if (se == "&lt;")
                {
                    // Generating HTML
                    // Skip over comments
                    if (p[1] == '!' && p[2] == '-' && p[3] == '-')
                    {
                        size_t j = i + 4;
                        p += 4;
                        while (1)
                        {
                            if (j == slice.length)
                                goto L1;
                            if (p[0] == '-' && p[1] == '-' && p[2] == '>')
                            {
                                i = j + 2; // place on closing '>'
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
                            if (j == slice.length)
                                break;
                            if (p[0] == '>')
                            {
                                i = j; // place on closing '>'
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
                if (se.length)
                {
                    buf.remove(i, 1);
                    i = buf.insert(i, se);
                    i--; // point to ';'
                }
                break;
            }

        case '>':
            {
                if (leadingBlank && (!inCode || quoteLevel))
                {
                    lineQuoted = true;
                    int lineQuoteLevel = 1;
                    size_t iAfterDelimiters = i + 1;
                    for (; iAfterDelimiters < buf.length; ++iAfterDelimiters)
                    {
                        const c0 = buf[iAfterDelimiters];
                        if (c0 == '>')
                            ++lineQuoteLevel;
                        else if (c0 != ' ' && c0 != '\t')
                            break;
                    }
                    if (!quoteMacroLevel)
                        quoteMacroLevel = macroLevel;
                    buf.remove(i, iAfterDelimiters - i);

                    if (quoteLevel < lineQuoteLevel)
                    {
                        i += endRowAndTable(buf, iLineStart, i, loc, inlineDelimiters, columnAlignments);
                        if (nestedLists.length)
                        {
                            const indent = getMarkdownIndent(buf, iLineStart, i);
                            if (indent < nestedLists[$-1].contentIndent)
                                i += MarkdownList.endAllNestedLists(buf, i, nestedLists);
                        }

                        for (; quoteLevel < lineQuoteLevel; ++quoteLevel)
                        {
                            i = buf.insert(i, "$(BLOCKQUOTE\n");
                            iLineStart = iParagraphStart = i;
                        }
                        --i;
                    }
                    else
                    {
                        --i;
                        if (nestedLists.length)
                            MarkdownList.handleSiblingOrEndingList(buf, i, iParagraphStart, nestedLists);
                    }
                    break;
                }

                leadingBlank = false;
                if (inCode)
                    break;
                // Replace '>' with '&gt;' character entity
                const se = sc._module.escapetable.escapeChar('>');
                if (se.length)
                {
                    buf.remove(i, 1);
                    i = buf.insert(i, se);
                    i--; // point to ';'
                }
                break;
            }

        case '&':
            {
                leadingBlank = false;
                if (inCode)
                    break;
                char* p = cast(char*)&buf[].ptr[i];
                if (p[1] == '#' || isalpha(p[1]))
                    break;
                // already a character entity
                // Replace '&' with '&amp;' character entity
                const se = sc._module.escapetable.escapeChar('&');
                if (se)
                {
                    buf.remove(i, 1);
                    i = buf.insert(i, se);
                    i--; // point to ';'
                }
                break;
            }

        case '`':
            {
                const iAfterDelimiter = skipChars(buf, i, "`");
                const count = iAfterDelimiter - i;

                if (inBacktick == count)
                {
                    inBacktick = 0;
                    inCode = 0;
                    OutBuffer codebuf;
                    codebuf.write(buf[iCodeStart + count .. i]);
                    // escape the contents, but do not perform highlighting except for DDOC_PSYMBOL
                    highlightCode(sc, a, codebuf, 0);
                    escapeStrayParenthesis(loc, &codebuf, 0, false);
                    buf.remove(iCodeStart, i - iCodeStart + count); // also trimming off the current `
                    immutable pre = "$(DDOC_BACKQUOTED ";
                    i = buf.insert(iCodeStart, pre);
                    i = buf.insert(i, codebuf[]);
                    i = buf.insert(i, ")");
                    i--; // point to the ending ) so when the for loop does i++, it will see the next character
                    break;
                }

                // Perhaps we're starting or ending a Markdown code block
                if (leadingBlank && count >= 3)
                {
                    bool moreBackticks = false;
                    for (size_t j = iAfterDelimiter; !moreBackticks && j < buf.length; ++j)
                        if (buf[j] == '`')
                            moreBackticks = true;
                        else if (buf[j] == '\r' || buf[j] == '\n')
                            break;
                    if (!moreBackticks)
                        goto case '-';
                }

                if (inCode)
                {
                    if (inBacktick)
                        i = iAfterDelimiter - 1;
                    break;
                }
                inCode = c;
                inBacktick = cast(int) count;
                codeIndent = 0; // inline code is not indented
                // All we do here is set the code flags and record
                // the location. The macro will be inserted lazily
                // so we can easily cancel the inBacktick if we come
                // across a newline character.
                iCodeStart = i;
                i = iAfterDelimiter - 1;
                break;
            }

        case '#':
        {
            /* A line beginning with # indicates an ATX-style heading. */
            if (leadingBlank && !inCode)
            {
                leadingBlank = false;

                headingLevel = detectAtxHeadingLevel(buf, i);
                if (!headingLevel)
                    break;

                i += endRowAndTable(buf, iLineStart, i, loc, inlineDelimiters, columnAlignments);
                if (!lineQuoted && quoteLevel)
                    i += endAllListsAndQuotes(buf, iLineStart, nestedLists, quoteLevel, quoteMacroLevel);

                // remove the ### prefix, including whitespace
                i = skipChars(buf, i + headingLevel, " \t");
                buf.remove(iLineStart, i - iLineStart);
                i = iParagraphStart = iLineStart;

                removeAnyAtxHeadingSuffix(buf, i);
                --i;

                headingMacroLevel = macroLevel;
            }
            break;
        }

        case '~':
            {
                if (leadingBlank)
                {
                    // Perhaps we're starting or ending a Markdown code block
                    const iAfterDelimiter = skipChars(buf, i, "~");
                    if (iAfterDelimiter - i >= 3)
                        goto case '-';
                }
                leadingBlank = false;
                break;
            }

        case '-':
            /* A line beginning with --- delimits a code section.
             * inCode tells us if it is start or end of a code section.
             */
            if (leadingBlank)
            {
                if (!inCode && c == '-')
                {
                    const list = MarkdownList.parseItem(buf, iLineStart, i);
                    if (list.isValid)
                    {
                        if (replaceMarkdownThematicBreak(buf, i, iLineStart, loc))
                        {
                            removeBlankLineMacro(buf, iPrecedingBlankLine, i);
                            iParagraphStart = skipChars(buf, i+1, " \t\r\n");
                            break;
                        }
                        else
                            goto case '+';
                    }
                }

                size_t istart = i;
                size_t eollen = 0;
                leadingBlank = false;
                const c0 = c; // if we jumped here from case '`' or case '~'
                size_t iInfoString = 0;
                if (!inCode)
                    codeLanguage.length = 0;
                while (1)
                {
                    ++i;
                    if (i >= buf.length)
                        break;
                    c = buf[i];
                    if (c == '\n')
                    {
                        eollen = 1;
                        break;
                    }
                    if (c == '\r')
                    {
                        eollen = 1;
                        if (i + 1 >= buf.length)
                            break;
                        if (buf[i + 1] == '\n')
                        {
                            eollen = 2;
                            break;
                        }
                    }
                    // BUG: handle UTF PS and LS too
                    if (c != c0 || iInfoString)
                    {
                        if (!iInfoString && !inCode && i - istart >= 3)
                        {
                            // Start a Markdown info string, like ```ruby
                            codeFenceLength = i - istart;
                            i = iInfoString = skipChars(buf, i, " \t");
                        }
                        else if (iInfoString && c != '`')
                        {
                            if (!codeLanguage.length && (c == ' ' || c == '\t'))
                                codeLanguage = cast(string) buf[iInfoString..i].idup;
                        }
                        else
                        {
                            iInfoString = 0;
                            goto Lcont;
                        }
                    }
                }
                if (i - istart < 3 || (inCode && (inCode != c0 || (inCode != '-' && i - istart < codeFenceLength))))
                    goto Lcont;
                if (iInfoString)
                {
                    if (!codeLanguage.length)
                        codeLanguage = cast(string) buf[iInfoString..i].idup;
                }
                else
                    codeFenceLength = i - istart;

                // We have the start/end of a code section
                // Remove the entire --- line, including blanks and \n
                buf.remove(iLineStart, i - iLineStart + eollen);
                i = iLineStart;
                if (eollen)
                    leadingBlank = true;
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
                    codebuf.write(buf[iCodeStart .. i]);
                    codebuf.writeByte(0);
                    // Remove leading indentations from all lines
                    bool lineStart = true;
                    char* endp = cast(char*)codebuf[].ptr + codebuf.length;
                    for (char* p = cast(char*)codebuf[].ptr; p < endp;)
                    {
                        if (lineStart)
                        {
                            size_t j = codeIndent;
                            char* q = p;
                            while (j-- > 0 && q < endp && isIndentWS(q))
                                ++q;
                            codebuf.remove(p - cast(char*)codebuf[].ptr, q - p);
                            assert(cast(char*)codebuf[].ptr <= p);
                            assert(p < cast(char*)codebuf[].ptr + codebuf.length);
                            lineStart = false;
                            endp = cast(char*)codebuf[].ptr + codebuf.length; // update
                            continue;
                        }
                        if (*p == '\n')
                            lineStart = true;
                        ++p;
                    }
                    if (!codeLanguage.length || codeLanguage == "dlang" || codeLanguage == "d")
                        highlightCode2(sc, a, codebuf, 0);
                    else
                        codebuf.remove(codebuf.length-1, 1);    // remove the trailing 0 byte
                    escapeStrayParenthesis(loc, &codebuf, 0, false);
                    buf.remove(iCodeStart, i - iCodeStart);
                    i = buf.insert(iCodeStart, codebuf[]);
                    i = buf.insert(i, ")\n");
                    i -= 2; // in next loop, c should be '\n'
                }
                else
                {
                    i += endRowAndTable(buf, iLineStart, i, loc, inlineDelimiters, columnAlignments);
                    if (!lineQuoted && quoteLevel)
                    {
                        const delta = endAllListsAndQuotes(buf, iLineStart, nestedLists, quoteLevel, quoteMacroLevel);
                        i += delta;
                        istart += delta;
                    }

                    inCode = c0;
                    codeIndent = istart - iLineStart; // save indent count
                    if (codeLanguage.length && codeLanguage != "dlang" && codeLanguage != "d")
                    {
                        // backslash-escape
                        for (size_t j; j < codeLanguage.length - 1; ++j)
                            if (codeLanguage[j] == '\\' && ispunct(codeLanguage[j + 1]))
                                codeLanguage = codeLanguage[0..j] ~ codeLanguage[j + 1..$];

                        i = buf.insert(i, "$(OTHER_CODE ");
                        i = buf.insert(i, codeLanguage);
                        i = buf.insert(i, ",");
                    }
                    else
                        i = buf.insert(i, "$(D_CODE ");
                    iCodeStart = i;
                    i--; // place i on >
                    leadingBlank = true;
                }
            }
            break;

        case '_':
        {
            if (leadingBlank && !inCode && replaceMarkdownThematicBreak(buf, i, iLineStart, loc))
            {
                i += endRowAndTable(buf, iLineStart, i, loc, inlineDelimiters, columnAlignments);
                if (!lineQuoted && quoteLevel)
                    i += endAllListsAndQuotes(buf, iLineStart, nestedLists, quoteLevel, quoteMacroLevel);
                removeBlankLineMacro(buf, iPrecedingBlankLine, i);
                iParagraphStart = skipChars(buf, i+1, " \t\r\n");
                break;
            }
            goto default;
        }

        case '+':
        case '0':
        ..
        case '9':
        {
            if (leadingBlank && !inCode)
            {
                MarkdownList list = MarkdownList.parseItem(buf, iLineStart, i);
                if (list.isValid)
                {
                    // Avoid starting a numbered list in the middle of a paragraph
                    if (!nestedLists.length && list.orderedStart.length &&
                        iParagraphStart < iLineStart)
                    {
                        i += list.orderedStart.length - 1;
                        break;
                    }

                    i += endRowAndTable(buf, iLineStart, i, loc, inlineDelimiters, columnAlignments);
                    if (!lineQuoted && quoteLevel)
                    {
                        const delta = endAllListsAndQuotes(buf, iLineStart, nestedLists, quoteLevel, quoteMacroLevel);
                        i += delta;
                        list.iStart += delta;
                        list.iContentStart += delta;
                    }

                    list.macroLevel = macroLevel;
                    list.startItem(buf, iLineStart, i, iPrecedingBlankLine, nestedLists, loc);
                    break;
                }
            }
            leadingBlank = false;
            break;
        }

        case '*':
        {
            if (inCode || inBacktick)
            {
                leadingBlank = false;
                break;
            }

            if (leadingBlank)
            {
                // Check for a thematic break
                if (replaceMarkdownThematicBreak(buf, i, iLineStart, loc))
                {
                    i += endRowAndTable(buf, iLineStart, i, loc, inlineDelimiters, columnAlignments);
                    if (!lineQuoted && quoteLevel)
                        i += endAllListsAndQuotes(buf, iLineStart, nestedLists, quoteLevel, quoteMacroLevel);
                    removeBlankLineMacro(buf, iPrecedingBlankLine, i);
                    iParagraphStart = skipChars(buf, i+1, " \t\r\n");
                    break;
                }

                // An initial * indicates a Markdown list item
                const list = MarkdownList.parseItem(buf, iLineStart, i);
                if (list.isValid)
                    goto case '+';
            }

            // Markdown emphasis
            const leftC = i > offset ? buf[i-1] : '\0';
            size_t iAfterEmphasis = skipChars(buf, i+1, "*");
            const rightC = iAfterEmphasis < buf.length ? buf[iAfterEmphasis] : '\0';
            int count = cast(int) (iAfterEmphasis - i);
            const leftFlanking = (rightC != '\0' && !isspace(rightC)) && (!ispunct(rightC) || leftC == '\0' || isspace(leftC) || ispunct(leftC));
            const rightFlanking = (leftC != '\0' && !isspace(leftC)) && (!ispunct(leftC) || rightC == '\0' || isspace(rightC) || ispunct(rightC));
            auto emphasis = MarkdownDelimiter(i, count, macroLevel, leftFlanking, rightFlanking, false, c);

            if (!emphasis.leftFlanking && !emphasis.rightFlanking)
            {
                i = iAfterEmphasis - 1;
                break;
            }

            inlineDelimiters ~= emphasis;
            i += emphasis.count;
            --i;
            break;
        }

        case '!':
        {
            leadingBlank = false;

            if (inCode)
                break;

            if (i < buf.length-1 && buf[i+1] == '[')
            {
                const imageStart = MarkdownDelimiter(i, 2, macroLevel, false, false, false, c);
                inlineDelimiters ~= imageStart;
                ++i;
            }
            break;
        }
        case '[':
        {
            if (inCode)
            {
                leadingBlank = false;
                break;
            }

            const leftC = i > offset ? buf[i-1] : '\0';
            const rightFlanking = leftC != '\0' && !isspace(leftC) && !ispunct(leftC);
            const atParagraphStart = leadingBlank && iParagraphStart >= iLineStart;
            const linkStart = MarkdownDelimiter(i, 1, macroLevel, false, rightFlanking, atParagraphStart, c);
            inlineDelimiters ~= linkStart;
            leadingBlank = false;
            break;
        }
        case ']':
        {
            leadingBlank = false;

            if (inCode)
                break;

            for (int d = cast(int) inlineDelimiters.length - 1; d >= 0; --d)
            {
                const delimiter = inlineDelimiters[d];
                if (delimiter.type == '[' || delimiter.type == '!')
                {
                    if (delimiter.isValid &&
                        MarkdownLink.replaceLink(buf, i, loc, inlineDelimiters, d, linkReferences))
                    {
                        // if we removed a reference link then we're at line start
                        if (i <= delimiter.iStart)
                            leadingBlank = true;

                        // don't nest links
                        if (delimiter.type == '[')
                            for (--d; d >= 0; --d)
                                if (inlineDelimiters[d].type == '[')
                                    inlineDelimiters[d].invalidate();
                    }
                    else
                    {
                        // nothing found, so kill the delimiter
                        inlineDelimiters = inlineDelimiters[0..d] ~ inlineDelimiters[d+1..$];
                    }
                    break;
                }
            }
            break;
        }

        case '|':
        {
            if (inCode)
            {
                leadingBlank = false;
                break;
            }

            tableRowDetected = true;
            inlineDelimiters ~= MarkdownDelimiter(i, 1, macroLevel, leadingBlank, false, false, c);
            leadingBlank = false;
            break;
        }

        case '\\':
        {
            leadingBlank = false;
            if (inCode || i+1 >= buf.length)
                break;

            /* Escape Markdown special characters */
            char c1 = buf[i+1];
            if (ispunct(c1))
            {
                buf.remove(i, 1);

                auto se = sc._module.escapetable.escapeChar(c1);
                if (!se)
                    se = c1 == '$' ? "$(DOLLAR)" : c1 == ',' ? "$(COMMA)" : null;
                if (se)
                {
                    buf.remove(i, 1);
                    i = buf.insert(i, se);
                    i--; // point to escaped char
                }
            }
            break;
        }

        case '$':
        {
            /* Look for the start of a macro, '$(Identifier'
             */
            leadingBlank = false;
            if (inCode || inBacktick)
                break;
            const slice = buf[];
            auto p = &slice[i];
            if (p[1] == '(' && isIdStart(&p[2]))
                ++macroLevel;
            break;
        }

        case '(':
        {
            if (!inCode && i > offset && buf[i-1] != '$')
                ++parenLevel;
            break;
        }

        case ')':
        {   /* End of macro
             */
            leadingBlank = false;
            if (inCode || inBacktick)
                break;
            if (parenLevel > 0)
                --parenLevel;
            else if (macroLevel)
            {
                int downToLevel = cast(int) inlineDelimiters.length;
                while (downToLevel > 0 && inlineDelimiters[downToLevel - 1].macroLevel >= macroLevel)
                    --downToLevel;
                if (headingLevel && headingMacroLevel >= macroLevel)
                {
                    endMarkdownHeading(buf, iParagraphStart, i, loc, headingLevel);
                    removeBlankLineMacro(buf, iPrecedingBlankLine, i);
                }
                i += endRowAndTable(buf, iLineStart, i, loc, inlineDelimiters, columnAlignments);
                while (nestedLists.length && nestedLists[$-1].macroLevel >= macroLevel)
                {
                    i = buf.insert(i, ")\n)");
                    --nestedLists.length;
                }
                if (quoteLevel && quoteMacroLevel >= macroLevel)
                    i += endAllMarkdownQuotes(buf, i, quoteLevel);
                i += replaceMarkdownEmphasis(buf, loc, inlineDelimiters, downToLevel);

                --macroLevel;
                quoteMacroLevel = 0;
            }
            break;
        }

        default:
            leadingBlank = false;
            if (sc._module.filetype == FileType.ddoc || inCode)
                break;
            const start = cast(char*)buf[].ptr + i;
            if (isIdStart(start))
            {
                size_t j = skippastident(buf, i);
                if (i < j)
                {
                    size_t k = skippastURL(buf, i);
                    if (i < k)
                    {
                        /* The URL is buf[i..k]
                         */
                        if (macroLevel)
                            /* Leave alone if already in a macro
                             */
                            i = k - 1;
                        else
                        {
                            /* Replace URL with '$(DDOC_LINK_AUTODETECT URL)'
                             */
                            i = buf.bracket(i, "$(DDOC_LINK_AUTODETECT ", k, ")") - 1;
                        }
                        break;
                    }
                }
                else
                    break;
                size_t len = j - i;
                // leading '_' means no highlight unless it's a reserved symbol name
                if (c == '_' && (i == 0 || !isdigit(*(start - 1))) && (i == buf.length - 1 || !isReservedName(start[0 .. len])))
                {
                    buf.remove(i, 1);
                    i = buf.bracket(i, "$(DDOC_AUTO_PSYMBOL_SUPPRESS ", j - 1, ")") - 1;
                    break;
                }
                if (isIdentifier(a, start[0 .. len]))
                {
                    i = buf.bracket(i, "$(DDOC_AUTO_PSYMBOL ", j, ")") - 1;
                    break;
                }
                if (isKeyword(start[0 .. len]))
                {
                    i = buf.bracket(i, "$(DDOC_AUTO_KEYWORD ", j, ")") - 1;
                    break;
                }
                if (isFunctionParameter(a, start[0 .. len]))
                {
                    //printf("highlighting arg '%s', i = %d, j = %d\n", arg.ident.toChars(), i, j);
                    i = buf.bracket(i, "$(DDOC_AUTO_PARAM ", j, ")") - 1;
                    break;
                }
                i = j - 1;
            }
            break;
        }
    }

    if (inCode == '-')
        error(loc, "unmatched `---` in DDoc comment");
    else if (inCode)
        buf.insert(buf.length, ")");

    size_t i = buf.length;
    if (headingLevel)
    {
        endMarkdownHeading(buf, iParagraphStart, i, loc, headingLevel);
        removeBlankLineMacro(buf, iPrecedingBlankLine, i);
    }
    i += endRowAndTable(buf, iLineStart, i, loc, inlineDelimiters, columnAlignments);
    i += replaceMarkdownEmphasis(buf, loc, inlineDelimiters);
    endAllListsAndQuotes(buf, i, nestedLists, quoteLevel, quoteMacroLevel);
}

/**************************************************
 * Highlight code for DDOC section.
 */
private void highlightCode(Scope* sc, Dsymbol s, ref OutBuffer buf, size_t offset)
{
    auto imp = s.isImport();
    if (imp && imp.aliases.length > 0)
    {
        // For example: `public import core.stdc.string : memcpy, memcmp;`
        for(int i = 0; i < imp.aliases.length; i++)
        {
            // Need to distinguish between
            // `public import core.stdc.string : memcpy, memcmp;` and
            // `public import core.stdc.string : copy = memcpy, compare = memcmp;`
            auto a = imp.aliases[i];
            auto id = a ? a : imp.names[i];
            auto loc = Loc.init;
            if (auto symFromId = sc.search(loc, id, null))
            {
                highlightCode(sc, symFromId, buf, offset);
            }
        }
    }
    else
    {
        OutBuffer ancbuf;
        emitAnchor(ancbuf, s, sc);
        buf.insert(offset, ancbuf[]);
        offset += ancbuf.length;

        Dsymbols a;
        a.push(s);
        highlightCode(sc, &a, buf, offset);
    }
}

/****************************************************
 */
private void highlightCode(Scope* sc, Dsymbols* a, ref OutBuffer buf, size_t offset)
{
    //printf("highlightCode(a = '%s')\n", a.toChars());
    bool resolvedTemplateParameters = false;

    for (size_t i = offset; i < buf.length; i++)
    {
        char c = buf[i];
        const se = sc._module.escapetable.escapeChar(c);
        if (se.length)
        {
            buf.remove(i, 1);
            i = buf.insert(i, se);
            i--; // point to ';'
            continue;
        }
        char* start = cast(char*)buf[].ptr + i;
        if (isIdStart(start))
        {
            size_t j = skipPastIdentWithDots(buf, i);
            if (i < j)
            {
                size_t len = j - i;
                if (isIdentifier(a, start[0 .. len]))
                {
                    i = buf.bracket(i, "$(DDOC_PSYMBOL ", j, ")") - 1;
                    continue;
                }
            }

            j = skippastident(buf, i);
            if (i < j)
            {
                size_t len = j - i;
                if (isIdentifier(a, start[0 .. len]))
                {
                    i = buf.bracket(i, "$(DDOC_PSYMBOL ", j, ")") - 1;
                    continue;
                }
                if (isFunctionParameter(a, start[0 .. len]))
                {
                    //printf("highlighting arg '%s', i = %d, j = %d\n", arg.ident.toChars(), i, j);
                    i = buf.bracket(i, "$(DDOC_PARAM ", j, ")") - 1;
                    continue;
                }
                i = j - 1;
            }
        }
        else if (!resolvedTemplateParameters)
        {
            size_t previ = i;

            // hunt for template declarations:
            foreach (symi; 0 .. a.length)
            {
                FuncDeclaration fd = (*a)[symi].isFuncDeclaration();

                if (!fd || !fd.parent || !fd.parent.isTemplateDeclaration())
                {
                    continue;
                }

                TemplateDeclaration td = fd.parent.isTemplateDeclaration();

                // build the template parameters
                Array!(size_t) paramLens;
                paramLens.reserve(td.parameters.length);

                OutBuffer parametersBuf;
                HdrGenState hgs;

                parametersBuf.writeByte('(');

                foreach (parami; 0 .. td.parameters.length)
                {
                    TemplateParameter tp = (*td.parameters)[parami];

                    if (parami)
                        parametersBuf.writestring(", ");

                    size_t lastOffset = parametersBuf.length;

                    .toCBuffer(tp, &parametersBuf, &hgs);

                    paramLens[parami] = parametersBuf.length - lastOffset;
                }
                parametersBuf.writeByte(')');

                const templateParams = parametersBuf[];

                //printf("templateDecl: %s\ntemplateParams: %s\nstart: %s\n", td.toChars(), templateParams, start);
                if (start[0 .. templateParams.length] == templateParams)
                {
                    immutable templateParamListMacro = "$(DDOC_TEMPLATE_PARAM_LIST ";
                    buf.bracket(i, templateParamListMacro.ptr, i + templateParams.length, ")");

                    // We have the parameter list. While we're here we might
                    // as well wrap the parameters themselves as well

                    // + 1 here to take into account the opening paren of the
                    // template param list
                    i += templateParamListMacro.length + 1;

                    foreach (const len; paramLens)
                    {
                        i = buf.bracket(i, "$(DDOC_TEMPLATE_PARAM ", i + len, ")");
                        // increment two here for space + comma
                        i += 2;
                    }

                    resolvedTemplateParameters = true;
                    // reset i to be positioned back before we found the template
                    // param list this assures that anything within the template
                    // param list that needs to be escaped or otherwise altered
                    // has an opportunity for that to happen outside of this context
                    i = previ;

                    continue;
                }
            }
        }
    }
}

/****************************************
 */
private void highlightCode3(Scope* sc, ref OutBuffer buf, const(char)* p, const(char)* pend)
{
    for (; p < pend; p++)
    {
        const se = sc._module.escapetable.escapeChar(*p);
        if (se.length)
            buf.writestring(se);
        else
            buf.writeByte(*p);
    }
}

/**************************************************
 * Highlight code for CODE section.
 */
private void highlightCode2(Scope* sc, Dsymbols* a, ref OutBuffer buf, size_t offset)
{
    uint errorsave = global.startGagging();

    scope Lexer lex = new Lexer(null, cast(char*)buf[].ptr, 0, buf.length - 1, 0, 1,
        global.errorSink,
        global.vendor, global.versionNumber());
    OutBuffer res;
    const(char)* lastp = cast(char*)buf[].ptr;
    //printf("highlightCode2('%.*s')\n", cast(int)(buf.length - 1), buf[].ptr);
    res.reserve(buf.length);
    while (1)
    {
        Token tok;
        lex.scan(&tok);
        highlightCode3(sc, res, lastp, tok.ptr);
        string highlight = null;
        switch (tok.value)
        {
        case TOK.identifier:
            {
                if (!sc)
                    break;
                size_t len = lex.p - tok.ptr;
                if (isIdentifier(a, tok.ptr[0 .. len]))
                {
                    highlight = "$(D_PSYMBOL ";
                    break;
                }
                if (isFunctionParameter(a, tok.ptr[0 .. len]))
                {
                    //printf("highlighting arg '%s', i = %d, j = %d\n", arg.ident.toChars(), i, j);
                    highlight = "$(D_PARAM ";
                    break;
                }
                break;
            }
        case TOK.comment:
            highlight = "$(D_COMMENT ";
            break;
        case TOK.string_:
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
            size_t o = res.length;
            highlightCode3(sc, res, tok.ptr, lex.p);
            if (tok.value == TOK.comment || tok.value == TOK.string_)
                /* https://issues.dlang.org/show_bug.cgi?id=7656
                 * https://issues.dlang.org/show_bug.cgi?id=7715
                 * https://issues.dlang.org/show_bug.cgi?id=10519
                 */
                escapeDdocString(&res, o);
            res.writeByte(')');
        }
        else
            highlightCode3(sc, res, tok.ptr, lex.p);
        if (tok.value == TOK.endOfFile)
            break;
        lastp = lex.p;
    }
    buf.setsize(offset);
    buf.write(&res);
    global.endGagging(errorsave);
}

/****************************************
 * Determine if p points to the start of a "..." parameter identifier.
 */
private bool isCVariadicArg(const(char)[] p) @nogc nothrow pure @safe
{
    return p.length >= 3 && p[0 .. 3] == "...";
}

/****************************************
 * Determine if p points to the start of an identifier.
 */
bool isIdStart(const(char)* p) @nogc nothrow pure
{
    dchar c = *p;
    if (isalpha(c) || c == '_')
        return true;
    if (c >= 0x80)
    {
        size_t i = 0;
        if (utf_decodeChar(p[0 .. 4], i, c))
            return false; // ignore errors
        if (isUniAlpha(c))
            return true;
    }
    return false;
}

/****************************************
 * Determine if p points to the rest of an identifier.
 */
bool isIdTail(const(char)* p) @nogc nothrow pure
{
    dchar c = *p;
    if (isalnum(c) || c == '_')
        return true;
    if (c >= 0x80)
    {
        size_t i = 0;
        if (utf_decodeChar(p[0 .. 4], i, c))
            return false; // ignore errors
        if (isUniAlpha(c))
            return true;
    }
    return false;
}

/****************************************
 * Determine if p points to the indentation space.
 */
private bool isIndentWS(const(char)* p) @nogc nothrow pure @safe
{
    return (*p == ' ') || (*p == '\t');
}

/*****************************************
 * Return number of bytes in UTF character.
 */
int utfStride(const(char)* p) @nogc nothrow pure
{
    dchar c = *p;
    if (c < 0x80)
        return 1;
    size_t i = 0;
    utf_decodeChar(p[0 .. 4], i, c); // ignore errors, but still consume input
    return cast(int)i;
}

private inout(char)* stripLeadingNewlines(inout(char)* s) @nogc nothrow pure
{
    while (s && *s == '\n' || *s == '\r')
        s++;

    return s;
}
