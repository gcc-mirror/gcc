
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/module.c
 */

#include "root/dsystem.h"
#include "root/rmem.h"

#include "mars.h"
#include "module.h"
#include "parse.h"
#include "scope.h"
#include "identifier.h"
#include "id.h"
#include "import.h"
#include "dsymbol.h"
#include "expression.h"
#include "lexer.h"
#include "attrib.h"

AggregateDeclaration *Module::moduleinfo;

Module *Module::rootModule;
DsymbolTable *Module::modules;
Modules Module::amodules;

Dsymbols Module::deferred;  // deferred Dsymbol's needing semantic() run on them
Dsymbols Module::deferred2; // deferred Dsymbol's needing semantic2() run on them
Dsymbols Module::deferred3; // deferred Dsymbol's needing semantic3() run on them
unsigned Module::dprogress;

void Module::_init()
{
    modules = new DsymbolTable();
}

Module::Module(const char *filename, Identifier *ident, int doDocComment, int doHdrGen)
        : Package(ident)
{
    const char *srcfilename;

//    printf("Module::Module(filename = '%s', ident = '%s')\n", filename, ident->toChars());
    this->arg = filename;
    md = NULL;
    errors = 0;
    numlines = 0;
    members = NULL;
    isDocFile = 0;
    isPackageFile = false;
    pkg = NULL;
    needmoduleinfo = 0;
    selfimports = 0;
    rootimports = 0;
    insearch = 0;
    searchCacheIdent = NULL;
    searchCacheSymbol = NULL;
    searchCacheFlags = 0;
    decldefs = NULL;
    sictor = NULL;
    sctor = NULL;
    sdtor = NULL;
    ssharedctor = NULL;
    sshareddtor = NULL;
    stest = NULL;
    sfilename = NULL;
    importedFrom = NULL;
    srcfile = NULL;
    docfile = NULL;

    debuglevel = 0;
    debugids = NULL;
    debugidsNot = NULL;
    versionlevel = 0;
    versionids = NULL;
    versionidsNot = NULL;

    macrotable = NULL;
    escapetable = NULL;
    doppelganger = 0;
    cov = NULL;
    covb = NULL;

    nameoffset = 0;
    namelen = 0;

    srcfilename = FileName::defaultExt(filename, global.mars_ext.ptr);

    if (global.run_noext && global.params.run &&
        !FileName::ext(filename) &&
        FileName::exists(srcfilename) == 0 &&
        FileName::exists(filename) == 1)
    {
        FileName::free(srcfilename);
        srcfilename = FileName::removeExt(filename);    // just does a mem.strdup(filename)
    }
    else if (!FileName::equalsExt(srcfilename, global.mars_ext.ptr) &&
        !FileName::equalsExt(srcfilename, global.hdr_ext.ptr) &&
        !FileName::equalsExt(srcfilename, "dd"))
    {
        error("source file name '%s' must have .%s extension", srcfilename, global.mars_ext);
        fatal();
    }
    srcfile = new File(srcfilename);
    objfile = setOutfile(global.params.objname.ptr, global.params.objdir.ptr, filename, global.obj_ext.ptr);

    if (doDocComment)
        setDocfile();

    if (doHdrGen)
        hdrfile = setOutfile(global.params.hdrname.ptr, global.params.hdrdir.ptr, arg, global.hdr_ext.ptr);

    //objfile = new File(objfilename);
}

Module *Module::create(const char *filename, Identifier *ident, int doDocComment, int doHdrGen)
{
    return new Module(filename, ident, doDocComment, doHdrGen);
}

void Module::setDocfile()
{
    docfile = setOutfile(global.params.docname.ptr, global.params.docdir.ptr, arg, global.doc_ext.ptr);
}

/*********************************************
 * Combines things into output file name for .html and .di files.
 * Input:
 *      name    Command line name given for the file, NULL if none
 *      dir     Command line directory given for the file, NULL if none
 *      arg     Name of the source file
 *      ext     File name extension to use if 'name' is NULL
 *      global.params.preservePaths     get output path from arg
 *      srcfile Input file - output file name must not match input file
 */

File *Module::setOutfile(const char *name, const char *dir, const char *arg, const char *ext)
{
    const char *docfilename;

    if (name)
    {
        docfilename = name;
    }
    else
    {
        const char *argdoc;
        if (global.params.preservePaths)
            argdoc = arg;
        else
            argdoc = FileName::name(arg);

        // If argdoc doesn't have an absolute path, make it relative to dir
        if (!FileName::absolute(argdoc))
        {   //FileName::ensurePathExists(dir);
            argdoc = FileName::combine(dir, argdoc);
        }
        docfilename = FileName::forceExt(argdoc, ext);
    }

    if (FileName::equals(docfilename, srcfile->name->str))
    {
        error("source file and output file have same name '%s'", srcfile->name->str);
        fatal();
    }

    return new File(docfilename);
}

void Module::deleteObjFile()
{
    if (global.params.obj)
        objfile->remove();
    if (docfile)
        docfile->remove();
}

const char *Module::kind() const
{
    return "module";
}

static void checkModFileAlias(OutBuffer *buf, OutBuffer *dotmods,
                              Array<const char *> *ms, size_t msdim, const char *p)
{
    /* Check and replace the contents of buf[] with
     * an alias string from global.params.modFileAliasStrings[]
     */
    dotmods->writestring(p);
    for (size_t j = msdim; j--;)
    {
        const char *m = (*ms)[j];
        const char *q = strchr(m, '=');
        assert(q);
        if (dotmods->length() == (size_t)(q - m) && memcmp(dotmods->peekChars(), m, q - m) == 0)
        {
            buf->reset();
            size_t qlen = strlen(q + 1);
            if (qlen && (q[qlen] == '/' || q[qlen] == '\\'))
                --qlen;             // remove trailing separator
            buf->write(q + 1, qlen);
            break;                  // last matching entry in ms[] wins
        }
    }
    dotmods->writeByte('.');
}

/**
 * Converts a chain of identifiers to the filename of the module
 *
 * Params:
 *  packages = the names of the "parent" packages
 *  ident = the name of the child package or module
 *
 * Returns:
 *  the filename of the child package or module
 */
static const char *getFilename(Identifiers *packages, Identifier *ident)
{
    const char *filename = ident->toChars();

    if (packages == NULL || packages->length == 0)
        return filename;

    OutBuffer buf;
    OutBuffer dotmods;
    Array<const char *> *ms = &global.params.modFileAliasStrings;
    const size_t msdim = ms ? ms->length : 0;

    for (size_t i = 0; i < packages->length; i++)
    {
        Identifier *pid = (*packages)[i];
        const char *p = pid->toChars();
        buf.writestring(p);
        if (msdim)
            checkModFileAlias(&buf, &dotmods, ms, msdim, p);
#if _WIN32
        buf.writeByte('\\');
#else
        buf.writeByte('/');
#endif
    }
    buf.writestring(filename);
    if (msdim)
        checkModFileAlias(&buf, &dotmods, ms, msdim, filename);
    buf.writeByte(0);
    filename = (char *)buf.extractData();

    return filename;
}

/********************************************
 * Look for the source file if it's different from filename.
 * Look for .di, .d, directory, and along global.path.
 * Does not open the file.
 * Input:
 *      filename        as supplied by the user
 *      global.path
 * Returns:
 *      NULL if it's not different from filename.
 */

static const char *lookForSourceFile(const char *filename)
{
    /* Search along global.path for .di file, then .d file.
     */
    const char *sdi = FileName::forceExt(filename, global.hdr_ext.ptr);
    if (FileName::exists(sdi) == 1)
        return sdi;

    const char *sd  = FileName::forceExt(filename, global.mars_ext.ptr);
    if (FileName::exists(sd) == 1)
        return sd;

    if (FileName::exists(filename) == 2)
    {
        /* The filename exists and it's a directory.
         * Therefore, the result should be: filename/package.d
         * iff filename/package.d is a file
         */
        const char *ni = FileName::combine(filename, "package.di");
        if (FileName::exists(ni) == 1)
            return ni;
        FileName::free(ni);
        const char *n = FileName::combine(filename, "package.d");
        if (FileName::exists(n) == 1)
            return n;
        FileName::free(n);
    }

    if (FileName::absolute(filename))
        return NULL;

    if (!global.path)
        return NULL;

    for (size_t i = 0; i < global.path->length; i++)
    {
        const char *p = (*global.path)[i];
        const char *n = FileName::combine(p, sdi);
        if (FileName::exists(n) == 1)
        {
            return n;
        }
        FileName::free(n);

        n = FileName::combine(p, sd);
        if (FileName::exists(n) == 1)
        {
            return n;
        }
        FileName::free(n);

        const char *b = FileName::removeExt(filename);
        n = FileName::combine(p, b);
        FileName::free(b);
        if (FileName::exists(n) == 2)
        {
            const char *n2i = FileName::combine(n, "package.di");
            if (FileName::exists(n2i) == 1)
                return n2i;
            FileName::free(n2i);
            const char *n2 = FileName::combine(n, "package.d");
            if (FileName::exists(n2) == 1)
            {
                return n2;
            }
            FileName::free(n2);
        }
        FileName::free(n);
    }
    return NULL;
}

Module *Module::load(Loc loc, Identifiers *packages, Identifier *ident)
{
    //printf("Module::load(ident = '%s')\n", ident->toChars());

    // Build module filename by turning:
    //  foo.bar.baz
    // into:
    //  foo\bar\baz
    const char *filename = getFilename(packages, ident);
    // Look for the source file
    const char *result = lookForSourceFile(filename);
    if (result)
        filename = result;

    Module *m = new Module(filename, ident, 0, 0);
    m->loc = loc;

    if (!m->read(loc))
        return NULL;

    if (global.params.verbose)
    {
        OutBuffer buf;
        if (packages)
        {
            for (size_t i = 0; i < packages->length; i++)
            {
                Identifier *pid = (*packages)[i];
                buf.writestring(pid->toChars());
                buf.writeByte('.');
            }
        }
        buf.printf("%s\t(%s)", ident->toChars(), m->srcfile->toChars());
        message("import    %s", buf.peekChars());
    }

    m = m->parse();

    // Call onImport here because if the module is going to be compiled then we
    // need to determine it early because it affects semantic analysis. This is
    // being done after parsing the module so the full module name can be taken
    // from whatever was declared in the file.
    if (!m->isRoot() && Compiler::onImport(m))
    {
        m->importedFrom = m;
        assert(m->isRoot());
    }
    return m;
}

bool Module::read(Loc loc)
{
    //printf("Module::read('%s') file '%s'\n", toChars(), srcfile->toChars());
    if (srcfile->read())
    {
        if (!strcmp(srcfile->toChars(), "object.d"))
        {
            ::error(loc, "cannot find source code for runtime library file 'object.d'");
            errorSupplemental(loc, "dmd might not be correctly installed. Run 'dmd -man' for installation instructions.");
            const char *dmdConfFile = global.inifilename.length ? FileName::canonicalName(global.inifilename.ptr) : NULL;
            errorSupplemental(loc, "config file: %s", dmdConfFile ? dmdConfFile : "not found");
        }
        else
        {
            // if module is not named 'package' but we're trying to read 'package.d', we're looking for a package module
            bool isPackageMod = (strcmp(toChars(), "package") != 0) &&
                                (strcmp(srcfile->name->name(), "package.d") == 0 || (strcmp(srcfile->name->name(), "package.di") == 0));

            if (isPackageMod)
                ::error(loc, "importing package '%s' requires a 'package.d' file which cannot be found in '%s'",
                    toChars(), srcfile->toChars());
            else
                error(loc, "is in file '%s' which cannot be read", srcfile->toChars());
        }

        if (!global.gag)
        {
            /* Print path
             */
            if (global.path)
            {
                for (size_t i = 0; i < global.path->length; i++)
                {
                    const char *p = (*global.path)[i];
                    fprintf(stderr, "import path[%llu] = %s\n", (ulonglong)i, p);
                }
            }
            else
                fprintf(stderr, "Specify path to file '%s' with -I switch\n", srcfile->toChars());
            fatal();
        }
        return false;
    }
    return true;
}

Module *Module::parse()
{
    //printf("Module::parse(srcfile='%s') this=%p\n", srcfile->name->toChars(), this);

    const char *srcname = srcfile->name->toChars();
    //printf("Module::parse(srcname = '%s')\n", srcname);

    isPackageFile = (strcmp(srcfile->name->name(), "package.d") == 0 ||
                     strcmp(srcfile->name->name(), "package.di") == 0);

    utf8_t *buf = (utf8_t *)srcfile->buffer;
    size_t buflen = srcfile->len;

    if (buflen >= 2)
    {
        /* Convert all non-UTF-8 formats to UTF-8.
         * BOM : http://www.unicode.org/faq/utf_bom.html
         * 00 00 FE FF  UTF-32BE, big-endian
         * FF FE 00 00  UTF-32LE, little-endian
         * FE FF        UTF-16BE, big-endian
         * FF FE        UTF-16LE, little-endian
         * EF BB BF     UTF-8
         */

        unsigned le;
        unsigned bom = 1;                // assume there's a BOM
        if (buf[0] == 0xFF && buf[1] == 0xFE)
        {
            if (buflen >= 4 && buf[2] == 0 && buf[3] == 0)
            {   // UTF-32LE
                le = 1;

            Lutf32:
                OutBuffer dbuf;
                unsigned *pu = (unsigned *)(buf);
                unsigned *pumax = &pu[buflen / 4];

                if (buflen & 3)
                {   error("odd length of UTF-32 char source %u", buflen);
                    fatal();
                }

                dbuf.reserve(buflen / 4);
                for (pu += bom; pu < pumax; pu++)
                {   unsigned u;

                    u = le ? Port::readlongLE(pu) : Port::readlongBE(pu);
                    if (u & ~0x7F)
                    {
                        if (u > 0x10FFFF)
                        {   error("UTF-32 value %08x greater than 0x10FFFF", u);
                            fatal();
                        }
                        dbuf.writeUTF8(u);
                    }
                    else
                        dbuf.writeByte(u);
                }
                dbuf.writeByte(0);              // add 0 as sentinel for scanner
                buflen = dbuf.length() - 1;     // don't include sentinel in count
                buf = (utf8_t *) dbuf.extractData();
            }
            else
            {   // UTF-16LE (X86)
                // Convert it to UTF-8
                le = 1;

            Lutf16:
                OutBuffer dbuf;
                unsigned short *pu = (unsigned short *)(buf);
                unsigned short *pumax = &pu[buflen / 2];

                if (buflen & 1)
                {   error("odd length of UTF-16 char source %u", buflen);
                    fatal();
                }

                dbuf.reserve(buflen / 2);
                for (pu += bom; pu < pumax; pu++)
                {   unsigned u;

                    u = le ? Port::readwordLE(pu) : Port::readwordBE(pu);
                    if (u & ~0x7F)
                    {   if (u >= 0xD800 && u <= 0xDBFF)
                        {   unsigned u2;

                            if (++pu > pumax)
                            {   error("surrogate UTF-16 high value %04x at EOF", u);
                                fatal();
                            }
                            u2 = le ? Port::readwordLE(pu) : Port::readwordBE(pu);
                            if (u2 < 0xDC00 || u2 > 0xDFFF)
                            {   error("surrogate UTF-16 low value %04x out of range", u2);
                                fatal();
                            }
                            u = (u - 0xD7C0) << 10;
                            u |= (u2 - 0xDC00);
                        }
                        else if (u >= 0xDC00 && u <= 0xDFFF)
                        {   error("unpaired surrogate UTF-16 value %04x", u);
                            fatal();
                        }
                        else if (u == 0xFFFE || u == 0xFFFF)
                        {   error("illegal UTF-16 value %04x", u);
                            fatal();
                        }
                        dbuf.writeUTF8(u);
                    }
                    else
                        dbuf.writeByte(u);
                }
                dbuf.writeByte(0);              // add 0 as sentinel for scanner
                buflen = dbuf.length() - 1;     // don't include sentinel in count
                buf = (utf8_t *) dbuf.extractData();
            }
        }
        else if (buf[0] == 0xFE && buf[1] == 0xFF)
        {   // UTF-16BE
            le = 0;
            goto Lutf16;
        }
        else if (buflen >= 4 && buf[0] == 0 && buf[1] == 0 && buf[2] == 0xFE && buf[3] == 0xFF)
        {   // UTF-32BE
            le = 0;
            goto Lutf32;
        }
        else if (buflen >= 3 && buf[0] == 0xEF && buf[1] == 0xBB && buf[2] == 0xBF)
        {   // UTF-8

            buf += 3;
            buflen -= 3;
        }
        else
        {
            /* There is no BOM. Make use of Arcane Jill's insight that
             * the first char of D source must be ASCII to
             * figure out the encoding.
             */

            bom = 0;
            if (buflen >= 4)
            {   if (buf[1] == 0 && buf[2] == 0 && buf[3] == 0)
                {   // UTF-32LE
                    le = 1;
                    goto Lutf32;
                }
                else if (buf[0] == 0 && buf[1] == 0 && buf[2] == 0)
                {   // UTF-32BE
                    le = 0;
                    goto Lutf32;
                }
            }
            if (buflen >= 2)
            {
                if (buf[1] == 0)
                {   // UTF-16LE
                    le = 1;
                    goto Lutf16;
                }
                else if (buf[0] == 0)
                {   // UTF-16BE
                    le = 0;
                    goto Lutf16;
                }
            }

            // It's UTF-8
            if (buf[0] >= 0x80)
            {   error("source file must start with BOM or ASCII character, not \\x%02X", buf[0]);
                fatal();
            }
        }
    }

    /* If it starts with the string "Ddoc", then it's a documentation
     * source file.
     */
    if (buflen >= 4 && memcmp(buf, "Ddoc", 4) == 0)
    {
        comment = buf + 4;
        isDocFile = 1;
        if (!docfile)
            setDocfile();
        return this;
    }
    {
        Parser p(this, buf, buflen, docfile != NULL);
        p.nextToken();
        members = p.parseModule();
        md = p.md;
        numlines = p.scanloc.linnum;
        if (p.errors)
            ++global.errors;
    }

    if (srcfile->ref == 0)
        ::free(srcfile->buffer);
    srcfile->buffer = NULL;
    srcfile->len = 0;

    /* The symbol table into which the module is to be inserted.
     */
    DsymbolTable *dst;

    if (md)
    {
        /* A ModuleDeclaration, md, was provided.
         * The ModuleDeclaration sets the packages this module appears in, and
         * the name of this module.
         */
        this->ident = md->id;
        Package *ppack = NULL;
        dst = Package::resolve(md->packages, &this->parent, &ppack);
        assert(dst);

        Module *m = ppack ? ppack->isModule() : NULL;
        if (m && (strcmp(m->srcfile->name->name(), "package.d") != 0 &&
                  strcmp(m->srcfile->name->name(), "package.di") != 0))
        {
            ::error(md->loc, "package name '%s' conflicts with usage as a module name in file %s",
                ppack->toPrettyChars(), m->srcfile->toChars());
        }
    }
    else
    {
        /* The name of the module is set to the source file name.
         * There are no packages.
         */
        dst = modules;          // and so this module goes into global module symbol table

        /* Check to see if module name is a valid identifier
         */
        if (!Identifier::isValidIdentifier(this->ident->toChars()))
            error("has non-identifier characters in filename, use module declaration instead");
    }

    // Insert module into the symbol table
    Dsymbol *s = this;
    if (isPackageFile)
    {
        /* If the source tree is as follows:
         *     pkg/
         *     +- package.d
         *     +- common.d
         * the 'pkg' will be incorporated to the internal package tree in two ways:
         *     import pkg;
         * and:
         *     import pkg.common;
         *
         * If both are used in one compilation, 'pkg' as a module (== pkg/package.d)
         * and a package name 'pkg' will conflict each other.
         *
         * To avoid the conflict:
         * 1. If preceding package name insertion had occurred by Package::resolve,
         *    reuse the previous wrapping 'Package' if it exists
         * 2. Otherwise, 'package.d' wrapped by 'Package' is inserted to the internal tree in here.
         *
         * Then change Package::isPkgMod to PKGmodule and set Package::mod.
         *
         * Note that the 'wrapping Package' is the Package that contains package.d and other submodules,
         * the one inserted to the symbol table.
         */
        Dsymbol *ps = dst->lookup(ident);
        Package *p = ps ? ps->isPackage() : NULL;
        if (p == NULL)
        {
            p = new Package(ident);
            p->tag = this->tag; // reuse the same package tag
            p->symtab = new DsymbolTable();
        }
        this->tag= p->tag; // reuse the 'older' package tag
        this->pkg = p;
        p->parent = this->parent;
        p->isPkgMod = PKGmodule;
        p->mod = this;
        s = p;
    }
    if (!dst->insert(s))
    {
        /* It conflicts with a name that is already in the symbol table.
         * Figure out what went wrong, and issue error message.
         */
        Dsymbol *prev = dst->lookup(ident);
        assert(prev);
        if (Module *mprev = prev->isModule())
        {
            if (FileName::compare(srcname, mprev->srcfile->toChars()) != 0)
                error(loc, "from file %s conflicts with another module %s from file %s",
                    srcname, mprev->toChars(), mprev->srcfile->toChars());
            else if (isRoot() && mprev->isRoot())
                error(loc, "from file %s is specified twice on the command line",
                    srcname);
            else
                error(loc, "from file %s must be imported with 'import %s;'",
                    srcname, toPrettyChars());

            // Bugzilla 14446: Return previously parsed module to avoid AST duplication ICE.
            return mprev;
        }
        else if (Package *pkg = prev->isPackage())
        {
            // 'package.d' loaded after a previous 'Package' insertion
            if (isPackageFile)
                amodules.push(this); // Add to global array of all modules
            else
                error(md ? md->loc : loc, "from file %s conflicts with package name %s",
                    srcname, pkg->toChars());
        }
        else
            assert(global.errors);
    }
    else
    {
        // Add to global array of all modules
        amodules.push(this);
    }
    Compiler::onParseModule(this);
    return this;
}

void Module::importAll(Scope *)
{
    //printf("+Module::importAll(this = %p, '%s'): parent = %p\n", this, toChars(), parent);

    if (_scope)
        return;                 // already done

    if (isDocFile)
    {
        error("is a Ddoc file, cannot import it");
        return;
    }

    /* Note that modules get their own scope, from scratch.
     * This is so regardless of where in the syntax a module
     * gets imported, it is unaffected by context.
     * Ignore prevsc.
     */
    Scope *sc = Scope::createGlobal(this);      // create root scope

    if (md && md->msg)
      md->msg = semanticString(sc, md->msg, "deprecation message");

    // Add import of "object", even for the "object" module.
    // If it isn't there, some compiler rewrites, like
    //    classinst == classinst -> .object.opEquals(classinst, classinst)
    // would fail inside object.d.
    if (members->length == 0 || ((*members)[0])->ident != Id::object ||
        (*members)[0]->isImport() == NULL)
    {
        Import *im = new Import(Loc(), NULL, Id::object, NULL, 0);
        members->shift(im);
    }

    if (!symtab)
    {
        // Add all symbols into module's symbol table
        symtab = new DsymbolTable();
        for (size_t i = 0; i < members->length; i++)
        {
            Dsymbol *s = (*members)[i];
            s->addMember(sc, sc->scopesym);
        }
    }
    // anything else should be run after addMember, so version/debug symbols are defined

    /* Set scope for the symbols so that if we forward reference
     * a symbol, it can possibly be resolved on the spot.
     * If this works out well, it can be extended to all modules
     * before any semantic() on any of them.
     */
    setScope(sc);               // remember module scope for semantic
    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        s->setScope(sc);
    }

    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        s->importAll(sc);
    }

    sc = sc->pop();
    sc->pop();          // 2 pops because Scope::createGlobal() created 2
}

/**********************************
 * Determine if we need to generate an instance of ModuleInfo
 * for this Module.
 */

int Module::needModuleInfo()
{
    //printf("needModuleInfo() %s, %d, %d\n", toChars(), needmoduleinfo, global.params.cov);
    return needmoduleinfo || global.params.cov;
}

Dsymbol *Module::search(const Loc &loc, Identifier *ident, int flags)
{
    /* Since modules can be circularly referenced,
     * need to stop infinite recursive searches.
     * This is done with the cache.
     */

    //printf("%s Module::search('%s', flags = x%x) insearch = %d\n", toChars(), ident->toChars(), flags, insearch);
    if (insearch)
        return NULL;

    /* Qualified module searches always search their imports,
     * even if SearchLocalsOnly
     */
    if (!(flags & SearchUnqualifiedModule))
        flags &= ~(SearchUnqualifiedModule | SearchLocalsOnly);

    if (searchCacheIdent == ident && searchCacheFlags == flags)
    {
        //printf("%s Module::search('%s', flags = %d) insearch = %d searchCacheSymbol = %s\n",
        //        toChars(), ident->toChars(), flags, insearch, searchCacheSymbol ? searchCacheSymbol->toChars() : "null");
        return searchCacheSymbol;
    }

    unsigned int errors = global.errors;

    insearch = 1;
    Dsymbol *s = ScopeDsymbol::search(loc, ident, flags);
    insearch = 0;

    if (errors == global.errors)
    {
        // Bugzilla 10752: We can cache the result only when it does not cause
        // access error so the side-effect should be reproduced in later search.
        searchCacheIdent = ident;
        searchCacheSymbol = s;
        searchCacheFlags = flags;
    }
    return s;
}

bool Module::isPackageAccessible(Package *p, Prot protection, int flags)
{
    if (insearch) // don't follow import cycles
        return false;
    if (flags & IgnorePrivateImports)
        protection = Prot(Prot::public_); // only consider public imports
    insearch = true;
    bool r = ScopeDsymbol::isPackageAccessible(p, protection);
    insearch = false;
    return r;
}

Dsymbol *Module::symtabInsert(Dsymbol *s)
{
    searchCacheIdent = NULL;       // symbol is inserted, so invalidate cache
    return Package::symtabInsert(s);
}

void Module::clearCache()
{
    for (size_t i = 0; i < amodules.length; i++)
    {
        Module *m = amodules[i];
        m->searchCacheIdent = NULL;
    }
}

/*******************************************
 * Can't run semantic on s now, try again later.
 */

void Module::addDeferredSemantic(Dsymbol *s)
{
    // Don't add it if it is already there
    for (size_t i = 0; i < deferred.length; i++)
    {
        Dsymbol *sd = deferred[i];

        if (sd == s)
            return;
    }

    //printf("Module::addDeferredSemantic('%s')\n", s->toChars());
    deferred.push(s);
}

void Module::addDeferredSemantic2(Dsymbol *s)
{
    //printf("Module::addDeferredSemantic2('%s')\n", s->toChars());
    deferred2.push(s);
}

void Module::addDeferredSemantic3(Dsymbol *s)
{
    //printf("Module::addDeferredSemantic3('%s')\n", s->toChars());
    deferred3.push(s);
}

/******************************************
 * Run semantic() on deferred symbols.
 */

void Module::runDeferredSemantic()
{
    if (dprogress == 0)
        return;

    static int nested;
    if (nested)
        return;
    //if (deferred.length) printf("+Module::runDeferredSemantic(), len = %d\n", deferred.length);
    nested++;

    size_t len;
    do
    {
        dprogress = 0;
        len = deferred.length;
        if (!len)
            break;

        Dsymbol **todo;
        Dsymbol **todoalloc = NULL;
        Dsymbol *tmp;
        if (len == 1)
        {
            todo = &tmp;
        }
        else
        {
            todo = (Dsymbol **)mem.xmalloc(len * sizeof(Dsymbol *));
            todoalloc = todo;
        }
        memcpy(todo, deferred.tdata(), len * sizeof(Dsymbol *));
        deferred.setDim(0);

        for (size_t i = 0; i < len; i++)
        {
            Dsymbol *s = todo[i];

            dsymbolSemantic(s, NULL);
            //printf("deferred: %s, parent = %s\n", s->toChars(), s->parent->toChars());
        }
        //printf("\tdeferred.length = %d, len = %d, dprogress = %d\n", deferred.length, len, dprogress);
        if (todoalloc)
            free(todoalloc);
    } while (deferred.length < len || dprogress);  // while making progress
    nested--;
    //printf("-Module::runDeferredSemantic(), len = %d\n", deferred.length);
}

void Module::runDeferredSemantic2()
{
    Module::runDeferredSemantic();

    Dsymbols *a = &Module::deferred2;
    for (size_t i = 0; i < a->length; i++)
    {
        Dsymbol *s = (*a)[i];
        //printf("[%d] %s semantic2a\n", i, s->toPrettyChars());
        semantic2(s, NULL);

        if (global.errors)
            break;
    }
    a->setDim(0);
}

void Module::runDeferredSemantic3()
{
    Module::runDeferredSemantic2();

    Dsymbols *a = &Module::deferred3;
    for (size_t i = 0; i < a->length; i++)
    {
        Dsymbol *s = (*a)[i];
        //printf("[%d] %s semantic3a\n", i, s->toPrettyChars());

        semantic3(s, NULL);

        if (global.errors)
            break;
    }
    a->setDim(0);
}

/************************************
 * Recursively look at every module this module imports,
 * return true if it imports m.
 * Can be used to detect circular imports.
 */

int Module::imports(Module *m)
{
    //printf("%s Module::imports(%s)\n", toChars(), m->toChars());
    for (size_t i = 0; i < aimports.length; i++)
    {
        Module *mi = aimports[i];
        if (mi == m)
            return true;
        if (!mi->insearch)
        {
            mi->insearch = 1;
            int r = mi->imports(m);
            if (r)
                return r;
        }
    }
    return false;
}

/*************************************
 * Return true if module imports itself.
 */

bool Module::selfImports()
{
    //printf("Module::selfImports() %s\n", toChars());
    if (selfimports == 0)
    {
        for (size_t i = 0; i < amodules.length; i++)
            amodules[i]->insearch = 0;

        selfimports = imports(this) + 1;

        for (size_t i = 0; i < amodules.length; i++)
            amodules[i]->insearch = 0;
    }
    return selfimports == 2;
}

/*************************************
 * Return true if module imports root module.
 */

bool Module::rootImports()
{
    //printf("Module::rootImports() %s\n", toChars());
    if (rootimports == 0)
    {
        for (size_t i = 0; i < amodules.length; i++)
            amodules[i]->insearch = 0;

        rootimports = 1;
        for (size_t i = 0; i < amodules.length; ++i)
        {
            Module *m = amodules[i];
            if (m->isRoot() && imports(m))
            {
                rootimports = 2;
                break;
            }
        }

        for (size_t i = 0; i < amodules.length; i++)
            amodules[i]->insearch = 0;
    }
    return rootimports == 2;
}

bool Module::isCoreModule(Identifier *ident)
{
    return this->ident == ident && parent && parent->ident == Id::core && !parent->parent;
}

/* =========================== ModuleDeclaration ===================== */

ModuleDeclaration::ModuleDeclaration(Loc loc, Identifiers *packages, Identifier *id)
{
    this->loc = loc;
    this->packages = packages;
    this->id = id;
    this->isdeprecated = false;
    this->msg = NULL;
}

const char *ModuleDeclaration::toChars()
{
    OutBuffer buf;

    if (packages && packages->length)
    {
        for (size_t i = 0; i < packages->length; i++)
        {
            Identifier *pid = (*packages)[i];
            buf.writestring(pid->toChars());
            buf.writeByte('.');
        }
    }
    buf.writestring(id->toChars());
    return buf.extractChars();
}

/* =========================== Package ===================== */

Package::Package(Identifier *ident)
        : ScopeDsymbol(ident)
{
    this->isPkgMod = PKGunknown;
    this->mod = NULL;
    static unsigned packageTag = 0;
    this->tag = packageTag++;
}


const char *Package::kind() const
{
    return "package";
}

Module *Package::isPackageMod()
{
    if (isPkgMod == PKGmodule)
    {
        return mod;
    }
    return NULL;
}

/**
 * Checks for the existence of a package.d to set isPkgMod appropriately
 * if isPkgMod == PKGunknown
 */
void Package::resolvePKGunknown()
{
    if (isModule())
        return;
    if (isPkgMod != PKGunknown)
        return;

    Identifiers packages;
    for (Dsymbol *s = this->parent; s; s = s->parent)
        packages.insert(0, s->ident);

    if (lookForSourceFile(getFilename(&packages, ident)))
        Module::load(Loc(), &packages, this->ident);
    else
        isPkgMod = PKGpackage;
}

/**
 * Checks if pkg is a sub-package of this
 *
 * For example, if this qualifies to 'a1.a2' and pkg - to 'a1.a2.a3',
 * this function returns 'true'. If it is other way around or qualified
 * package paths conflict function returns 'false'.
 *
 * Params:
 *  pkg = possible subpackage
 *
 * Returns:
 *  see description
 */
bool Package::isAncestorPackageOf(const Package * const pkg) const
{
    if (this == pkg)
        return true;
    if (!pkg || !pkg->parent)
        return false;
    return isAncestorPackageOf(pkg->parent->isPackage());
}

/****************************************************
 * Input:
 *      packages[]      the pkg1.pkg2 of pkg1.pkg2.mod
 * Returns:
 *      the symbol table that mod should be inserted into
 * Output:
 *      *pparent        the rightmost package, i.e. pkg2, or NULL if no packages
 *      *ppkg           the leftmost package, i.e. pkg1, or NULL if no packages
 */

DsymbolTable *Package::resolve(Identifiers *packages, Dsymbol **pparent, Package **ppkg)
{
    DsymbolTable *dst = Module::modules;
    Dsymbol *parent = NULL;

    //printf("Package::resolve()\n");
    if (ppkg)
        *ppkg = NULL;

    if (packages)
    {
        for (size_t i = 0; i < packages->length; i++)
        {
            Identifier *pid = (*packages)[i];
            Package *pkg;
            Dsymbol *p = dst->lookup(pid);
            if (!p)
            {
                pkg = new Package(pid);
                dst->insert(pkg);
                pkg->parent = parent;
                pkg->symtab = new DsymbolTable();
            }
            else
            {
                pkg = p->isPackage();
                assert(pkg);
                // It might already be a module, not a package, but that needs
                // to be checked at a higher level, where a nice error message
                // can be generated.
                // dot net needs modules and packages with same name

                // But we still need a symbol table for it
                if (!pkg->symtab)
                    pkg->symtab = new DsymbolTable();
            }
            parent = pkg;
            dst = pkg->symtab;
            if (ppkg && !*ppkg)
                *ppkg = pkg;
            if (pkg->isModule())
            {
                // Return the module so that a nice error message can be generated
                if (ppkg)
                    *ppkg = (Package *)p;
                break;
            }
        }
    }
    if (pparent)
        *pparent = parent;
    return dst;
}

Dsymbol *Package::search(const Loc &loc, Identifier *ident, int flags)
{
    //printf("%s Package::search('%s', flags = x%x)\n", toChars(), ident->toChars(), flags);
    flags &= ~SearchLocalsOnly;  // searching an import is always transitive
    if (!isModule() && mod)
    {
        // Prefer full package name.
        Dsymbol *s = symtab ? symtab->lookup(ident) : NULL;
        if (s)
            return s;
        //printf("[%s] through pkdmod: %s\n", loc.toChars(), toChars());
        return mod->search(loc, ident, flags);
    }

    return ScopeDsymbol::search(loc, ident, flags);
}
