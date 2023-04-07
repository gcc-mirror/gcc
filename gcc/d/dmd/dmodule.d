/**
 * Defines a package and module.
 *
 * Specification: $(LINK2 https://dlang.org/spec/module.html, Modules)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dmodule.d, _dmodule.d)
 * Documentation:  https://dlang.org/phobos/dmd_dmodule.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dmodule.d
 */

module dmd.dmodule;

import core.stdc.stdio;
import core.stdc.stdlib;
import core.stdc.string;
import dmd.aggregate;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.compiler;
import dmd.gluelayer;
import dmd.dimport;
import dmd.dmacro;
import dmd.doc;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.errors;
import dmd.errorsink;
import dmd.expression;
import dmd.expressionsem;
import dmd.file_manager;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.parse;
import dmd.cparse;
import dmd.root.array;
import dmd.root.file;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.port;
import dmd.root.rmem;
import dmd.root.rootobject;
import dmd.root.string;
import dmd.semantic2;
import dmd.semantic3;
import dmd.target;
import dmd.utils;
import dmd.visitor;

// function used to call semantic3 on a module's dependencies
void semantic3OnDependencies(Module m)
{
    if (!m)
        return;

    if (m.semanticRun > PASS.semantic3)
        return;

    m.semantic3(null);

    foreach (i; 1 .. m.aimports.length)
        semantic3OnDependencies(m.aimports[i]);
}

/**
 * Remove generated .di files on error and exit
 */
void removeHdrFilesAndFail(ref Param params, ref Modules modules) nothrow
{
    if (params.dihdr.doOutput)
    {
        foreach (m; modules)
        {
            if (m.filetype == FileType.dhdr)
                continue;
            File.remove(m.hdrfile.toChars());
        }
    }

    fatal();
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
private const(char)[] getFilename(Identifier[] packages, Identifier ident) nothrow
{
    const(char)[] filename = ident.toString();

    OutBuffer buf;
    OutBuffer dotmods;
    auto modAliases = &global.params.modFileAliasStrings;

    if (packages.length == 0 && modAliases.length == 0)
        return filename;

    void checkModFileAlias(const(char)[] p)
    {
        /* Check and replace the contents of buf[] with
        * an alias string from global.params.modFileAliasStrings[]
        */
        dotmods.writestring(p);
        foreach_reverse (const m; *modAliases)
        {
            const q = strchr(m, '=');
            assert(q);
            if (dotmods.length == q - m && memcmp(dotmods.peekChars(), m, q - m) == 0)
            {
                buf.setsize(0);
                auto rhs = q[1 .. strlen(q)];
                if (rhs.length > 0 && (rhs[$ - 1] == '/' || rhs[$ - 1] == '\\'))
                    rhs = rhs[0 .. $ - 1]; // remove trailing separator
                buf.writestring(rhs);
                break; // last matching entry in ms[] wins
            }
        }
        dotmods.writeByte('.');
    }

    foreach (pid; packages)
    {
        const p = pid.toString();
        buf.writestring(p);
        if (modAliases.length)
            checkModFileAlias(p);
        version (Windows)
            enum FileSeparator = '\\';
        else
            enum FileSeparator = '/';
        buf.writeByte(FileSeparator);
    }
    buf.writestring(filename);
    if (modAliases.length)
        checkModFileAlias(filename);
    buf.writeByte(0);
    filename = buf.extractSlice()[0 .. $ - 1];

    return filename;
}

/***********************************************************
 */
extern (C++) class Package : ScopeDsymbol
{
    PKG isPkgMod = PKG.unknown;
    uint tag;        // auto incremented tag, used to mask package tree in scopes
    Module mod;     // !=null if isPkgMod == PKG.module_

    final extern (D) this(const ref Loc loc, Identifier ident) nothrow
    {
        super(loc, ident);
        __gshared uint packageTag;
        this.tag = packageTag++;
    }

    override const(char)* kind() const nothrow
    {
        return "package";
    }

    override bool equals(const RootObject o) const
    {
        // custom 'equals' for bug 17441. "package a" and "module a" are not equal
        if (this == o)
            return true;
        auto p = cast(Package)o;
        return p && isModule() == p.isModule() && ident.equals(p.ident);
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
    extern (D) static DsymbolTable resolve(Identifier[] packages, Dsymbol* pparent, Package* ppkg)
    {
        DsymbolTable dst = Module.modules;
        Dsymbol parent = null;
        //printf("Package::resolve()\n");
        if (ppkg)
            *ppkg = null;
        foreach (pid; packages)
        {
            Package pkg;
            Dsymbol p = dst.lookup(pid);
            if (!p)
            {
                pkg = new Package(Loc.initial, pid);
                dst.insert(pkg);
                pkg.parent = parent;
                pkg.symtab = new DsymbolTable();
            }
            else
            {
                pkg = p.isPackage();
                assert(pkg);
                // It might already be a module, not a package, but that needs
                // to be checked at a higher level, where a nice error message
                // can be generated.
                // dot net needs modules and packages with same name
                // But we still need a symbol table for it
                if (!pkg.symtab)
                    pkg.symtab = new DsymbolTable();
            }
            parent = pkg;
            dst = pkg.symtab;
            if (ppkg && !*ppkg)
                *ppkg = pkg;
            if (pkg.isModule())
            {
                // Return the module so that a nice error message can be generated
                if (ppkg)
                    *ppkg = cast(Package)p;
                break;
            }
        }

        if (pparent)
            *pparent = parent;
        return dst;
    }

    override final inout(Package) isPackage() inout
    {
        return this;
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
    final bool isAncestorPackageOf(const Package pkg) const
    {
        if (this == pkg)
            return true;
        if (!pkg || !pkg.parent)
            return false;
        return isAncestorPackageOf(pkg.parent.isPackage());
    }

    override Dsymbol search(const ref Loc loc, Identifier ident, int flags = SearchLocalsOnly)
    {
        //printf("%s Package.search('%s', flags = x%x)\n", toChars(), ident.toChars(), flags);
        flags &= ~SearchLocalsOnly;  // searching an import is always transitive
        if (!isModule() && mod)
        {
            // Prefer full package name.
            Dsymbol s = symtab ? symtab.lookup(ident) : null;
            if (s)
                return s;
            //printf("[%s] through pkdmod: %s\n", loc.toChars(), toChars());
            return mod.search(loc, ident, flags);
        }
        return ScopeDsymbol.search(loc, ident, flags);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    final Module isPackageMod()
    {
        if (isPkgMod == PKG.module_)
        {
            return mod;
        }
        return null;
    }

    /**
     * Checks for the existence of a package.d to set isPkgMod appropriately
     * if isPkgMod == PKG.unknown
     */
    final void resolvePKGunknown()
    {
        if (isModule())
            return;
        if (isPkgMod != PKG.unknown)
            return;

        Identifier[] packages;
        for (Dsymbol s = this.parent; s; s = s.parent)
            packages ~= s.ident;
        reverse(packages);

        if (Module.find(getFilename(packages, ident)))
            Module.load(Loc.initial, packages, this.ident);
        else
            isPkgMod = PKG.package_;
    }
}

/***********************************************************
 */
extern (C++) final class Module : Package
{
    extern (C++) __gshared Module rootModule;
    extern (C++) __gshared DsymbolTable modules; // symbol table of all modules
    extern (C++) __gshared Modules amodules;     // array of all modules
    extern (C++) __gshared Dsymbols deferred;    // deferred Dsymbol's needing semantic() run on them
    extern (C++) __gshared Dsymbols deferred2;   // deferred Dsymbol's needing semantic2() run on them
    extern (C++) __gshared Dsymbols deferred3;   // deferred Dsymbol's needing semantic3() run on them

    static void _init()
    {
        modules = new DsymbolTable();
    }

    /**
     * Deinitializes the global state of the compiler.
     *
     * This can be used to restore the state set by `_init` to its original
     * state.
     */
    static void deinitialize()
    {
        modules = modules.init;
    }

    extern (C++) __gshared AggregateDeclaration moduleinfo;

    const(char)[] arg;           // original argument name
    ModuleDeclaration* md;      // if !=null, the contents of the ModuleDeclaration declaration
    const FileName srcfile;     // input source file
    const FileName objfile;     // output .obj file
    const FileName hdrfile;     // 'header' file
    FileName docfile;           // output documentation file
    const(ubyte)[] src;         /// Raw content of the file
    uint errors;                // if any errors in file
    uint numlines;              // number of lines in source file
    FileType filetype;          // source file type
    bool hasAlwaysInlines;      // contains references to functions that must be inlined
    bool isPackageFile;         // if it is a package.d
    Package pkg;                // if isPackageFile is true, the Package that contains this package.d
    Strings contentImportedFiles; // array of files whose content was imported
    int needmoduleinfo;
    private ThreeState selfimports;
    private ThreeState rootimports;
    Dsymbol[void*] tagSymTab;   /// ImportC: tag symbols that conflict with other symbols used as the index

    private OutBuffer defines;  // collect all the #define lines here


    /*************************************
     * Return true if module imports itself.
     */
    bool selfImports()
    {
        //printf("Module::selfImports() %s\n", toChars());
        if (selfimports == ThreeState.none)
        {
            foreach (Module m; amodules)
                m.insearch = false;
            selfimports = imports(this) ? ThreeState.yes : ThreeState.no;
            foreach (Module m; amodules)
                m.insearch = false;
        }
        return selfimports == ThreeState.yes;
    }

    /*************************************
     * Return true if module imports root module.
     */
    bool rootImports()
    {
        //printf("Module::rootImports() %s\n", toChars());
        if (rootimports == ThreeState.none)
        {
            foreach (Module m; amodules)
                m.insearch = false;
            rootimports = ThreeState.no;
            foreach (Module m; amodules)
            {
                if (m.isRoot() && imports(m))
                {
                    rootimports = ThreeState.yes;
                    break;
                }
            }
            foreach (Module m; amodules)
                m.insearch = false;
        }
        return rootimports == ThreeState.yes;
    }

    private Identifier searchCacheIdent;
    private Dsymbol searchCacheSymbol;  // cached value of search
    private int searchCacheFlags;       // cached flags
    private bool insearch;

    /**
     * A root module is one that will be compiled all the way to
     * object code.  This field holds the root module that caused
     * this module to be loaded.  If this module is a root module,
     * then it will be set to `this`.  This is used to determine
     * ownership of template instantiation.
     */
    Module importedFrom;

    Dsymbols* decldefs;         // top level declarations for this Module

    Modules aimports;           // all imported modules

    uint debuglevel;            // debug level
    Identifiers* debugids;      // debug identifiers
    Identifiers* debugidsNot;   // forward referenced debug identifiers

    uint versionlevel;          // version level
    Identifiers* versionids;    // version identifiers
    Identifiers* versionidsNot; // forward referenced version identifiers

    MacroTable macrotable;      // document comment macros
    Escape* _escapetable;       // document comment escapes

    size_t nameoffset;          // offset of module name from start of ModuleInfo
    size_t namelen;             // length of module name in characters

    extern (D) this(const ref Loc loc, const(char)[] filename, Identifier ident, int doDocComment, int doHdrGen)
    {
        super(loc, ident);
        const(char)[] srcfilename;
        //printf("Module::Module(filename = '%.*s', ident = '%s')\n", cast(int)filename.length, filename.ptr, ident.toChars());
        this.arg = filename;
        srcfilename = FileName.defaultExt(filename, mars_ext);
        if (target.run_noext && global.params.run &&
            !FileName.ext(filename) &&
            FileName.exists(srcfilename) == 0 &&
            FileName.exists(filename) == 1)
        {
            FileName.free(srcfilename.ptr);
            srcfilename = FileName.removeExt(filename); // just does a mem.strdup(filename)
        }
        else if (!FileName.equalsExt(srcfilename, mars_ext) &&
                 !FileName.equalsExt(srcfilename, hdr_ext) &&
                 !FileName.equalsExt(srcfilename, c_ext) &&
                 !FileName.equalsExt(srcfilename, i_ext) &&
                 !FileName.equalsExt(srcfilename, dd_ext))
        {

            error("source file name '%.*s' must have .%.*s extension",
                  cast(int)srcfilename.length, srcfilename.ptr,
                  cast(int)mars_ext.length, mars_ext.ptr);
            fatal();
        }

        srcfile = FileName(srcfilename);
        objfile = setOutfilename(global.params.objname, global.params.objdir, filename, target.obj_ext);
        if (doDocComment)
            setDocfile();
        if (doHdrGen)
            hdrfile = setOutfilename(global.params.dihdr.name, global.params.dihdr.dir, arg, hdr_ext);
    }

    extern (D) this(const(char)[] filename, Identifier ident, int doDocComment, int doHdrGen)
    {
        this(Loc.initial, filename, ident, doDocComment, doHdrGen);
    }

    static Module create(const(char)* filename, Identifier ident, int doDocComment, int doHdrGen)
    {
        return create(filename.toDString, ident, doDocComment, doHdrGen);
    }

    extern (D) static Module create(const(char)[] filename, Identifier ident, int doDocComment, int doHdrGen)
    {
        return new Module(Loc.initial, filename, ident, doDocComment, doHdrGen);
    }

    static const(char)* find(const(char)* filename)
    {
        return find(filename.toDString).ptr;
    }

    extern (D) static const(char)[] find(const(char)[] filename)
    {
        return global.fileManager.lookForSourceFile(filename, global.path ? (*global.path)[] : null);
    }

    extern (C++) static Module load(const ref Loc loc, Identifiers* packages, Identifier ident)
    {
        return load(loc, packages ? (*packages)[] : null, ident);
    }

    extern (D) static Module load(const ref Loc loc, Identifier[] packages, Identifier ident)
    {
        //printf("Module::load(ident = '%s')\n", ident.toChars());
        // Build module filename by turning:
        //  foo.bar.baz
        // into:
        //  foo\bar\baz
        const(char)[] filename = getFilename(packages, ident);
        // Look for the source file
        if (const result = find(filename))
            filename = result; // leaks

        auto m = new Module(loc, filename, ident, 0, 0);

        if (!m.read(loc))
            return null;
        if (global.params.verbose)
        {
            OutBuffer buf;
            foreach (pid; packages)
            {
                buf.writestring(pid.toString());
                buf.writeByte('.');
            }
            buf.printf("%s\t(%s)", ident.toChars(), m.srcfile.toChars());
            message("import    %s", buf.peekChars());
        }
        if((m = m.parse()) is null) return null;

        return m;
    }

    override const(char)* kind() const
    {
        return "module";
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
    extern(D) FileName setOutfilename(const(char)[] name, const(char)[] dir, const(char)[] arg, const(char)[] ext)
    {
        const(char)[] docfilename;
        if (name)
        {
            docfilename = name;
        }
        else
        {
            const(char)[] argdoc;
            OutBuffer buf;
            if (arg == "__stdin.d")
            {
                version (Posix)
                    import core.sys.posix.unistd : getpid;
                else version (Windows)
                    import core.sys.windows.winbase : getpid = GetCurrentProcessId;
                buf.printf("__stdin_%d.d", getpid());
                arg = buf[];
            }
            if (global.params.preservePaths)
                argdoc = arg;
            else
                argdoc = FileName.name(arg);
            // If argdoc doesn't have an absolute path, make it relative to dir
            if (!FileName.absolute(argdoc))
            {
                //FileName::ensurePathExists(dir);
                argdoc = FileName.combine(dir, argdoc);
            }
            docfilename = FileName.forceExt(argdoc, ext);
        }
        if (FileName.equals(docfilename, srcfile.toString()))
        {
            error("source file and output file have same name '%s'", srcfile.toChars());
            fatal();
        }
        return FileName(docfilename);
    }

    extern (D) void setDocfile()
    {
        docfile = setOutfilename(global.params.ddoc.name, global.params.ddoc.dir, arg, doc_ext);
    }

    /**
     * Trigger the relevant semantic error when a file cannot be read
     *
     * We special case `object.d` as a failure is likely to be a rare
     * but difficult to diagnose case for the user. Packages also require
     * special handling to avoid exposing the compiler's internals.
     *
     * Params:
     *  loc = The location at which the file read originated (e.g. import)
     */
    private void onFileReadError(const ref Loc loc)
    {
        if (FileName.equals(srcfile.toString(), "object.d"))
        {
            .error(loc, "cannot find source code for runtime library file 'object.d'");
            errorSupplemental(loc, "dmd might not be correctly installed. Run 'dmd -man' for installation instructions.");
            const dmdConfFile = global.inifilename.length ? FileName.canonicalName(global.inifilename) : "not found";
            errorSupplemental(loc, "config file: %.*s", cast(int)dmdConfFile.length, dmdConfFile.ptr);
        }
        else if (FileName.ext(this.arg) || !loc.isValid())
        {
            // Modules whose original argument name has an extension, or do not
            // have a valid location come from the command-line.
            // Error that their file cannot be found and return early.
            .error(loc, "cannot find input file `%s`", srcfile.toChars());
        }
        else
        {
            // if module is not named 'package' but we're trying to read 'package.d', we're looking for a package module
            bool isPackageMod = (strcmp(toChars(), "package") != 0) && isPackageFileName(srcfile);
            if (isPackageMod)
                .error(loc, "importing package '%s' requires a 'package.d' file which cannot be found in '%s'", toChars(), srcfile.toChars());
            else
            {
                .error(loc, "unable to read module `%s`", toChars());
                const pkgfile = FileName.combine(FileName.removeExt(srcfile.toString()), package_d);
                .errorSupplemental(loc, "Expected '%s' or '%s' in one of the following import paths:",
                    srcfile.toChars(), pkgfile.ptr);
            }
        }
        if (!global.gag)
        {
            /* Print path
             */
            if (global.path)
            {
                foreach (i, p; *global.path)
                    fprintf(stderr, "import path[%llu] = %s\n", cast(ulong)i, p);
            }
            else
            {
                fprintf(stderr, "Specify path to file '%s' with -I switch\n", srcfile.toChars());
            }

            removeHdrFilesAndFail(global.params, Module.amodules);
        }
    }

    /**
     * Reads the file from `srcfile` and loads the source buffer.
     *
     * If makefile module dependency is requested, we add this module
     * to the list of dependencies from here.
     *
     * Params:
     *  loc = the location
     *
     * Returns: `true` if successful
     */
    bool read(const ref Loc loc)
    {
        if (this.src)
            return true; // already read

        //printf("Module::read('%s') file '%s'\n", toChars(), srcfile.toChars());

        /* Preprocess the file if it's a .c file
         */
        FileName filename = srcfile;
        bool ifile = false;             // did we generate a .i file
        scope (exit)
        {
            if (ifile)
                File.remove(filename.toChars());        // remove generated file
        }

        if (global.preprocess &&
            FileName.equalsExt(srcfile.toString(), c_ext) &&
            FileName.exists(srcfile.toString()))
        {
            filename = global.preprocess(srcfile, loc, ifile, &defines);  // run C preprocessor
        }

        if (auto result = global.fileManager.lookup(filename))
        {
            this.src = result;
            if (global.params.makeDeps.doOutput)
                global.params.makeDeps.files.push(srcfile.toChars());
            return true;
        }

        this.onFileReadError(loc);
        return false;
    }

    /// syntactic parse
    Module parse()
    {
        return parseModule!ASTCodegen();
    }

    /// ditto
    extern (D) Module parseModule(AST)()
    {
        const(char)* srcname = srcfile.toChars();
        //printf("Module::parse(srcname = '%s')\n", srcname);
        isPackageFile = isPackageFileName(srcfile);
        const(char)[] buf = processSource(src, this);
        // an error happened on UTF conversion
        if (buf is null) return null;

        /* If it starts with the string "Ddoc", then it's a documentation
         * source file.
         */
        if (buf.length>= 4 && buf[0..4] == "Ddoc")
        {
            comment = buf.ptr + 4;
            filetype = FileType.ddoc;
            if (!docfile)
                setDocfile();
            return this;
        }
        /* If it has the extension ".dd", it is also a documentation
         * source file. Documentation source files may begin with "Ddoc"
         * but do not have to if they have the .dd extension.
         * https://issues.dlang.org/show_bug.cgi?id=15465
         */
        if (FileName.equalsExt(arg, dd_ext))
        {
            comment = buf.ptr; // the optional Ddoc, if present, is handled above.
            filetype = FileType.ddoc;
            if (!docfile)
                setDocfile();
            return this;
        }
        /* If it has the extension ".di", it is a "header" file.
         */
        if (FileName.equalsExt(arg, hdr_ext))
            filetype = FileType.dhdr;

        /// Promote `this` to a root module if requested via `-i`
        void checkCompiledImport()
        {
            if (!this.isRoot() && Compiler.onImport(this))
                this.importedFrom = this;
        }

        DsymbolTable dst;
        Package ppack = null;

        /* If it has the extension ".c", it is a "C" file.
         * If it has the extension ".i", it is a preprocessed "C" file.
         */
        if (FileName.equalsExt(arg, c_ext) || FileName.equalsExt(arg, i_ext))
        {
            filetype = FileType.c;

            scope p = new CParser!AST(this, buf, cast(bool) docfile, global.errorSink, target.c, &defines);
            p.nextToken();
            checkCompiledImport();
            members = p.parseModule();
            assert(!p.md); // C doesn't have module declarations
            numlines = p.scanloc.linnum;
        }
        else
        {
            scope p = new Parser!AST(this, buf, cast(bool) docfile, global.errorSink);
            p.nextToken();
            p.parseModuleDeclaration();
            md = p.md;

            if (md)
            {
                /* A ModuleDeclaration, md, was provided.
                * The ModuleDeclaration sets the packages this module appears in, and
                * the name of this module.
                */
                this.ident = md.id;
                dst = Package.resolve(md.packages, &this.parent, &ppack);
            }

            // Done after parsing the module header because `module x.y.z` may override the file name
            checkCompiledImport();

            members = p.parseModuleContent();
            numlines = p.scanloc.linnum;
        }

        /* The symbol table into which the module is to be inserted.
         */

        if (md)
        {
            // Mark the package path as accessible from the current module
            // https://issues.dlang.org/show_bug.cgi?id=21661
            // Code taken from Import.addPackageAccess()
            if (md.packages.length > 0)
            {
                // module a.b.c.d;
                auto p = ppack; // a
                addAccessiblePackage(p, Visibility(Visibility.Kind.private_));
                foreach (id; md.packages[1 .. $]) // [b, c]
                {
                    p = cast(Package) p.symtab.lookup(id);
                    if (p is null)
                        break;
                    addAccessiblePackage(p, Visibility(Visibility.Kind.private_));
                }
            }
            assert(dst);
            Module m = ppack ? ppack.isModule() : null;
            if (m && !isPackageFileName(m.srcfile))
            {
                .error(md.loc, "package name '%s' conflicts with usage as a module name in file %s", ppack.toPrettyChars(), m.srcfile.toChars());
            }
        }
        else
        {
            /* The name of the module is set to the source file name.
             * There are no packages.
             */
            dst = modules; // and so this module goes into global module symbol table
            /* Check to see if module name is a valid identifier
             */
            if (!Identifier.isValidIdentifier(this.ident.toChars()))
                error("has non-identifier characters in filename, use module declaration instead");
        }
        // Insert module into the symbol table
        Dsymbol s = this;
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
             * Then change Package::isPkgMod to PKG.module_ and set Package::mod.
             *
             * Note that the 'wrapping Package' is the Package that contains package.d and other submodules,
             * the one inserted to the symbol table.
             */
            auto ps = dst.lookup(ident);
            Package p = ps ? ps.isPackage() : null;
            if (p is null)
            {
                p = new Package(Loc.initial, ident);
                p.tag = this.tag; // reuse the same package tag
                p.symtab = new DsymbolTable();
            }
            this.tag = p.tag; // reuse the 'older' package tag
            this.pkg = p;
            p.parent = this.parent;
            p.isPkgMod = PKG.module_;
            p.mod = this;
            s = p;
        }
        if (!dst.insert(s))
        {
            /* It conflicts with a name that is already in the symbol table.
             * Figure out what went wrong, and issue error message.
             */
            Dsymbol prev = dst.lookup(ident);
            assert(prev);
            if (Module mprev = prev.isModule())
            {
                if (!FileName.equals(srcname, mprev.srcfile.toChars()))
                    error(loc, "from file %s conflicts with another module %s from file %s", srcname, mprev.toChars(), mprev.srcfile.toChars());
                else if (isRoot() && mprev.isRoot())
                    error(loc, "from file %s is specified twice on the command line", srcname);
                else
                    error(loc, "from file %s must be imported with 'import %s;'", srcname, toPrettyChars());
                // https://issues.dlang.org/show_bug.cgi?id=14446
                // Return previously parsed module to avoid AST duplication ICE.
                return mprev;
            }
            else if (Package pkg = prev.isPackage())
            {
                // 'package.d' loaded after a previous 'Package' insertion
                if (isPackageFile)
                    amodules.push(this); // Add to global array of all modules
                else
                    error(md ? md.loc : loc, "from file %s conflicts with package name %s", srcname, pkg.toChars());
            }
            else
                assert(global.errors);
        }
        else
        {
            // Add to global array of all modules
            amodules.push(this);
        }
        Compiler.onParseModule(this);
        return this;
    }

    override void importAll(Scope* prevsc)
    {
        //printf("+Module::importAll(this = %p, '%s'): parent = %p\n", this, toChars(), parent);
        if (_scope)
            return; // already done
        if (filetype == FileType.ddoc)
        {
            error("is a Ddoc file, cannot import it");
            return;
        }

        /* Note that modules get their own scope, from scratch.
         * This is so regardless of where in the syntax a module
         * gets imported, it is unaffected by context.
         * Ignore prevsc.
         */
        Scope* sc = Scope.createGlobal(this); // create root scope

        if (md && md.msg)
            md.msg = semanticString(sc, md.msg, "deprecation message");

        // Add import of "object", even for the "object" module.
        // If it isn't there, some compiler rewrites, like
        //    classinst == classinst -> .object.opEquals(classinst, classinst)
        // would fail inside object.d.
        if (filetype != FileType.c &&
            (members.length == 0 ||
             (*members)[0].ident != Id.object ||
             (*members)[0].isImport() is null))
        {
            auto im = new Import(Loc.initial, null, Id.object, null, 0);
            members.shift(im);
        }
        if (!symtab)
        {
            // Add all symbols into module's symbol table
            symtab = new DsymbolTable();
            for (size_t i = 0; i < members.length; i++)
            {
                Dsymbol s = (*members)[i];
                s.addMember(sc, sc.scopesym);
            }
        }
        // anything else should be run after addMember, so version/debug symbols are defined
        /* Set scope for the symbols so that if we forward reference
         * a symbol, it can possibly be resolved on the spot.
         * If this works out well, it can be extended to all modules
         * before any semantic() on any of them.
         */
        setScope(sc); // remember module scope for semantic
        for (size_t i = 0; i < members.length; i++)
        {
            Dsymbol s = (*members)[i];
            s.setScope(sc);
        }
        for (size_t i = 0; i < members.length; i++)
        {
            Dsymbol s = (*members)[i];
            s.importAll(sc);
        }
        sc = sc.pop();
        sc.pop(); // 2 pops because Scope.createGlobal() created 2
    }

    /**********************************
     * Determine if we need to generate an instance of ModuleInfo
     * for this Module.
     */
    int needModuleInfo()
    {
        //printf("needModuleInfo() %s, %d, %d\n", toChars(), needmoduleinfo, global.params.cov);
        return needmoduleinfo || global.params.cov;
    }

    /*******************************************
     * Print deprecation warning if we're deprecated, when
     * this module is imported from scope sc.
     *
     * Params:
     *  sc = the scope into which we are imported
     *  loc = the location of the import statement
     */
    void checkImportDeprecation(const ref Loc loc, Scope* sc)
    {
        if (md && md.isdeprecated && !sc.isDeprecated)
        {
            Expression msg = md.msg;
            if (StringExp se = msg ? msg.toStringExp() : null)
            {
                const slice = se.peekString();
                if (slice.length)
                {
                    deprecation(loc, "is deprecated - %.*s", cast(int)slice.length, slice.ptr);
                    return;
                }
            }
            deprecation(loc, "is deprecated");
        }
    }

    override Dsymbol search(const ref Loc loc, Identifier ident, int flags = SearchLocalsOnly)
    {
        /* Since modules can be circularly referenced,
         * need to stop infinite recursive searches.
         * This is done with the cache.
         */
        //printf("%s Module.search('%s', flags = x%x) insearch = %d\n", toChars(), ident.toChars(), flags, insearch);
        if (insearch)
            return null;

        /* Qualified module searches always search their imports,
         * even if SearchLocalsOnly
         */
        if (!(flags & SearchUnqualifiedModule))
            flags &= ~(SearchUnqualifiedModule | SearchLocalsOnly);

        if (searchCacheIdent == ident && searchCacheFlags == flags)
        {
            //printf("%s Module::search('%s', flags = %d) insearch = %d searchCacheSymbol = %s\n",
            //        toChars(), ident.toChars(), flags, insearch, searchCacheSymbol ? searchCacheSymbol.toChars() : "null");
            return searchCacheSymbol;
        }

        uint errors = global.errors;

        insearch = true;
        Dsymbol s = ScopeDsymbol.search(loc, ident, flags);
        insearch = false;

        if (errors == global.errors)
        {
            // https://issues.dlang.org/show_bug.cgi?id=10752
            // Can cache the result only when it does not cause
            // access error so the side-effect should be reproduced in later search.
            searchCacheIdent = ident;
            searchCacheSymbol = s;
            searchCacheFlags = flags;
        }
        return s;
    }

    override bool isPackageAccessible(Package p, Visibility visibility, int flags = 0)
    {
        if (insearch) // don't follow import cycles
            return false;
        insearch = true;
        scope (exit)
            insearch = false;
        if (flags & IgnorePrivateImports)
            visibility = Visibility(Visibility.Kind.public_); // only consider public imports
        return super.isPackageAccessible(p, visibility);
    }

    override Dsymbol symtabInsert(Dsymbol s)
    {
        searchCacheIdent = null; // symbol is inserted, so invalidate cache
        return Package.symtabInsert(s);
    }

    void deleteObjFile()
    {
        if (global.params.obj)
            File.remove(objfile.toChars());
        if (docfile)
            File.remove(docfile.toChars());
    }

    /*******************************************
     * Can't run semantic on s now, try again later.
     */
    extern (D) static void addDeferredSemantic(Dsymbol s)
    {
        //printf("Module::addDeferredSemantic('%s')\n", s.toChars());
        if (!deferred.contains(s))
            deferred.push(s);
    }

    extern (D) static void addDeferredSemantic2(Dsymbol s)
    {
        //printf("Module::addDeferredSemantic2('%s')\n", s.toChars());
        if (!deferred2.contains(s))
            deferred2.push(s);
    }

    extern (D) static void addDeferredSemantic3(Dsymbol s)
    {
        //printf("Module::addDeferredSemantic3('%s')\n", s.toChars());
        if (!deferred.contains(s))
            deferred3.push(s);
    }

    /******************************************
     * Run semantic() on deferred symbols.
     */
    static void runDeferredSemantic()
    {
        __gshared int nested;
        if (nested)
            return;
        //if (deferred.length) printf("+Module::runDeferredSemantic(), len = %ld\n", deferred.length);
        nested++;

        size_t len;
        do
        {
            len = deferred.length;
            if (!len)
                break;

            Dsymbol* todo;
            Dsymbol* todoalloc = null;
            Dsymbol tmp;
            if (len == 1)
            {
                todo = &tmp;
            }
            else
            {
                todo = cast(Dsymbol*)Mem.check(malloc(len * Dsymbol.sizeof));
                todoalloc = todo;
            }
            memcpy(todo, deferred.tdata(), len * Dsymbol.sizeof);
            deferred.setDim(0);

            foreach (i; 0..len)
            {
                Dsymbol s = todo[i];
                s.dsymbolSemantic(null);
                //printf("deferred: %s, parent = %s\n", s.toChars(), s.parent.toChars());
            }
            //printf("\tdeferred.length = %ld, len = %ld\n", deferred.length, len);
            if (todoalloc)
                free(todoalloc);
        }
        while (deferred.length != len); // while making progress
        nested--;
        //printf("-Module::runDeferredSemantic(), len = %ld\n", deferred.length);
    }

    static void runDeferredSemantic2()
    {
        Module.runDeferredSemantic();

        Dsymbols* a = &Module.deferred2;
        for (size_t i = 0; i < a.length; i++)
        {
            Dsymbol s = (*a)[i];
            //printf("[%d] %s semantic2a\n", i, s.toPrettyChars());
            s.semantic2(null);

            if (global.errors)
                break;
        }
        a.setDim(0);
    }

    static void runDeferredSemantic3()
    {
        Module.runDeferredSemantic2();

        Dsymbols* a = &Module.deferred3;
        for (size_t i = 0; i < a.length; i++)
        {
            Dsymbol s = (*a)[i];
            //printf("[%d] %s semantic3a\n", i, s.toPrettyChars());
            s.semantic3(null);

            if (global.errors)
                break;
        }
        a.setDim(0);
    }

    extern (D) static void clearCache() nothrow
    {
        foreach (Module m; amodules)
            m.searchCacheIdent = null;
    }

    /************************************
     * Recursively look at every module this module imports,
     * return true if it imports m.
     * Can be used to detect circular imports.
     */
    int imports(Module m) nothrow
    {
        //printf("%s Module::imports(%s)\n", toChars(), m.toChars());
        version (none)
        {
            foreach (i, Module mi; aimports)
                printf("\t[%d] %s\n", cast(int) i, mi.toChars());
        }
        foreach (Module mi; aimports)
        {
            if (mi == m)
                return true;
            if (!mi.insearch)
            {
                mi.insearch = true;
                int r = mi.imports(m);
                if (r)
                    return r;
            }
        }
        return false;
    }

    bool isRoot() nothrow
    {
        return this.importedFrom == this;
    }

    // true if the module source file is directly
    // listed in command line.
    bool isCoreModule(Identifier ident) nothrow
    {
        return this.ident == ident && parent && parent.ident == Id.core && !parent.parent;
    }

    // Back end
    int doppelganger; // sub-module
    Symbol* cov; // private uint[] __coverage;
    uint* covb; // bit array of valid code line numbers
    Symbol* sictor; // module order independent constructor
    Symbol* sctor; // module constructor
    Symbol* sdtor; // module destructor
    Symbol* ssharedctor; // module shared constructor
    Symbol* sshareddtor; // module shared destructor
    Symbol* stest; // module unit test
    Symbol* sfilename; // symbol for filename

    uint[uint] ctfe_cov; /// coverage information from ctfe execution_count[line]

    override inout(Module) isModule() inout nothrow
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    /***********************************************
     * Writes this module's fully-qualified name to buf
     * Params:
     *    buf = The buffer to write to
     */
    void fullyQualifiedName(ref OutBuffer buf) nothrow
    {
        buf.writestring(ident.toString());

        for (auto package_ = parent; package_ !is null; package_ = package_.parent)
        {
            buf.prependstring(".");
            buf.prependstring(package_.ident.toChars());
        }
    }

    /** Lazily initializes and returns the escape table.
    Turns out it eats a lot of memory.
    */
    extern(D) Escape* escapetable() nothrow
    {
        if (!_escapetable)
            _escapetable = new Escape();
        return _escapetable;
    }

    /****************************
     * A Singleton that loads core.atomic
     * Returns:
     *  Module of core.atomic, null if couldn't find it
     */
    extern (D) static Module loadCoreAtomic()
    {
        __gshared Module core_atomic;
        return loadModuleFromLibrary(core_atomic, Id.core, Id.atomic);
    }

    /****************************
     * A Singleton that loads std.math
     * Returns:
     *  Module of std.math, null if couldn't find it
     */
    extern (D) static Module loadStdMath()
    {
        __gshared Module std_math;
        return loadModuleFromLibrary(std_math, Id.std, Id.math);
    }

    /**********************************
     * Load a Module from the library.
     * Params:
     *  mod = cached return value of this call
     *  pkgid = package id
     *  modid = module id
     * Returns:
     *  Module loaded, null if cannot load it
     */
    private static Module loadModuleFromLibrary(ref Module mod, Identifier pkgid, Identifier modid)
    {
        if (mod)
            return mod;

        auto ids = new Identifier[1];
        ids[0] = pkgid;
        auto imp = new Import(Loc.initial, ids[], modid, null, true);
        // Module.load will call fatal() if there's no module available.
        // Gag the error here, pushing the error handling to the caller.
        const errors = global.startGagging();
        imp.load(null);
        if (imp.mod)
        {
            imp.mod.importAll(null);
            imp.mod.dsymbolSemantic(null);
        }
        global.endGagging(errors);
        mod = imp.mod;
        return mod;
    }
}

/***********************************************************
 */
extern (C++) struct ModuleDeclaration
{
    Loc loc;
    Identifier id;
    Identifier[] packages;  // array of Identifier's representing packages
    bool isdeprecated;      // if it is a deprecated module
    Expression msg;

    extern (D) this(const ref Loc loc, Identifier[] packages, Identifier id, Expression msg, bool isdeprecated)
    {
        this.loc = loc;
        this.packages = packages;
        this.id = id;
        this.msg = msg;
        this.isdeprecated = isdeprecated;
    }

    extern (C++) const(char)* toChars() const
    {
        OutBuffer buf;
        foreach (pid; packages)
        {
            buf.writestring(pid.toString());
            buf.writeByte('.');
        }
        buf.writestring(id.toString());
        return buf.extractChars();
    }

    /// Provide a human readable representation
    extern (D) const(char)[] toString() const
    {
        return this.toChars().toDString;
    }
}

/****************************************
 * Create array of the local classes in the Module, suitable
 * for inclusion in ModuleInfo
 * Params:
 *      mod = the Module
 *      aclasses = array to fill in
 * Returns: array of local classes
 */
extern (C++) void getLocalClasses(Module mod, ref ClassDeclarations aclasses)
{
    //printf("members.length = %d\n", mod.members.length);
    int pushAddClassDg(size_t n, Dsymbol sm)
    {
        if (!sm)
            return 0;

        if (auto cd = sm.isClassDeclaration())
        {
            // compatibility with previous algorithm
            if (cd.parent && cd.parent.isTemplateMixin())
                return 0;

            if (cd.classKind != ClassKind.objc)
                aclasses.push(cd);
        }
        return 0;
    }

    ScopeDsymbol._foreach(null, mod.members, &pushAddClassDg);
}

/**
 * Process the content of a source file
 *
 * Attempts to find which encoding it is using, if it has BOM,
 * and then normalize the source to UTF-8. If no encoding is required,
 * a slice of `src` will be returned without extra allocation.
 *
 * Params:
 *  src = Content of the source file to process
 *  mod = Module matching `src`, used for error handling
 *
 * Returns:
 *   UTF-8 encoded variant of `src`, stripped of any BOM,
 *   or `null` if an error happened.
 */
private const(char)[] processSource (const(ubyte)[] src, Module mod)
{
    enum SourceEncoding { utf16, utf32}
    enum Endian { little, big}

    /*
     * Convert a buffer from UTF32 to UTF8
     * Params:
     *    Endian = is the buffer big/little endian
     *    buf = buffer of UTF32 data
     * Returns:
     *    input buffer reencoded as UTF8
     */

    char[] UTF32ToUTF8(Endian endian)(const(char)[] buf)
    {
        static if (endian == Endian.little)
            alias readNext = Port.readlongLE;
        else
            alias readNext = Port.readlongBE;

        if (buf.length & 3)
        {
            mod.error("odd length of UTF-32 char source %llu", cast(ulong) buf.length);
            return null;
        }

        const (uint)[] eBuf = cast(const(uint)[])buf;

        OutBuffer dbuf;
        dbuf.reserve(eBuf.length);

        foreach (i; 0 .. eBuf.length)
        {
            const u = readNext(&eBuf[i]);
            if (u & ~0x7F)
            {
                if (u > 0x10FFFF)
                {
                    mod.error("UTF-32 value %08x greater than 0x10FFFF", u);
                    return null;
                }
                dbuf.writeUTF8(u);
            }
            else
                dbuf.writeByte(u);
        }
        dbuf.writeByte(0); //add null terminator
        return dbuf.extractSlice();
    }

    /*
     * Convert a buffer from UTF16 to UTF8
     * Params:
     *    Endian = is the buffer big/little endian
     *    buf = buffer of UTF16 data
     * Returns:
     *    input buffer reencoded as UTF8
     */

    char[] UTF16ToUTF8(Endian endian)(const(char)[] buf)
    {
        static if (endian == Endian.little)
            alias readNext = Port.readwordLE;
        else
            alias readNext = Port.readwordBE;

        if (buf.length & 1)
        {
            mod.error("odd length of UTF-16 char source %llu", cast(ulong) buf.length);
            return null;
        }

        const (ushort)[] eBuf = cast(const(ushort)[])buf;

        OutBuffer dbuf;
        dbuf.reserve(eBuf.length);

        //i will be incremented in the loop for high codepoints
        foreach (ref i; 0 .. eBuf.length)
        {
            uint u = readNext(&eBuf[i]);
            if (u & ~0x7F)
            {
                if (0xD800 <= u && u < 0xDC00)
                {
                    i++;
                    if (i >= eBuf.length)
                    {
                        mod.error("surrogate UTF-16 high value %04x at end of file", u);
                        return null;
                    }
                    const u2 = readNext(&eBuf[i]);
                    if (u2 < 0xDC00 || 0xE000 <= u2)
                    {
                        mod.error("surrogate UTF-16 low value %04x out of range", u2);
                        return null;
                    }
                    u = (u - 0xD7C0) << 10;
                    u |= (u2 - 0xDC00);
                }
                else if (u >= 0xDC00 && u <= 0xDFFF)
                {
                    mod.error("unpaired surrogate UTF-16 value %04x", u);
                    return null;
                }
                else if (u == 0xFFFE || u == 0xFFFF)
                {
                    mod.error("illegal UTF-16 value %04x", u);
                    return null;
                }
                dbuf.writeUTF8(u);
            }
            else
                dbuf.writeByte(u);
        }
        dbuf.writeByte(0); //add a terminating null byte
        return dbuf.extractSlice();
    }

    const(char)[] buf = cast(const(char)[]) src;

    // Assume the buffer is from memory and has not be read from disk. Assume UTF-8.
    if (buf.length < 2)
        return buf;

    /* Convert all non-UTF-8 formats to UTF-8.
     * BOM : https://www.unicode.org/faq/utf_bom.html
     * 00 00 FE FF  UTF-32BE, big-endian
     * FF FE 00 00  UTF-32LE, little-endian
     * FE FF        UTF-16BE, big-endian
     * FF FE        UTF-16LE, little-endian
     * EF BB BF     UTF-8
     */
    if (buf[0] == 0xFF && buf[1] == 0xFE)
    {
        if (buf.length >= 4 && buf[2] == 0 && buf[3] == 0)
            return UTF32ToUTF8!(Endian.little)(buf[4 .. $]);
        return UTF16ToUTF8!(Endian.little)(buf[2 .. $]);
    }

    if (buf[0] == 0xFE && buf[1] == 0xFF)
        return UTF16ToUTF8!(Endian.big)(buf[2 .. $]);

    if (buf.length >= 4 && buf[0] == 0 && buf[1] == 0 && buf[2] == 0xFE && buf[3] == 0xFF)
        return UTF32ToUTF8!(Endian.big)(buf[4 .. $]);

    if (buf.length >= 3 && buf[0] == 0xEF && buf[1] == 0xBB && buf[2] == 0xBF)
        return buf[3 .. $];

    /* There is no BOM. Make use of Arcane Jill's insight that
     * the first char of D source must be ASCII to
     * figure out the encoding.
     */
    if (buf.length >= 4 && buf[1] == 0 && buf[2] == 0 && buf[3] == 0)
        return UTF32ToUTF8!(Endian.little)(buf);
    if (buf.length >= 4 && buf[0] == 0 && buf[1] == 0 && buf[2] == 0)
        return UTF32ToUTF8!(Endian.big)(buf);
    // try to check for UTF-16
    if (buf.length >= 2 && buf[1] == 0)
        return UTF16ToUTF8!(Endian.little)(buf);
    if (buf[0] == 0)
        return UTF16ToUTF8!(Endian.big)(buf);

    // It's UTF-8
    if (buf[0] >= 0x80)
    {
        mod.error("source file must start with BOM or ASCII character, not \\x%02X", buf[0]);
        return null;
    }

    return buf;
}
