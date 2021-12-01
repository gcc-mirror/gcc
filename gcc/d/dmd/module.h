
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/module.h
 */

#pragma once

#include "dsymbol.h"

struct ModuleDeclaration;
struct Escape;
struct FileBuffer;

struct MacroTable
{
    void* internal;  // PIMPL
};

enum PKG
{
    PKGunknown, // not yet determined whether it's a package.d or not
    PKGmodule,  // already determined that's an actual package.d
    PKGpackage  // already determined that's an actual package
};

class Package : public ScopeDsymbol
{
public:
    PKG isPkgMod;
    unsigned tag;       // auto incremented tag, used to mask package tree in scopes
    Module *mod;        // != NULL if isPkgMod == PKGmodule

    const char *kind() const;

    bool equals(const RootObject *o) const;

    Package *isPackage() { return this; }

    bool isAncestorPackageOf(const Package * const pkg) const;

    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);
    void accept(Visitor *v) { v->visit(this); }

    Module *isPackageMod();
};

class Module : public Package
{
public:
    static Module *rootModule;
    static DsymbolTable *modules;       // symbol table of all modules
    static Modules amodules;            // array of all modules
    static Dsymbols deferred;   // deferred Dsymbol's needing semantic() run on them
    static Dsymbols deferred2;  // deferred Dsymbol's needing semantic2() run on them
    static Dsymbols deferred3;  // deferred Dsymbol's needing semantic3() run on them
    static unsigned dprogress;  // progress resolving the deferred list

    static void _init();

    static AggregateDeclaration *moduleinfo;


    DString arg;        // original argument name
    ModuleDeclaration *md; // if !NULL, the contents of the ModuleDeclaration declaration
    FileName srcfile;   // input source file
    FileName objfile;   // output .obj file
    FileName hdrfile;   // 'header' file
    FileName docfile;   // output documentation file
    FileBuffer *srcBuffer; // set during load(), free'd in parse()
    unsigned errors;    // if any errors in file
    unsigned numlines;  // number of lines in source file
    bool isHdrFile;     // if it is a header (.di) file
    bool isCFile;       // if it is a C (.c) file
    bool isDocFile;     // if it is a documentation input file, not D source
    bool hasAlwaysInlines; // contains references to functions that must be inlined
    bool isPackageFile; // if it is a package.d
    Package *pkg;       // if isPackageFile is true, the Package that contains this package.d
    Strings contentImportedFiles;  // array of files whose content was imported
    int needmoduleinfo;
    int selfimports;            // 0: don't know, 1: does not, 2: does
    void* tagSymTab;            // ImportC: tag symbols that conflict with other symbols used as the index
    bool selfImports();         // returns true if module imports itself

    int rootimports;            // 0: don't know, 1: does not, 2: does
    bool rootImports();         // returns true if module imports root module

    int insearch;
    Identifier *searchCacheIdent;
    Dsymbol *searchCacheSymbol; // cached value of search
    int searchCacheFlags;       // cached flags

    // module from command line we're imported from,
    // i.e. a module that will be taken all the
    // way to an object file
    Module *importedFrom;

    Dsymbols *decldefs;         // top level declarations for this Module

    Modules aimports;             // all imported modules

    unsigned debuglevel;        // debug level
    Identifiers *debugids;      // debug identifiers
    Identifiers *debugidsNot;   // forward referenced debug identifiers

    unsigned versionlevel;      // version level
    Identifiers *versionids;    // version identifiers
    Identifiers *versionidsNot; // forward referenced version identifiers

    MacroTable macrotable;      // document comment macros
    Escape *escapetable;        // document comment escapes

    size_t nameoffset;          // offset of module name from start of ModuleInfo
    size_t namelen;             // length of module name in characters

    static Module* create(const char *arg, Identifier *ident, int doDocComment, int doHdrGen);

    static Module *load(Loc loc, Identifiers *packages, Identifier *ident);

    const char *kind() const;
    bool read(const Loc &loc); // read file, returns 'true' if succeed, 'false' otherwise.
    Module *parse();    // syntactic parse
    void importAll(Scope *sc);
    int needModuleInfo();
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);
    bool isPackageAccessible(Package *p, Visibility visibility, int flags = 0);
    Dsymbol *symtabInsert(Dsymbol *s);
    void deleteObjFile();
    static void runDeferredSemantic();
    static void runDeferredSemantic2();
    static void runDeferredSemantic3();
    int imports(Module *m);

    bool isRoot() { return this->importedFrom == this; }
    // true if the module source file is directly
    // listed in command line.
    bool isCoreModule(Identifier *ident);

    // Back end

    int doppelganger;           // sub-module
    Symbol *cov;                // private uint[] __coverage;
    unsigned *covb;             // bit array of valid code line numbers

    Symbol *sictor;             // module order independent constructor
    Symbol *sctor;              // module constructor
    Symbol *sdtor;              // module destructor
    Symbol *ssharedctor;        // module shared constructor
    Symbol *sshareddtor;        // module shared destructor
    Symbol *stest;              // module unit test

    Symbol *sfilename;          // symbol for filename

    void *ctfe_cov;             // stores coverage information from ctfe

    Module *isModule() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};


struct ModuleDeclaration
{
    Loc loc;
    Identifier *id;
    DArray<Identifier*> packages;  // array of Identifier's representing packages
    bool isdeprecated;  // if it is a deprecated module
    Expression *msg;

    const char *toChars() const;
};
