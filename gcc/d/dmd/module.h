
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
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

enum class Edition : unsigned char
{
    none = 0u,
    legacy = 1u,
    v2024 = 2u,
    latest = 2u,
};

class Package : public ScopeDsymbol
{
public:
    PKG isPkgMod;
    unsigned tag;       // auto incremented tag, used to mask package tree in scopes
    Module *mod;        // != NULL if isPkgMod == PKGmodule

    const char *kind() const override;

    bool equals(const RootObject * const o) const override;

    bool isAncestorPackageOf(const Package * const pkg) const;

    void accept(Visitor *v) override { v->visit(this); }

    Module *isPackageMod();
};

class Module final : public Package
{
public:
    static Module *rootModule;
    static DsymbolTable *modules;       // symbol table of all modules
    static Modules amodules;            // array of all modules
    static Dsymbols deferred;   // deferred Dsymbol's needing semantic() run on them
    static Dsymbols deferred2;  // deferred Dsymbol's needing semantic2() run on them
    static Dsymbols deferred3;  // deferred Dsymbol's needing semantic3() run on them

    static void _init();

    static AggregateDeclaration *moduleinfo;


    DString arg;        // original argument name
    ModuleDeclaration *md; // if !NULL, the contents of the ModuleDeclaration declaration
    FileName srcfile;   // input source file
    FileName objfile;   // output .obj file
    FileName hdrfile;   // 'header' file
    FileName docfile;   // output documentation file
    DArray<unsigned char> src; // Raw content of the file
    unsigned errors;    // if any errors in file
    unsigned numlines;  // number of lines in source file
    FileType filetype;  // source file type
    d_bool hasAlwaysInlines; // contains references to functions that must be inlined
    d_bool isPackageFile; // if it is a package.d
    Edition edition;    // language edition that this module is compiled with
    Package *pkg;       // if isPackageFile is true, the Package that contains this package.d
    Strings contentImportedFiles;  // array of files whose content was imported
    int needmoduleinfo;
    ThreeState selfimports;
    ThreeState rootimports;
    void* tagSymTab;            // ImportC: tag symbols that conflict with other symbols used as the index
    OutBuffer defines;          // collect all the #define lines here
    bool selfImports();         // returns true if module imports itself

    bool rootImports();         // returns true if module imports root module

    Identifier *searchCacheIdent;
    Dsymbol *searchCacheSymbol; // cached value of search
    SearchOptFlags searchCacheFlags;       // cached flags
    d_bool insearch;

    // module from command line we're imported from,
    // i.e. a module that will be taken all the
    // way to an object file
    Module *importedFrom;

    Dsymbols *decldefs;         // top level declarations for this Module

    Modules aimports;             // all imported modules

    Identifiers *debugids;      // debug identifiers
    Identifiers *debugidsNot;   // forward referenced debug identifiers

    Identifiers *versionids;    // version identifiers
    Identifiers *versionidsNot; // forward referenced version identifiers

    MacroTable macrotable;      // document comment macros
    Escape *escapetable;        // document comment escapes

    size_t nameoffset;          // offset of module name from start of ModuleInfo
    size_t namelen;             // length of module name in characters

    static Module* create(const char *arg, Identifier *ident, int doDocComment, int doHdrGen);
    static const char *find(const char *filename);
    static Module *load(Loc loc, Identifiers *packages, Identifier *ident);

    const char *kind() const override;
    bool read(Loc loc); // read file, returns 'true' if succeed, 'false' otherwise.
    Module *parse();    // syntactic parse
    int needModuleInfo();
    bool isPackageAccessible(Package *p, Visibility visibility, SearchOptFlags flags = (SearchOptFlags)SearchOpt::all) override;
    Dsymbol *symtabInsert(Dsymbol *s) override;
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
    DArray<unsigned> covb;      // bit array of valid code line numbers

    Symbol *sictor;             // module order independent constructor
    Symbol *sctor;              // module constructor
    Symbol *sdtor;              // module destructor
    Symbol *ssharedctor;        // module shared constructor
    Symbol *sshareddtor;        // module shared destructor
    Symbol *stest;              // module unit test

    Symbol *sfilename;          // symbol for filename

    void *ctfe_cov;             // stores coverage information from ctfe

    void accept(Visitor *v) override { v->visit(this); }
};


struct ModuleDeclaration
{
    Loc loc;
    Identifier *id;
    DArray<Identifier*> packages;  // array of Identifier's representing packages
    d_bool isdeprecated;  // if it is a deprecated module
    Expression *msg;

    const char *toChars() const;
};

namespace dmd
{
    void getLocalClasses(Module* mod, Array<ClassDeclaration* >& aclasses);
    FuncDeclaration *findGetMembers(ScopeDsymbol *dsym);
}
