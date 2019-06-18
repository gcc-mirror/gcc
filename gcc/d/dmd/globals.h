
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/globals.h
 */

#pragma once

#include "root/dcompat.h"
#include "root/ctfloat.h"
#include "root/outbuffer.h"
#include "root/filename.h"
#include "compiler.h"

// Can't include arraytypes.h here, need to declare these directly.
template <typename TYPE> struct Array;

typedef unsigned char Diagnostic;
enum
{
    DIAGNOSTICerror,  // generate an error
    DIAGNOSTICinform, // generate a warning
    DIAGNOSTICoff     // disable diagnostic
};

typedef unsigned char MessageStyle;
enum
{
    MESSAGESTYLEdigitalmars, // file(line,column): message
    MESSAGESTYLEgnu          // file:line:column: message
};

// The state of array bounds checking
typedef unsigned char CHECKENABLE;
enum
{
    CHECKENABLEdefault, // initial value
    CHECKENABLEoff,     // never do bounds checking
    CHECKENABLEon,      // always do bounds checking
    CHECKENABLEsafeonly // do bounds checking only in @safe functions
};

typedef unsigned char CHECKACTION;
enum
{
    CHECKACTION_D,        // call D assert on failure
    CHECKACTION_C,        // call C assert on failure
    CHECKACTION_halt,     // cause program halt on failure
    CHECKACTION_context   // call D assert with the error context on failure
};

enum JsonFieldFlags
{
    none         = 0,
    compilerInfo = (1 << 0),
    buildInfo    = (1 << 1),
    modules      = (1 << 2),
    semantics    = (1 << 3)
};

enum CppStdRevision
{
    CppStdRevisionCpp98 = 199711,
    CppStdRevisionCpp11 = 201103,
    CppStdRevisionCpp14 = 201402,
    CppStdRevisionCpp17 = 201703,
    CppStdRevisionCpp20 = 202002
};

/// Configuration for the C++ header generator
enum class CxxHeaderMode
{
    none,   /// Don't generate headers
    silent, /// Generate headers
    verbose /// Generate headers and add comments for hidden declarations
};

/// Trivalent boolean to represent the state of a `revert`able change
enum class FeatureState : signed char
{
    default_ = -1, /// Not specified by the user
    disabled = 0,  /// Specified as `-revert=`
    enabled = 1    /// Specified as `-preview=`
};

// Put command line switches in here
struct Param
{
    bool obj;           // write object file
    bool link;          // perform link
    bool dll;           // generate shared dynamic library
    bool lib;           // write library file instead of object file(s)
    bool multiobj;      // break one object file into multiple ones
    bool oneobj;        // write one object file instead of multiple ones
    bool trace;         // insert profiling hooks
    bool tracegc;       // instrument calls to 'new'
    bool verbose;       // verbose compile
    bool vcg_ast;       // write-out codegen-ast
    bool showColumns;   // print character (column) numbers in diagnostics
    bool vtls;          // identify thread local variables
    bool vtemplates;    // collect and list statistics on template instantiations
    bool vtemplatesListInstances; // collect and list statistics on template instantiations origins
    bool vgc;           // identify gc usage
    bool vfield;        // identify non-mutable field variables
    bool vcomplex;      // identify complex/imaginary type usage
    unsigned char symdebug;  // insert debug symbolic information
    bool symdebugref;   // insert debug information for all referenced types, too
    bool optimize;      // run optimizer
    Diagnostic useDeprecated;
    bool stackstomp;    // add stack stomping code
    bool useUnitTests;  // generate unittest code
    bool useInline;     // inline expand functions
    FeatureState useDIP25;      // implement http://wiki.dlang.org/DIP25
    FeatureState useDIP1000; // implement https://dlang.org/spec/memory-safe-d.html#scope-return-params
    bool useDIP1021;    // implement https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1021.md
    bool release;       // build release version
    bool preservePaths; // true means don't strip path from source file
    Diagnostic warnings;
    unsigned char pic;  // generate position-independent-code for shared libs
    bool color;         // use ANSI colors in console output
    bool cov;           // generate code coverage data
    unsigned char covPercent;   // 0..100 code coverage percentage required
    bool ctfe_cov;      // generate coverage data for ctfe
    bool nofloat;       // code should not pull in floating point support
    bool ignoreUnsupportedPragmas;      // rather than error on them
    bool useModuleInfo; // generate runtime module information
    bool useTypeInfo;   // generate runtime type information
    bool useExceptions; // support exception handling
    bool noSharedAccess; // read/write access to shared memory objects
    bool previewIn;     // `in` means `scope const`, perhaps `ref`, accepts rvalues
    bool shortenedMethods; // allow => in normal function declarations
    bool betterC;       // be a "better C" compiler; no dependency on D runtime
    bool addMain;       // add a default main() function
    bool allInst;       // generate code for all template instantiations
    bool fix16997;      // fix integral promotions for unary + - ~ operators
                        // https://issues.dlang.org/show_bug.cgi?id=16997
    bool fixAliasThis;  // if the current scope has an alias this, check it before searching upper scopes
    bool inclusiveInContracts;   // 'in' contracts of overridden methods must be a superset of parent contract
    bool ehnogc;        // use @nogc exception handling
    FeatureState dtorFields;  // destruct fields of partially constructed objects
                              // https://issues.dlang.org/show_bug.cgi?id=14246
    bool fieldwise;         // do struct equality testing field-wise rather than by memcmp()
    bool rvalueRefParam;    // allow rvalues to be arguments to ref parameters
    CppStdRevision cplusplus;  // version of C++ name mangling to support
    bool markdown;          // enable Markdown replacements in Ddoc
    bool vmarkdown;         // list instances of Markdown replacements in Ddoc
    bool showGaggedErrors;  // print gagged errors anyway
    bool printErrorContext;  // print errors with the error context (the error line in the source file)
    bool manual;            // open browser on compiler manual
    bool usage;             // print usage and exit
    bool mcpuUsage;         // print help on -mcpu switch
    bool transitionUsage;   // print help on -transition switch
    bool checkUsage;        // print help on -check switch
    bool checkActionUsage;  // print help on -checkaction switch
    bool revertUsage;       // print help on -revert switch
    bool previewUsage;      // print help on -preview switch
    bool externStdUsage;    // print help on -extern-std switch
    bool hcUsage;           // print help on -HC switch
    bool logo;              // print logo;

    CHECKENABLE useInvariants;     // generate class invariant checks
    CHECKENABLE useIn;             // generate precondition checks
    CHECKENABLE useOut;            // generate postcondition checks
    CHECKENABLE useArrayBounds;    // when to generate code for array bounds checks
    CHECKENABLE useAssert;         // when to generate code for assert()'s
    CHECKENABLE useSwitchError;    // check for switches without a default
    CHECKENABLE boundscheck;       // state of -boundscheck switch

    CHECKACTION checkAction;       // action to take when bounds, asserts or switch defaults are violated

    unsigned errorLimit;

    DString  argv0;    // program name
    Array<const char *> modFileAliasStrings; // array of char*'s of -I module filename alias strings
    Array<const char *> *imppath;     // array of char*'s of where to look for import modules
    Array<const char *> *fileImppath; // array of char*'s of where to look for file import modules
    DString objdir;    // .obj/.lib file output directory
    DString objname;   // .obj file output name
    DString libname;   // .lib file output name

    bool doDocComments;  // process embedded documentation comments
    DString docdir;      // write documentation file to docdir directory
    DString docname;     // write documentation file to docname
    Array<const char *> ddocfiles;  // macro include files for Ddoc

    bool doHdrGeneration;  // process embedded documentation comments
    DString hdrdir;        // write 'header' file to docdir directory
    DString hdrname;       // write 'header' file to docname
    bool hdrStripPlainFunctions; // strip the bodies of plain (non-template) functions

    CxxHeaderMode doCxxHdrGeneration;  // write 'Cxx header' file
    DString cxxhdrdir;        // write 'header' file to docdir directory
    DString cxxhdrname;       // write 'header' file to docname

    bool doJsonGeneration;    // write JSON file
    DString jsonfilename;     // write JSON file to jsonfilename
    unsigned jsonFieldFlags;  // JSON field flags to include

    OutBuffer *mixinOut;                // write expanded mixins for debugging
    const char *mixinFile;             // .mixin file output name
    int mixinLines;                     // Number of lines in writeMixins

    unsigned debuglevel;   // debug level
    Array<const char *> *debugids;     // debug identifiers

    unsigned versionlevel; // version level
    Array<const char *> *versionids;   // version identifiers

    DString defaultlibname;     // default library for non-debug builds
    DString debuglibname;       // default library for debug builds
    DString mscrtlib;           // MS C runtime library

    DString moduleDepsFile;     // filename for deps output
    OutBuffer *moduleDeps;      // contents to be written to deps file

    bool emitMakeDeps;                // whether to emit makedeps
    DString makeDepsFile;             // filename for makedeps output
    Array<const char *> makeDeps;     // dependencies for makedeps

    MessageStyle messageStyle;  // style of file/line annotations on messages

    bool run;           // run resulting executable
    Strings runargs;    // arguments for executable

    // Linker stuff
    Array<const char *> objfiles;
    Array<const char *> linkswitches;
    Array<bool> linkswitchIsForCC;
    Array<const char *> libfiles;
    Array<const char *> dllfiles;
    DString deffile;
    DString resfile;
    DString exefile;
    DString mapfile;
};

typedef unsigned structalign_t;
// magic value means "match whatever the underlying C compiler does"
// other values are all powers of 2
#define STRUCTALIGN_DEFAULT ((structalign_t) ~0)

const DString mars_ext = "d";
const DString doc_ext  = "html";     // for Ddoc generated files
const DString ddoc_ext = "ddoc";     // for Ddoc macro include files
const DString dd_ext   = "dd";       // for Ddoc source files
const DString hdr_ext  = "di";       // for D 'header' import files
const DString json_ext = "json";     // for JSON files
const DString map_ext  = "map";      // for .map files

struct Global
{
    DString inifilename;

    const DString copyright;
    const DString written;
    Array<const char *> *path;        // Array of char*'s which form the import lookup path
    Array<const char *> *filePath;    // Array of char*'s which form the file import lookup path

    DString vendor;          // Compiler backend name

    Param params;
    unsigned errors;         // number of errors reported so far
    unsigned warnings;       // number of warnings reported so far
    unsigned gag;            // !=0 means gag reporting of errors & warnings
    unsigned gaggedErrors;   // number of errors reported while gagged
    unsigned gaggedWarnings; // number of warnings reported while gagged

    void* console;         // opaque pointer to console for controlling text attributes

    Array<class Identifier*>* versionids; // command line versions and predefined versions
    Array<class Identifier*>* debugids;   // command line debug versions and predefined versions

    /* Start gagging. Return the current number of gagged errors
     */
    unsigned startGagging();

    /* End gagging, restoring the old gagged state.
     * Return true if errors occurred while gagged.
     */
    bool endGagging(unsigned oldGagged);

    /*  Increment the error count to record that an error
     *  has occurred in the current context. An error message
     *  may or may not have been printed.
     */
    void increaseErrorCount();

    void _init();

    /**
    Returns: the version as the number that would be returned for __VERSION__
    */
    unsigned versionNumber();

    /**
    Returns: the compiler version string.
    */
    const char * versionChars();
};

extern Global global;

// Because int64_t and friends may be any integral type of the correct size,
// we have to explicitly ask for the correct integer type to get the correct
// mangling with dmd. The #if logic here should match the mangling of
// Tint64 and Tuns64 in cppmangle.d.
#if MARS && DMD_VERSION >= 2079 && DMD_VERSION <= 2081 && \
    __APPLE__ && __SIZEOF_LONG__ == 8
// DMD versions between 2.079 and 2.081 mapped D long to int64_t on OS X.
typedef uint64_t dinteger_t;
typedef int64_t sinteger_t;
typedef uint64_t uinteger_t;
#elif __SIZEOF_LONG__ == 8
// Be careful not to care about sign when using dinteger_t
// use this instead of integer_t to
// avoid conflicts with system #include's
typedef unsigned long dinteger_t;
// Signed and unsigned variants
typedef long sinteger_t;
typedef unsigned long uinteger_t;
#else
typedef unsigned long long dinteger_t;
typedef long long sinteger_t;
typedef unsigned long long uinteger_t;
#endif

typedef int8_t                  d_int8;
typedef uint8_t                 d_uns8;
typedef int16_t                 d_int16;
typedef uint16_t                d_uns16;
typedef int32_t                 d_int32;
typedef uint32_t                d_uns32;
typedef int64_t                 d_int64;
typedef uint64_t                d_uns64;

// file location
struct Loc
{
    const char *filename; // either absolute or relative to cwd
    unsigned linnum;
    unsigned charnum;

    Loc()
    {
        linnum = 0;
        charnum = 0;
        filename = NULL;
    }

    Loc(const char *filename, unsigned linnum, unsigned charnum)
    {
        this->linnum = linnum;
        this->charnum = charnum;
        this->filename = filename;
    }

    const char *toChars(
        bool showColumns = global.params.showColumns,
        MessageStyle messageStyle = global.params.messageStyle) const;
    bool equals(const Loc& loc) const;
};

enum class LINK : uint8_t
{
    default_,
    d,
    c,
    cpp,
    windows,
    objc,
    system
};

enum class CPPMANGLE : uint8_t
{
    def,
    asStruct,
    asClass
};

enum class MATCH : int
{
    nomatch,       // no match
    convert,       // match with conversions
    constant,      // match with conversion to const
    exact          // exact match
};

enum class PINLINE : uint8_t
{
    default_,     // as specified on the command line
    never,        // never inline
    always        // always inline
};

typedef uinteger_t StorageClass;
