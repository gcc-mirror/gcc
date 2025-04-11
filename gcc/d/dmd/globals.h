
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/globals.h
 */

#pragma once

#include "root/dcompat.h"
#include "root/ctfloat.h"
#include "common/outbuffer.h"
#include "common/charactertables.h"
#include "root/filename.h"
#include "compiler.h"

// Can't include arraytypes.h here, need to declare these directly.
template <typename TYPE> struct Array;

class ErrorSink;
class FileManager;
struct Loc;

typedef unsigned char Diagnostic;
enum
{
    DIAGNOSTICerror,  // generate an error
    DIAGNOSTICinform, // generate a warning
    DIAGNOSTICoff     // disable diagnostic
};

enum class MessageStyle : unsigned char
{
    digitalmars, // file(line,column): message
    gnu,         // file:line:column: message
    sarif        // JSON SARIF output, see https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html
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
    CppStdRevisionCpp20 = 202002,
    CppStdRevisionCpp23 = 202302,
};

/// Trivalent boolean to represent the state of a `revert`able change
enum class FeatureState : unsigned char
{
    default_ = 0,  /// Not specified by the user
    disabled = 1,  /// Specified as `-revert=`
    enabled  = 2,  /// Specified as `-preview=`
};

/// Different identifier tables specifiable by CLI
enum class CLIIdentifierTable : unsigned char
{
    default_ = 0, /// Not specified by user
    C99      = 1, /// Tables from C99 standard
    C11      = 2, /// Tables from C11 standard
    UAX31    = 3, /// Tables from the Unicode Standard Annex 31: UNICODE IDENTIFIERS AND SYNTAX
    All      = 4, /// The least restrictive set of all other tables
};

/// Specifies the mode for error printing
enum class ErrorPrintMode : unsigned char
{
    simpleError,        // Print errors without squiggles and carets
    printErrorContext,  // Print errors with the error line and caret
};

struct Output
{
    /// Configuration for the compiler generator
    d_bool doOutput;      // Output is enabled
    d_bool fullOutput;    // Generate comments for hidden declarations (for -HC),
                        // and don't strip the bodies of plain (non-template) functions (for -H)
    DString dir;   // write to directory 'dir'
    DString name;  // write to file 'name'
    Array<const char*> files; // Other files associated with this output,
                                // e.g. macro include files for Ddoc, dependencies for makedeps
    OutBuffer* buffer;  // if this output is buffered, this is the buffer
    int bufferLines;    // number of lines written to the buffer
};

/// Command line state related to printing uasage about other switches
struct Help
{
    d_bool manual;       // open browser on compiler manual
    d_bool usage;        // print usage and exit
    // print help of switch:
    d_bool mcpu;         // -mcpu
    d_bool transition;   // -transition
    d_bool check;        // -check
    d_bool checkAction;  // -checkaction
    d_bool revert;       // -revert
    d_bool preview;      // -preview
    d_bool externStd;    // -extern-std
    d_bool hc;           // -HC
};

struct Verbose
{
    d_bool verbose;           // verbose compile
    d_bool showColumns;       // print character (column) numbers in diagnostics
    d_bool tls;               // identify thread local variables
    d_bool templates;         // collect and list statistics on template instantiations
    // collect and list statistics on template instantiations origins.
    // TODO: make this an enum when we want to list other kinds of instances
    d_bool templatesListInstances;
    d_bool gc;                 // identify gc usage
    d_bool field;              // identify non-mutable field variables
    d_bool complex = true;     // identify complex/imaginary type usage
    d_bool vin;                // identify 'in' parameters
    d_bool showGaggedErrors;   // print gagged errors anyway
    d_bool logo;               // print compiler logo
    d_bool color;              // use ANSI colors in console output
    d_bool cov;                // generate code coverage data
    ErrorPrintMode errorPrintMode; // enum for error printing mode
    MessageStyle messageStyle; // style of file/line annotations on messages
    unsigned errorLimit;
    unsigned errorSupplementLimit; // Limit the number of supplemental messages for each error (0 means unlimited)
    unsigned errorSupplementCount();
};

struct ImportPathInfo
{
    const char* path;

    ImportPathInfo() : path(NULL) { }
    ImportPathInfo(const char* p) : path(p) { }
};

// Put command line switches in here
struct Param
{
    d_bool obj;           // write object file
    d_bool readStdin;     // read source file from stdin
    d_bool multiobj;      // break one object file into multiple ones
    d_bool trace;         // insert profiling hooks
    d_bool tracegc;       // instrument calls to 'new'
    d_bool vcg_ast;       // write-out codegen-ast
    Diagnostic useDeprecated;
    d_bool useUnitTests;  // generate unittest code
    d_bool useInline;     // inline expand functions
    d_bool release;       // build release version
    d_bool preservePaths; // true means don't strip path from source file
    Diagnostic useWarnings;
    d_bool cov;           // generate code coverage data
    unsigned char covPercent;   // 0..100 code coverage percentage required
    d_bool ctfe_cov;      // generate coverage data for ctfe
    d_bool ignoreUnsupportedPragmas;      // rather than error on them
    d_bool useModuleInfo; // generate runtime module information
    d_bool useTypeInfo;   // generate runtime type information
    d_bool useExceptions; // support exception handling
    d_bool useGC;         // support features that require the D runtime GC
    d_bool betterC;       // be a "better C" compiler; no dependency on D runtime
    d_bool addMain;       // add a default main() function
    d_bool allInst;       // generate code for all template instantiations
    d_bool bitfields;         // support C style bit fields
    CppStdRevision cplusplus;  // version of C++ name mangling to support

    Help help;
    Verbose v;

    // Options for `-preview=/-revert=`
    FeatureState useDIP25;       // implement https://wiki.dlang.org/DIP25
    FeatureState useDIP1000;     // implement https://dlang.org/spec/memory-safe-d.html#scope-return-params
    d_bool ehnogc;                 // use @nogc exception handling
    d_bool useDIP1021;             // implement https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1021.md
    FeatureState fieldwise;      // do struct equality testing field-wise rather than by memcmp()
    d_bool fixAliasThis;           // if the current scope has an alias this, check it before searching upper scopes
    FeatureState rvalueRefParam; // allow rvalues to be arguments to ref parameters
                                 // https://dconf.org/2019/talks/alexandrescu.html
                                 // https://gist.github.com/andralex/e5405a5d773f07f73196c05f8339435a
                                 // https://digitalmars.com/d/archives/digitalmars/D/Binding_rvalues_to_ref_parameters_redux_325087.html
                                 // Implementation: https://github.com/dlang/dmd/pull/9817
    FeatureState safer;          // safer by default (more @safe checks in unattributed code)
                                 // https://github.com/WalterBright/documents/blob/38f0a846726b571f8108f6e63e5e217b91421c86/safer.md

    FeatureState noSharedAccess; // read/write access to shared memory objects
    d_bool previewIn;              // `in` means `[ref] scope const`, accepts rvalues
    d_bool inclusiveInContracts;   // 'in' contracts of overridden methods must be a superset of parent contract
    d_bool shortenedMethods;       // allow => in normal function declarations
    d_bool fixImmutableConv;       // error on unsound immutable conversion - https://github.com/dlang/dmd/pull/14070
    d_bool fix16997;               // fix integral promotions for unary + - ~ operators
                                 // https://issues.dlang.org/show_bug.cgi?id=16997
    FeatureState dtorFields;     // destruct fields of partially constructed objects
                                 // https://issues.dlang.org/show_bug.cgi?id=14246
    FeatureState systemVariables; // limit access to variables marked @system from @safe code

    CHECKENABLE useInvariants;     // generate class invariant checks
    CHECKENABLE useIn;             // generate precondition checks
    CHECKENABLE useOut;            // generate postcondition checks
    CHECKENABLE useArrayBounds;    // when to generate code for array bounds checks
    CHECKENABLE useAssert;         // when to generate code for assert()'s
    CHECKENABLE useSwitchError;    // check for switches without a default
    CHECKENABLE boundscheck;       // state of -boundscheck switch

    CHECKACTION checkAction;       // action to take when bounds, asserts or switch defaults are violated

    CLIIdentifierTable dIdentifierTable;
    CLIIdentifierTable cIdentifierTable;

    DString  argv0;    // program name
    Array<const char *> modFileAliasStrings; // array of char*'s of -I module filename alias strings
    Array<ImportPathInfo> imppath;     // array of import path information of where to look for import modules
    Array<const char *> fileImppath; // array of char*'s of where to look for file import modules
    DString objdir;    // .obj/.lib file output directory
    DString objname;   // .obj file output name
    DString libname;   // .lib file output name

    Output ddoc;              // Generate embedded documentation comments
    Output dihdr;             // Generate `.di` 'header' files
    Output cxxhdr;            // Generate 'Cxx header' file
    Output json;              // Generate JSON file
    unsigned jsonFieldFlags;  // JSON field flags to include
    Output makeDeps;          // Generate make file dependencies
    Output mixinOut;          // write expanded mixins for debugging
    Output moduleDeps;        // Generate `.deps` module dependencies

    d_bool debugEnabled;   // -debug flag is passed

    d_bool run;           // run resulting executable
    Strings runargs;    // arguments for executable

    Array<const char *> cppswitches; // preprocessor switches
    const char *cpp;                 // if not null, then this specifies the C preprocessor

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
    bool fullyQualifiedObjectFiles;
    bool timeTrace;
    uint32_t timeTraceGranularityUs;
    const char* timeTraceFile;
};

struct structalign_t
{
    unsigned short value;
    d_bool pack;

    bool isDefault() const;
    void setDefault();
    bool isUnknown() const;
    void setUnknown();
    void set(unsigned value);
    unsigned get() const;
    bool isPack() const;
    void setPack(bool pack);
};

// magic value means "match whatever the underlying C compiler does"
// other values are all powers of 2
//#define STRUCTALIGN_DEFAULT ((structalign_t) ~0)

const DString mars_ext = "d";
const DString doc_ext  = "html";     // for Ddoc generated files
const DString ddoc_ext = "ddoc";     // for Ddoc macro include files
const DString dd_ext   = "dd";       // for Ddoc source files
const DString hdr_ext  = "di";       // for D 'header' import files
const DString json_ext = "json";     // for JSON files
const DString map_ext  = "map";      // for .map files

struct CompileEnv
{
    uint32_t versionNumber;
    DString date;
    DString time;
    DString vendor;
    DString timestamp;
    d_bool previewIn;
    d_bool transitionIn;
    d_bool ddocOutput;
    d_bool masm;
    IdentifierCharLookup cCharLookupTable;
    IdentifierCharLookup dCharLookupTable;
};

struct Global
{
    DString inifilename;

    const DString copyright;
    const DString written;
    Array<ImportPathInfo> path;        // Array of path informations which form the import lookup path
    Array<const char *> importPaths;   // Array of char*'s which form the import lookup path without metadata
    Array<const char *> filePath;      // Array of char*'s which form the file import lookup path

    char datetime[26];       /// string returned by ctime()
    CompileEnv compileEnv;

    Param params;
    unsigned errors;         // number of errors reported so far
    unsigned deprecations;   // number of deprecations reported so far
    unsigned warnings;       // number of warnings reported so far
    unsigned gag;            // !=0 means gag reporting of errors & warnings
    unsigned gaggedErrors;   // number of errors reported while gagged
    unsigned gaggedWarnings; // number of warnings reported while gagged

    void* console;         // opaque pointer to console for controlling text attributes

    Array<class Identifier*> versionids; // command line versions and predefined versions
    Array<class Identifier*> debugids;   // command line debug versions and predefined versions

    d_bool hasMainFunction;
    unsigned varSequenceNumber;

    FileManager* fileManager;
    ErrorSink* errorSink;       // where the error messages go
    ErrorSink* errorSinkNull;   // where the error messages disappear

    DArray<unsigned char> (*preprocess)(FileName, Loc, OutBuffer&);

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
     * Indicate to stateful error sinks that no more errors can be produced.
     * This is to support error sinks that collect information to produce a
     * single (say) report.
     */
    void plugErrorSinks();

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

// file location
struct SourceLoc
{
    DString filename;
    uint32_t line;
    uint32_t column;
    uint32_t fileOffset;
    DString fileContent;
};

struct Loc
{
private:

    unsigned int index;

#if MARS && defined(__linux__) && defined(__i386__)
    unsigned int dummy;
#endif

public:
    static void set(bool showColumns, MessageStyle messageStyle);
    static Loc singleFilename(const char* const filename);

    static bool showColumns;
    static MessageStyle messageStyle;

    Loc()
    {
        index = 0;
    }

    uint32_t charnum() const;
    uint32_t linnum() const;
    const char *filename() const;
    SourceLoc toSourceLoc() const;

    const char *toChars(
        bool showColumns = Loc::showColumns,
        MessageStyle messageStyle = Loc::messageStyle) const;
    bool equals(Loc loc) const;
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

enum class FileType : uint8_t
{
    d,    /// normal D source file
    dhdr, /// D header file (.di)
    ddoc, /// Ddoc documentation file (.dd)
    c,    /// C source file
};

typedef uinteger_t StorageClass;
