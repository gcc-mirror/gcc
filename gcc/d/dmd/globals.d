/**
 * Stores command line options and contains other miscellaneous declarations.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/globals.d, _globals.d)
 * Documentation:  https://dlang.org/phobos/dmd_globals.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/globals.d
 */

module dmd.globals;

import core.stdc.stdio;
import core.stdc.stdint;
import core.stdc.string;

import dmd.astenums;
import dmd.root.array;
import dmd.root.file;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.errorsink;
import dmd.errors;
import dmd.file_manager;
import dmd.identifier;
import dmd.location;
import dmd.lexer : CompileEnv;
import dmd.utils;

version (IN_GCC) {}
else version (IN_LLVM) {}
else version = MARS;

/// Defines a setting for how compiler warnings and deprecations are handled
enum DiagnosticReporting : ubyte
{
    error,        /// generate an error
    inform,       /// generate a warning
    off,          /// disable diagnostic
}

/**
Each flag represents a field that can be included in the JSON output.

NOTE: set type to uint so its size matches C++ unsigned type
*/
enum JsonFieldFlags : uint
{
    none         = 0,
    compilerInfo = (1 << 0),
    buildInfo    = (1 << 1),
    modules      = (1 << 2),
    semantics    = (1 << 3),
}

/// Version of C++ standard to support
enum CppStdRevision : uint
{
    cpp98 = 1997_11,
    cpp11 = 2011_03,
    cpp14 = 2014_02,
    cpp17 = 2017_03,
    cpp20 = 2020_02,
}

/// Trivalent boolean to represent the state of a `revert`able change
enum FeatureState : ubyte
{
    default_ = 0,  /// Not specified by the user
    disabled = 1,  /// Specified as `-revert=`
    enabled  = 2,  /// Specified as `-preview=`
}

/// Different identifier tables specifiable by CLI
enum CLIIdentifierTable : ubyte
{
    default_ = 0, /// Not specified by user
    C99      = 1, /// Tables from C99 standard
    C11      = 2, /// Tables from C11 standard
    UAX31    = 3, /// Tables from the Unicode Standard Annex 31: UNICODE IDENTIFIERS AND SYNTAX
    All      = 4, /// The least restrictive set of all other tables
}

/// Specifies the mode for error printing
enum ErrorPrintMode : ubyte
{
    simpleError,      // Print errors without squiggles and carets
    printErrorContext, // Print errors with context (source line and caret)
}

extern(C++) struct Output
{
    bool doOutput;      // Output is enabled
    bool fullOutput;    // Generate comments for hidden declarations (for -HC),
                        // and don't strip the bodies of plain (non-template) functions (for -H)

    const(char)[] dir;  // write to directory 'dir'
    const(char)[] name; // write to file 'name'
    Array!(const(char)*) files; // Other files associated with this output,
                                // e.g. macro include files for Ddoc, dependencies for makedeps
    OutBuffer* buffer;  // if this output is buffered, this is the buffer
    int bufferLines;    // number of lines written to the buffer
}

/// Command line state related to printing usage about other switches
extern(C++) struct Help
{
    bool manual;       // open browser on compiler manual
    bool usage;        // print usage and exit
    // print help of switch:
    bool mcpu;         // -mcpu
    bool transition;   // -transition
    bool check;        // -check
    bool checkAction;  // -checkaction
    bool revert;       // -revert
    bool preview;      // -preview
    bool externStd;    // -extern-std
    bool hc;           // -HC
}

extern(C++) struct Verbose
{
    bool verbose;           // verbose compile
    bool showColumns;       // print character (column) numbers in diagnostics
    bool tls;               // identify thread local variables
    bool templates;         // collect and list statistics on template instantiations
    // collect and list statistics on template instantiations origins.
    // TODO: make this an enum when we want to list other kinds of instances
    bool templatesListInstances;
    bool gc;                // identify gc usage
    bool field;             // identify non-mutable field variables
    bool complex = true;    // identify complex/imaginary type usage
    bool vin;               // identify 'in' parameters
    bool showGaggedErrors;  // print gagged errors anyway
    bool logo;              // print compiler logo
    bool color;             // use ANSI colors in console output
    bool cov;               // generate code coverage data
    ErrorPrintMode errorPrintMode; // enum for error printing mode
    MessageStyle messageStyle = MessageStyle.digitalmars; // style of file/line annotations on messages
    uint errorLimit = 20;
    uint errorSupplementLimit = 6;      // Limit the number of supplemental messages for each error (0 means unlimited)

    uint errorSupplementCount() @safe
    {
        if (verbose)
            return uint.max;
        if (errorSupplementLimit == 0)
            return uint.max;
        return errorSupplementLimit;
    }
}

extern (C++) struct ImportPathInfo {
    const(char)* path; // char*'s of where to look for import modules
}

/// Put command line switches in here
extern (C++) struct Param
{
    bool obj = true;        // write object file
    bool multiobj;          // break one object file into multiple ones
    bool trace;             // insert profiling hooks
    bool tracegc;           // instrument calls to 'new'
    bool vcg_ast;           // write-out codegen-ast
    DiagnosticReporting useDeprecated = DiagnosticReporting.inform;  // how use of deprecated features are handled
    bool useUnitTests;          // generate unittest code
    bool useInline = false;     // inline expand functions
    bool release;           // build release version
    bool preservePaths;     // true means don't strip path from source file
    DiagnosticReporting useWarnings = DiagnosticReporting.off;  // how compiler warnings are handled
    bool cov;               // generate code coverage data
    ubyte covPercent;       // 0..100 code coverage percentage required
    bool ctfe_cov = false;  // generate coverage data for ctfe
    bool ignoreUnsupportedPragmas = true;  // rather than error on them
    bool useModuleInfo = true;   // generate runtime module information
    bool useTypeInfo = true;     // generate runtime type information
    bool useExceptions = true;   // support exception handling
    bool useGC = true;           // support features that require the D runtime GC
    bool betterC;           // be a "better C" compiler; no dependency on D runtime
    bool addMain;           // add a default main() function
    bool allInst;           // generate code for all template instantiations
    bool bitfields;         // support C style bit fields

    CppStdRevision cplusplus = CppStdRevision.cpp11;    // version of C++ standard to support

    Help help;
    Verbose v;

    // Options for `-preview=/-revert=`
    FeatureState useDIP25 = FeatureState.enabled; // implement https://wiki.dlang.org/DIP25
    FeatureState useDIP1000;     // implement https://dlang.org/spec/memory-safe-d.html#scope-return-params
    bool ehnogc;                 // use @nogc exception handling
    bool useDIP1021;             // implement https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1021.md
    FeatureState fieldwise;      // do struct equality testing field-wise rather than by memcmp()
    bool fixAliasThis;           // if the current scope has an alias this, check it before searching upper scopes
    FeatureState rvalueRefParam; // allow rvalues to be arguments to ref parameters
                                 // https://dconf.org/2019/talks/alexandrescu.html
                                 // https://gist.github.com/andralex/e5405a5d773f07f73196c05f8339435a
                                 // https://digitalmars.com/d/archives/digitalmars/D/Binding_rvalues_to_ref_parameters_redux_325087.html
                                 // Implementation: https://github.com/dlang/dmd/pull/9817
    FeatureState safer;          // safer by default (more @safe checks in unattributed code)
                                 // https://github.com/WalterBright/documents/blob/38f0a846726b571f8108f6e63e5e217b91421c86/safer.md
    FeatureState noSharedAccess; // read/write access to shared memory objects
    bool previewIn;              // `in` means `[ref] scope const`, accepts rvalues
    bool inclusiveInContracts;   // 'in' contracts of overridden methods must be a superset of parent contract
    bool shortenedMethods = true;       // allow => in normal function declarations
    bool fixImmutableConv;       // error on unsound immutable conversion - https://github.com/dlang/dmd/pull/14070
    bool fix16997 = true;        // fix integral promotions for unary + - ~ operators
                                 // https://issues.dlang.org/show_bug.cgi?id=16997
    FeatureState dtorFields;     // destruct fields of partially constructed objects
                                 // https://issues.dlang.org/show_bug.cgi?id=14246
    FeatureState systemVariables; // limit access to variables marked @system from @safe code

    CHECKENABLE useInvariants  = CHECKENABLE._default;  // generate class invariant checks
    CHECKENABLE useIn          = CHECKENABLE._default;  // generate precondition checks
    CHECKENABLE useOut         = CHECKENABLE._default;  // generate postcondition checks
    CHECKENABLE useArrayBounds = CHECKENABLE._default;  // when to generate code for array bounds checks
    CHECKENABLE useAssert      = CHECKENABLE._default;  // when to generate code for assert()'s
    CHECKENABLE useSwitchError = CHECKENABLE._default;  // check for switches without a default
    CHECKENABLE boundscheck    = CHECKENABLE._default;  // state of -boundscheck switch

    CHECKACTION checkAction = CHECKACTION.D; // action to take when bounds, asserts or switch defaults are violated

    CLIIdentifierTable dIdentifierTable = CLIIdentifierTable.default_;
    CLIIdentifierTable cIdentifierTable = CLIIdentifierTable.default_;

    const(char)[] argv0;                // program name
    Array!(const(char)*) modFileAliasStrings; // array of char*'s of -I module filename alias strings
    Array!(ImportPathInfo) imppath;       // array of import path information of where to look for import modules
    Array!(const(char)*) fileImppath;   // array of char*'s of where to look for file import modules
    const(char)[] objdir;                // .obj/.lib file output directory
    const(char)[] objname;               // .obj file output name
    const(char)[] libname;               // .lib file output name

    Output ddoc;                        // Generate embedded documentation comments
    Output dihdr;                       // Generate `.di` 'header' files
    Output cxxhdr;                      // Generate 'Cxx header' file
    Output json;                        // Generate JSON file
    JsonFieldFlags jsonFieldFlags;      // JSON field flags to include
    Output makeDeps;                    // Generate make file dependencies
    Output mixinOut;                    // write expanded mixins for debugging
    Output moduleDeps;                  // Generate `.deps` module dependencies

    uint debuglevel;                    // debug level
    uint versionlevel;                  // version level

    bool run; // run resulting executable
    Strings runargs; // arguments for executable
    Array!(const(char)*) cppswitches;   // C preprocessor switches
    const(char)* cpp;                   // if not null, then this specifies the C preprocessor

    // Linker stuff
    Array!(const(char)*) objfiles;
    Array!(const(char)*) linkswitches;
    Array!bool linkswitchIsForCC;
    Array!(const(char)*) libfiles;
    Array!(const(char)*) dllfiles;
    const(char)[] deffile;
    const(char)[] resfile;
    const(char)[] exefile;
    const(char)[] mapfile;

    bool fullyQualifiedObjectFiles; // prepend module names to object files to prevent name conflicts with -od

    // Time tracing
    bool timeTrace = false; /// Whether profiling of compile time is enabled
    uint timeTraceGranularityUs = 500; /// In microseconds, minimum event size to report
    const(char)* timeTraceFile; /// File path of output file

    ///
    bool parsingUnittestsRequired() @safe
    {
        return useUnitTests || ddoc.doOutput || dihdr.doOutput;
    }
}

enum mars_ext = "d";        // for D source files
enum doc_ext  = "html";     // for Ddoc generated files
enum ddoc_ext = "ddoc";     // for Ddoc macro include files
enum dd_ext   = "dd";       // for Ddoc source files
enum hdr_ext  = "di";       // for D 'header' import files
enum json_ext = "json";     // for JSON files
enum map_ext  = "map";      // for .map files
enum c_ext    = "c";        // for C source files
enum i_ext    = "i";        // for preprocessed C source file

/**
 * Collection of global compiler settings and global state used by the frontend
 */
extern (C++) struct Global
{
    const(char)[] inifilename; /// filename of configuration file as given by `-conf=`, or default value

    string copyright = "Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved";
    string written = "written by Walter Bright";

    Array!(ImportPathInfo) path;       /// Array of path informations which form the import lookup path
    Array!(const(char)*) importPaths;  /// Array of char*'s which form the import lookup path without metadata
    Array!(const(char)*) filePath;     /// Array of char*'s which form the file import lookup path

    private enum string _version = import("VERSION");
    char[26] datetime;      /// string returned by ctime()
    CompileEnv compileEnv;

    Param params;           /// command line parameters
    uint errors;            /// number of errors reported so far
    uint deprecations;      /// number of deprecations reported so far
    uint warnings;          /// number of warnings reported so far
    uint gag;               /// !=0 means gag reporting of errors & warnings
    uint gaggedErrors;      /// number of errors reported while gagged
    uint gaggedWarnings;    /// number of warnings reported while gagged

    void* console;         /// opaque pointer to console for controlling text attributes

    Array!Identifier versionids; /// command line versions and predefined versions
    Array!Identifier debugids;   /// command line debug versions and predefined versions

    bool hasMainFunction; /// Whether a main function has already been compiled in (for -main switch)
    uint varSequenceNumber = 1; /// Relative lifetime of `VarDeclaration` within a function, used for `scope` checks

    /// Cache files read from disk
    FileManager fileManager;

    enum recursionLimit = 500; /// number of recursive template expansions before abort

    ErrorSink errorSink;       /// where the error messages go
    ErrorSink errorSinkNull;   /// where the error messages are ignored

    extern (C++) DArray!ubyte function(FileName, ref const Loc, ref OutBuffer) preprocess;

  nothrow:

    /**
     * Start ignoring compile errors instead of reporting them.
     *
     * Used for speculative compilation like `__traits(compiles, XXX)`, but also internally
     * to e.g. try out an `alias this` rewrite without comitting to it.
     *
     * Works like a stack, so N calls to `startGagging` should be paired with N
     * calls to `endGagging`.
     *
     * Returns: the current number of gagged errors, which should later be passed to `endGagging`
     */
    extern (C++) uint startGagging() @safe
    {
        ++gag;
        gaggedWarnings = 0;
        return gaggedErrors;
    }

    /**
     * Stop gagging, restoring the old gagged state before the most recent call to `startGagging`.
     *
     * Params:
     *   oldGagged = the previous number of errors, as returned by `startGagging`
     * Returns: true if errors occurred while gagged.
     */
    extern (C++) bool endGagging(uint oldGagged) @safe
    {
        bool anyErrs = (gaggedErrors != oldGagged);
        --gag;
        // Restore the original state of gagged errors; set total errors
        // to be original errors + new ungagged errors.
        errors -= (gaggedErrors - oldGagged);
        gaggedErrors = oldGagged;
        return anyErrs;
    }

    /**
     * Increment the error count to record that an error has occurred in the current context.
     *
     * An error message may or may not have been printed.
     */
    extern (C++) void increaseErrorCount() @safe
    {
        if (gag)
            ++gaggedErrors;
        ++errors;
    }

    extern (C++) void _init()
    {
        errorSink = new ErrorSinkCompiler;
        errorSinkNull = new ErrorSinkNull;

        this.fileManager = new FileManager();
        version (MARS)
        {
            compileEnv.vendor = "Digital Mars D";

            // -color=auto is the default value
            import dmd.console : detectTerminal, detectColorPreference;
            params.v.color = detectTerminal() && detectColorPreference();
        }
        else version (IN_GCC)
        {
            compileEnv.vendor = "GNU D";
        }
        else version (IN_LLVM)
        {
            compileEnv.vendor = "LDC";

            import dmd.console : detectTerminal;
            params.v.color = detectTerminal();
        }

        params.v.errorPrintMode = ErrorPrintMode.printErrorContext; // Enable error context globally by default
        compileEnv.versionNumber = parseVersionNumber(versionString());

        /* Initialize date, time, and timestamp
         */
        import core.stdc.time;
        import core.stdc.stdlib : getenv;

        time_t ct;
        // https://issues.dlang.org/show_bug.cgi?id=20444
        if (auto p = getenv("SOURCE_DATE_EPOCH"))
        {
            if (!ct.parseDigits(p[0 .. strlen(p)]))
                errorSink.error(Loc.initial, "value of environment variable `SOURCE_DATE_EPOCH` should be a valid UNIX timestamp, not: `%s`", p);
        }
        else
            core.stdc.time.time(&ct);
        const p = ctime(&ct);
        assert(p);
        datetime[] = p[0 .. 26];

        __gshared char[11 + 1] date = 0;        // put in BSS segment
        __gshared char[8  + 1] time = 0;
        __gshared char[24 + 1] timestamp = 0;

        const dsz = snprintf(&date[0], date.length, "%.6s %.4s", p + 4, p + 20);
        const tsz = snprintf(&time[0], time.length, "%.8s", p + 11);
        const tssz = snprintf(&timestamp[0], timestamp.length, "%.24s", p);
        assert(dsz > 0 && tsz > 0 && tssz > 0);
        compileEnv.time = time[0 .. tsz];
        compileEnv.date = date[0 .. dsz];
        compileEnv.timestamp = timestamp[0 .. tssz];
    }

    /**
     * Deinitializes the global state of the compiler.
     *
     * This can be used to restore the state set by `_init` to its original
     * state.
     */
    extern (D) void deinitialize()
    {
        this = this.init;
    }

    /**
     * Computes the version number __VERSION__ from the compiler version string.
     */
    extern (D) private static uint parseVersionNumber(string version_) @safe
    {
        //
        // parse _version
        //
        uint major = 0;
        uint minor = 0;
        bool point = false;
        // skip initial 'v'
        foreach (const c; version_[1..$])
        {
            if ('0' <= c && c <= '9') // isdigit
            {
                minor = minor * 10 + c - '0';
            }
            else if (c == '.')
            {
                if (point)
                    break; // ignore everything after second '.'
                point = true;
                major = minor;
                minor = 0;
            }
            else
                break;
        }
        return major * 1000 + minor;
    }

    /**
     * Indicate to stateful error sinks that no more errors can be produced.
     * This is to support error sinks that collect information to produce a
     * single (say) report.
     */
    extern(C++) void plugErrorSinks()
    {
        global.errorSink.plugSink();
        global.errorSinkNull.plugSink();
    }

    /**
    Returns: the version as the number that would be returned for __VERSION__
    */
    extern(C++) uint versionNumber() @safe
    {
        return compileEnv.versionNumber;
    }

    /**
    Returns: compiler version string.
    */
    extern(D) string versionString() @safe
    {
        return _version;
    }

    /**
    Returns: compiler version as char string.
    */
    extern(C++) const(char*) versionChars()
    {
        return _version.ptr;
    }
}

// Because int64_t and friends may be any integral type of the
// correct size, we have to explicitly ask for the correct
// integer type to get the correct mangling with dmd

// Be careful not to care about sign when using dinteger_t
// use this instead of integer_t to
// avoid conflicts with system #include's
alias dinteger_t = ulong;
// Signed and unsigned variants
alias sinteger_t = long;
alias uinteger_t = ulong;

/// Collection of global state
extern (C++) __gshared Global global;
