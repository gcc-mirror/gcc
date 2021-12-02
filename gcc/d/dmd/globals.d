/**
 * Stores command line options and contains other miscellaneous declarations.
 *
 * Copyright:   Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/globals.d, _globals.d)
 * Documentation:  https://dlang.org/phobos/dmd_globals.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/globals.d
 */

module dmd.globals;

import core.stdc.stdint;
import dmd.root.array;
import dmd.root.filename;
import dmd.root.outbuffer;
import dmd.identifier;

/// Defines a setting for how compiler warnings and deprecations are handled
enum DiagnosticReporting : ubyte
{
    error,        /// generate an error
    inform,       /// generate a warning
    off,          /// disable diagnostic
}

/// How code locations are formatted for diagnostic reporting
enum MessageStyle : ubyte
{
    digitalmars,  /// filename.d(line): message
    gnu,          /// filename.d:line: message, see https://www.gnu.org/prep/standards/html_node/Errors.html
}

/// In which context checks for assertions, contracts, bounds checks etc. are enabled
enum CHECKENABLE : ubyte
{
    _default,     /// initial value
    off,          /// never do checking
    on,           /// always do checking
    safeonly,     /// do checking only in @safe functions
}

/// What should happend when an assertion fails
enum CHECKACTION : ubyte
{
    D,            /// call D assert on failure
    C,            /// call C assert on failure
    halt,         /// cause program halt on failure
    context,      /// call D assert with the error context on failure
}

/// Position Indepent Code setting
enum PIC : ubyte
{
    fixed,              /// located at a specific address
    pic,                /// Position Independent Code
    pie,                /// Position Independent Executable
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

/// Configuration for the C++ header generator
enum CxxHeaderMode : uint
{
    none,   /// Don't generate headers
    silent, /// Generate headers
    verbose /// Generate headers and add comments for hidden declarations
}

/// Trivalent boolean to represent the state of a `revert`able change
enum FeatureState : byte
{
    default_ = -1, /// Not specified by the user
    disabled = 0,  /// Specified as `-revert=`
    enabled = 1    /// Specified as `-preview=`
}

/// Put command line switches in here
extern (C++) struct Param
{
    bool obj = true;        // write object file
    bool link = true;       // perform link
    bool dll;               // generate shared dynamic library
    bool lib;               // write library file instead of object file(s)
    bool multiobj;          // break one object file into multiple ones
    bool oneobj;            // write one object file instead of multiple ones
    bool trace;             // insert profiling hooks
    bool tracegc;           // instrument calls to 'new'
    bool verbose;           // verbose compile
    bool vcg_ast;           // write-out codegen-ast
    bool showColumns;       // print character (column) numbers in diagnostics
    bool vtls;              // identify thread local variables
    bool vtemplates;        // collect and list statistics on template instantiations
    bool vtemplatesListInstances; // collect and list statistics on template instantiations origins. TODO: make this an enum when we want to list other kinds of instances
    bool vgc;               // identify gc usage
    bool vfield;            // identify non-mutable field variables
    bool vcomplex = true;   // identify complex/imaginary type usage
    ubyte symdebug;         // insert debug symbolic information
    bool symdebugref;       // insert debug information for all referenced types, too
    bool optimize;          // run optimizer
    DiagnosticReporting useDeprecated = DiagnosticReporting.inform;  // how use of deprecated features are handled
    bool stackstomp;            // add stack stomping code
    bool useUnitTests;          // generate unittest code
    bool useInline = false;     // inline expand functions
    FeatureState useDIP25;  // implement http://wiki.dlang.org/DIP25
    FeatureState useDIP1000; // implement https://dlang.org/spec/memory-safe-d.html#scope-return-params
    bool useDIP1021;        // implement https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1021.md
    bool release;           // build release version
    bool preservePaths;     // true means don't strip path from source file
    DiagnosticReporting warnings = DiagnosticReporting.off;  // how compiler warnings are handled
    PIC pic = PIC.fixed;    // generate fixed, pic or pie code
    bool color;             // use ANSI colors in console output
    bool cov;               // generate code coverage data
    ubyte covPercent;       // 0..100 code coverage percentage required
    bool ctfe_cov = false;  // generate coverage data for ctfe
    bool nofloat;           // code should not pull in floating point support
    bool ignoreUnsupportedPragmas;  // rather than error on them
    bool useModuleInfo = true;   // generate runtime module information
    bool useTypeInfo = true;     // generate runtime type information
    bool useExceptions = true;   // support exception handling
    bool noSharedAccess;         // read/write access to shared memory objects
    bool previewIn;         // `in` means `[ref] scope const`, accepts rvalues
    bool shortenedMethods; // allow => in normal function declarations
    bool betterC;           // be a "better C" compiler; no dependency on D runtime
    bool addMain;           // add a default main() function
    bool allInst;           // generate code for all template instantiations
    bool fix16997;          // fix integral promotions for unary + - ~ operators
                            // https://issues.dlang.org/show_bug.cgi?id=16997
    bool fixAliasThis;      // if the current scope has an alias this, check it before searching upper scopes
    bool inclusiveInContracts;   // 'in' contracts of overridden methods must be a superset of parent contract
    /** The --transition=safe switch should only be used to show code with
     * silent semantics changes related to @safe improvements.  It should not be
     * used to hide a feature that will have to go through deprecate-then-error
     * before becoming default.
     */
    bool ehnogc;            // use @nogc exception handling
    FeatureState dtorFields; // destruct fields of partially constructed objects
                            // https://issues.dlang.org/show_bug.cgi?id=14246
    bool fieldwise;         // do struct equality testing field-wise rather than by memcmp()
    bool rvalueRefParam;    // allow rvalues to be arguments to ref parameters
                            // http://dconf.org/2019/talks/alexandrescu.html
                            // https://gist.github.com/andralex/e5405a5d773f07f73196c05f8339435a
                            // https://digitalmars.com/d/archives/digitalmars/D/Binding_rvalues_to_ref_parameters_redux_325087.html
                            // Implementation: https://github.com/dlang/dmd/pull/9817

    CppStdRevision cplusplus = CppStdRevision.cpp11;    // version of C++ standard to support

    bool markdown = true;   // enable Markdown replacements in Ddoc
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
    bool logo;              // print compiler logo

    CHECKENABLE useInvariants  = CHECKENABLE._default;  // generate class invariant checks
    CHECKENABLE useIn          = CHECKENABLE._default;  // generate precondition checks
    CHECKENABLE useOut         = CHECKENABLE._default;  // generate postcondition checks
    CHECKENABLE useArrayBounds = CHECKENABLE._default;  // when to generate code for array bounds checks
    CHECKENABLE useAssert      = CHECKENABLE._default;  // when to generate code for assert()'s
    CHECKENABLE useSwitchError = CHECKENABLE._default;  // check for switches without a default
    CHECKENABLE boundscheck    = CHECKENABLE._default;  // state of -boundscheck switch

    CHECKACTION checkAction = CHECKACTION.D; // action to take when bounds, asserts or switch defaults are violated

    uint errorLimit = 20;

    const(char)[] argv0;                // program name
    Array!(const(char)*) modFileAliasStrings; // array of char*'s of -I module filename alias strings
    Array!(const(char)*)* imppath;      // array of char*'s of where to look for import modules
    Array!(const(char)*)* fileImppath;  // array of char*'s of where to look for file import modules
    const(char)[] objdir;                // .obj/.lib file output directory
    const(char)[] objname;               // .obj file output name
    const(char)[] libname;               // .lib file output name

    bool doDocComments;                 // process embedded documentation comments
    const(char)[] docdir;               // write documentation file to docdir directory
    const(char)[] docname;              // write documentation file to docname
    Array!(const(char)*) ddocfiles;     // macro include files for Ddoc

    bool doHdrGeneration;               // process embedded documentation comments
    const(char)[] hdrdir;                // write 'header' file to docdir directory
    const(char)[] hdrname;               // write 'header' file to docname
    bool hdrStripPlainFunctions = true; // strip the bodies of plain (non-template) functions

    CxxHeaderMode doCxxHdrGeneration;      /// Generate 'Cxx header' file
    const(char)[] cxxhdrdir;            // write 'header' file to docdir directory
    const(char)[] cxxhdrname;           // write 'header' file to docname

    bool doJsonGeneration;              // write JSON file
    const(char)[] jsonfilename;          // write JSON file to jsonfilename
    JsonFieldFlags jsonFieldFlags;      // JSON field flags to include

    OutBuffer* mixinOut;                // write expanded mixins for debugging
    const(char)* mixinFile;             // .mixin file output name
    int mixinLines;                     // Number of lines in writeMixins

    uint debuglevel;                    // debug level
    Array!(const(char)*)* debugids;     // debug identifiers

    uint versionlevel;                  // version level
    Array!(const(char)*)* versionids;   // version identifiers

    const(char)[] defaultlibname;        // default library for non-debug builds
    const(char)[] debuglibname;          // default library for debug builds
    const(char)[] mscrtlib;              // MS C runtime library

    const(char)[] moduleDepsFile;        // filename for deps output
    OutBuffer* moduleDeps;              // contents to be written to deps file

    bool emitMakeDeps;                   // whether to emit makedeps
    const(char)[] makeDepsFile;          // filename for makedeps output
    Array!(const(char)*) makeDeps;      // dependencies for makedeps

    MessageStyle messageStyle = MessageStyle.digitalmars; // style of file/line annotations on messages

    bool run; // run resulting executable
    Strings runargs; // arguments for executable

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
}

alias structalign_t = uint;

// magic value means "match whatever the underlying C compiler does"
// other values are all powers of 2
enum STRUCTALIGN_DEFAULT = (cast(structalign_t)~0);

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

    string copyright = "Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved";
    string written = "written by Walter Bright";

    Array!(const(char)*)* path;         /// Array of char*'s which form the import lookup path
    Array!(const(char)*)* filePath;     /// Array of char*'s which form the file import lookup path

    private enum string _version = import("VERSION");
    private enum uint _versionNumber = parseVersionNumber(_version);

    const(char)[] vendor;   /// Compiler backend name

    Param params;           /// command line parameters
    uint errors;            /// number of errors reported so far
    uint warnings;          /// number of warnings reported so far
    uint gag;               /// !=0 means gag reporting of errors & warnings
    uint gaggedErrors;      /// number of errors reported while gagged
    uint gaggedWarnings;    /// number of warnings reported while gagged

    void* console;         /// opaque pointer to console for controlling text attributes

    Array!Identifier* versionids; /// command line versions and predefined versions
    Array!Identifier* debugids;   /// command line debug versions and predefined versions

    enum recursionLimit = 500; /// number of recursive template expansions before abort

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
    extern (C++) uint startGagging()
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
    extern (C++) bool endGagging(uint oldGagged)
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
    extern (C++) void increaseErrorCount()
    {
        if (gag)
            ++gaggedErrors;
        ++errors;
    }

    extern (C++) void _init()
    {
        version (MARS)
        {
            vendor = "Digital Mars D";

            // -color=auto is the default value
            import dmd.console : detectTerminal;
            params.color = detectTerminal();
        }
        else version (IN_GCC)
        {
            vendor = "GNU D";
        }
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
    extern (D) private static uint parseVersionNumber(string version_)
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
    Returns: the version as the number that would be returned for __VERSION__
    */
    extern(C++) uint versionNumber()
    {
        return _versionNumber;
    }

    /**
    Returns: compiler version string.
    */
    extern(D) string versionString()
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

    /**
    Returns: the final defaultlibname based on the command-line parameters
    */
    extern (D) const(char)[] finalDefaultlibname() const
    {
        return params.betterC ? null :
            params.symdebug ? params.debuglibname : params.defaultlibname;
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

alias d_int8 = int8_t;
alias d_uns8 = uint8_t;
alias d_int16 = int16_t;
alias d_uns16 = uint16_t;
alias d_int32 = int32_t;
alias d_uns32 = uint32_t;
alias d_int64 = int64_t;
alias d_uns64 = uint64_t;

version (DMDLIB)
{
    version = LocOffset;
}

/**
A source code location

Used for error messages, `__FILE__` and `__LINE__` tokens, `__traits(getLocation, XXX)`,
debug info etc.
*/
struct Loc
{
    /// zero-terminated filename string, either absolute or relative to cwd
    const(char)* filename;
    uint linnum; /// line number, starting from 1
    uint charnum; /// utf8 code unit index relative to start of line, starting from 1
    version (LocOffset)
        uint fileOffset; /// utf8 code unit index relative to start of file, starting from 0

    static immutable Loc initial; /// use for default initialization of const ref Loc's

nothrow:
    extern (D) this(const(char)* filename, uint linnum, uint charnum) pure
    {
        this.linnum = linnum;
        this.charnum = charnum;
        this.filename = filename;
    }

    extern (C++) const(char)* toChars(
        bool showColumns = global.params.showColumns,
        ubyte messageStyle = global.params.messageStyle) const pure nothrow
    {
        OutBuffer buf;
        if (filename)
        {
            buf.writestring(filename);
        }
        if (linnum)
        {
            final switch (messageStyle)
            {
                case MessageStyle.digitalmars:
                    buf.writeByte('(');
                    buf.print(linnum);
                    if (showColumns && charnum)
                    {
                        buf.writeByte(',');
                        buf.print(charnum);
                    }
                    buf.writeByte(')');
                    break;
                case MessageStyle.gnu: // https://www.gnu.org/prep/standards/html_node/Errors.html
                    buf.writeByte(':');
                    buf.print(linnum);
                    if (showColumns && charnum)
                    {
                        buf.writeByte(':');
                        buf.print(charnum);
                    }
                    break;
            }
        }
        return buf.extractChars();
    }

    /**
     * Checks for equivalence by comparing the filename contents (not the pointer) and character location.
     *
     * Note:
     *  - Uses case-insensitive comparison on Windows
     *  - Ignores `charnum` if `global.params.showColumns` is false.
     */
    extern (C++) bool equals(ref const(Loc) loc) const
    {
        return (!global.params.showColumns || charnum == loc.charnum) &&
               linnum == loc.linnum &&
               FileName.equals(filename, loc.filename);
    }

    /**
     * `opEquals()` / `toHash()` for AA key usage
     *
     * Compare filename contents (case-sensitively on Windows too), not
     * the pointer - a static foreach loop repeatedly mixing in a mixin
     * may lead to multiple equivalent filenames (`foo.d-mixin-<line>`),
     * e.g., for test/runnable/test18880.d.
     */
    extern (D) bool opEquals(ref const(Loc) loc) const @trusted pure nothrow @nogc
    {
        import core.stdc.string : strcmp;

        return charnum == loc.charnum &&
               linnum == loc.linnum &&
               (filename == loc.filename ||
                (filename && loc.filename && strcmp(filename, loc.filename) == 0));
    }

    /// ditto
    extern (D) size_t toHash() const @trusted pure nothrow
    {
        import dmd.root.string : toDString;

        auto hash = hashOf(linnum);
        hash = hashOf(charnum, hash);
        hash = hashOf(filename.toDString, hash);
        return hash;
    }

    /******************
     * Returns:
     *   true if Loc has been set to other than the default initialization
     */
    bool isValid() const pure
    {
        return filename !is null;
    }
}

/// A linkage attribute as defined by `extern(XXX)`
///
/// https://dlang.org/spec/attribute.html#linkage
enum LINK : ubyte
{
    default_,
    d,
    c,
    cpp,
    windows,
    objc,
    system,
}

/// Whether to mangle an external aggregate as a struct or class, as set by `extern(C++, struct)`
enum CPPMANGLE : ubyte
{
    def,      /// default
    asStruct, /// `extern(C++, struct)`
    asClass,  /// `extern(C++, class)`
}

/// Function match levels
///
/// https://dlang.org/spec/function.html#function-overloading
enum MATCH : int
{
    nomatch,   /// no match
    convert,   /// match with conversions
    constant,  /// match with conversion to const
    exact,     /// exact match
}

/// Inline setting as defined by `pragma(inline, XXX)`
enum PINLINE : ubyte
{
    default_, /// as specified on the command line
    never,    /// never inline
    always,   /// always inline
}

alias StorageClass = uinteger_t;

/// Collection of global state
extern (C++) __gshared Global global;
