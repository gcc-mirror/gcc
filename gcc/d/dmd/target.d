/**
 * Handles target-specific parameters
 *
 * In order to allow for cross compilation, when the compiler produces a binary
 * for a different platform than it is running on, target information needs
 * to be abstracted. This is done in this module, primarily through `Target`.
 *
 * Note:
 * While DMD itself does not support cross-compilation, GDC and LDC do.
 * Hence, this module is (sometimes heavily) modified by them,
 * and contributors should review how their changes affect them.
 *
 * See_Also:
 * - $(LINK2 https://wiki.osdev.org/Target_Triplet, Target Triplets)
 * - $(LINK2 https://github.com/ldc-developers/ldc, LDC repository)
 * - $(LINK2 https://github.com/D-Programming-GDC/gcc, GDC repository)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/target.d, _target.d)
 * Documentation:  https://dlang.org/phobos/dmd_target.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/target.d
 */

module dmd.target;

import dmd.globals : Param;

enum CPU : ubyte
{
    x87,
    mmx,
    sse,
    sse2,
    sse3,
    ssse3,
    sse4_1,
    sse4_2,
    avx,                // AVX1 instruction set
    avx2,               // AVX2 instruction set
    avx512,             // AVX-512 instruction set

    // Special values that don't survive past the command line processing
    baseline,           // (default) the minimum capability CPU
    native              // the machine the compiler is being run on
}

////////////////////////////////////////////////////////////////////////////////
/**
 * Describes a back-end target. At present it is incomplete, but in the future
 * it should grow to contain most or all target machine and target O/S specific
 * information.
 *
 * In many cases, calls to sizeof() can't be used directly for getting data type
 * sizes since cross compiling is supported and would end up using the host
 * sizes rather than the target sizes.
 */
extern (C++) struct Target
{
    import dmd.dscope : Scope;
    import dmd.expression : Expression;
    import dmd.func : FuncDeclaration;
    import dmd.location;
    import dmd.astenums : LINK, TY;
    import dmd.mtype : Type, TypeFunction, TypeTuple;
    import dmd.root.ctfloat : real_t;
    import dmd.statement : Statement;
    import dmd.tokens : EXP;

    /// Bit decoding of the Target.OS
    enum OS : ubyte
    {
        /* These are mutually exclusive; one and only one is set.
         * Match spelling and casing of corresponding version identifiers
         */
        none         = 0,
        linux        = 1,
        Windows      = 2,
        OSX          = 4,
        OpenBSD      = 8,
        FreeBSD      = 0x10,
        Solaris      = 0x20,
        DragonFlyBSD = 0x40,

        // Combination masks
        all = linux | Windows | OSX | OpenBSD | FreeBSD | Solaris | DragonFlyBSD,
        Posix = linux | OSX | OpenBSD | FreeBSD | Solaris | DragonFlyBSD,
    }

    OS os;
    ubyte osMajor;

    // D ABI
    ubyte ptrsize;            /// size of a pointer in bytes
    ubyte realsize;           /// size a real consumes in memory
    ubyte realpad;            /// padding added to the CPU real size to bring it up to realsize
    ubyte realalignsize;      /// alignment for reals
    ubyte classinfosize;      /// size of `ClassInfo`
    ulong maxStaticDataSize;  /// maximum size of static data

    /// C ABI
    TargetC c;

    /// C++ ABI
    TargetCPP cpp;

    /// Objective-C ABI
    TargetObjC objc;

    /// Architecture name
    const(char)[] architectureName;
    CPU cpu = CPU.baseline; // CPU instruction set to target
    bool is64bit;           // generate 64 bit code for x86_64; true by default for 64 bit dmd
    bool isLP64;            // pointers are 64 bits

    // Environmental
    const(char)[] obj_ext;    /// extension for object files
    const(char)[] lib_ext;    /// extension for static library files
    const(char)[] dll_ext;    /// extension for dynamic library files
    bool run_noext;           /// allow -run sources without extensions
    bool omfobj = false;      // for Win32: write OMF object files instead of MsCoff
    /**
     * Values representing all properties for floating point types
     */
    extern (C++) struct FPTypeProperties(T)
    {
        real_t max;         /// largest representable value that's not infinity
        real_t min_normal;  /// smallest representable normalized value that's not 0
        real_t nan;         /// NaN value
        real_t infinity;    /// infinity value
        real_t epsilon;     /// smallest increment to the value 1

        long dig;           /// number of decimal digits of precision
        long mant_dig;      /// number of bits in mantissa
        long max_exp;       /// maximum int value such that 2$(SUPERSCRIPT `max_exp-1`) is representable
        long min_exp;       /// minimum int value such that 2$(SUPERSCRIPT `min_exp-1`) is representable as a normalized value
        long max_10_exp;    /// maximum int value such that 10$(SUPERSCRIPT `max_10_exp` is representable)
        long min_10_exp;    /// minimum int value such that 10$(SUPERSCRIPT `min_10_exp`) is representable as a normalized value
    }

    FPTypeProperties!float FloatProperties;     ///
    FPTypeProperties!double DoubleProperties;   ///
    FPTypeProperties!real_t RealProperties;     ///

    private Type tvalist; // cached lazy result of va_listType()

    private const(Param)* params;  // cached reference to global.params

    /**
     * Initialize the Target
     */
    extern (C++) void _init(ref const Param params);


    /**
     * Deinitializes the global state of the compiler.
     *
     * This can be used to restore the state set by `_init` to its original
     * state.
     */
    void deinitialize()
    {
        this = this.init;
    }

    /**
     * Requested target memory alignment size of the given type.
     * Params:
     *      type = type to inspect
     * Returns:
     *      alignment in bytes
     */
    extern (C++) uint alignsize(Type type);

    /**
     * Requested target field alignment size of the given type.
     * Params:
     *      type = type to inspect
     * Returns:
     *      alignment in bytes
     */
    extern (C++) uint fieldalign(Type type);

    /**
     * Type for the `va_list` type for the target; e.g., required for `_argptr`
     * declarations.
     * NOTE: For Posix/x86_64 this returns the type which will really
     * be used for passing an argument of type va_list.
     * Returns:
     *      `Type` that represents `va_list`.
     */
    extern (C++) Type va_listType(const ref Loc loc, Scope* sc);

    /**
     * Checks whether the target supports a vector type.
     * Params:
     *      sz   = vector type size in bytes
     *      type = vector element type
     * Returns:
     *      0   vector type is supported,
     *      1   vector type is not supported on the target at all
     *      2   vector element type is not supported
     *      3   vector size is not supported
     */
    extern (C++) int isVectorTypeSupported(int sz, Type type);

    /**
     * Checks whether the target supports the given operation for vectors.
     * Params:
     *      type = target type of operation
     *      op   = the unary or binary op being done on the `type`
     *      t2   = type of second operand if `op` is a binary operation
     * Returns:
     *      true if the operation is supported or type is not a vector
     */
    extern (C++) bool isVectorOpSupported(Type type, EXP op, Type t2 = null);

    /**
     * Default system linkage for the target.
     * Returns:
     *      `LINK` to use for `extern(System)`
     */
    extern (C++) LINK systemLinkage();

    /**
     * Describes how an argument type is passed to a function on target.
     * Params:
     *      t = type to break down
     * Returns:
     *      tuple of types if type is passed in one or more registers
     *      empty tuple if type is always passed on the stack
     *      null if the type is a `void` or argtypes aren't supported by the target
     */
    extern (C++) TypeTuple toArgTypes(Type t);

    /**
     * Determine return style of function - whether in registers or
     * through a hidden pointer to the caller's stack.
     * Params:
     *   tf = function type to check
     *   needsThis = true if the function type is for a non-static member function
     * Returns:
     *   true if return value from function is on the stack
     */
    extern (C++) bool isReturnOnStack(TypeFunction tf, bool needsThis);

    /**
     * Decides whether an `in` parameter of the specified POD type is to be
     * passed by reference or by value. To be used with `-preview=in` only!
     * Params:
     *  t = type of the `in` parameter, must be a POD
     * Returns:
     *  `true` if the `in` parameter is to be passed by reference
     */
    extern(C++) bool preferPassByRef(Type t);

    /**
     * Get targetInfo by key
     * Params:
     *  name = name of targetInfo to get
     *  loc = location to use for error messages
     * Returns:
     *  Expression for the requested targetInfo
     */
    extern (C++) Expression getTargetInfo(const(char)* name, const ref Loc loc);

    /**
     * Params:
     *  tf = type of function being called
     * Returns: `true` if the callee invokes destructors for arguments.
     */
    extern (C++) bool isCalleeDestroyingArgs(TypeFunction tf);

    /**
     * Returns true if the implementation for object monitors is always defined
     * in the D runtime library (rt/monitor_.d).
     * Params:
     *      fd = function with `synchronized` storage class.
     *      fbody = entire function body of `fd`
     * Returns:
     *      `false` if the target backend handles synchronizing monitors.
     */
    extern (C++) bool libraryObjectMonitors(FuncDeclaration fd, Statement fbody);

    /**
     * Returns true if the target supports `pragma(linkerDirective)`.
     * Returns:
     *      `false` if the target does not support `pragma(linkerDirective)`.
     */
    extern (C++) bool supportsLinkerDirective() const;
}

////////////////////////////////////////////////////////////////////////////////
/**
 * Functions and variables specific to interfacing with extern(C) ABI.
 */
struct TargetC
{
    enum Runtime : ubyte
    {
        Unspecified,
        Bionic,
        DigitalMars,
        Glibc,
        Microsoft,
        Musl,
        Newlib,
        UClibc,
        WASI,
    }

    enum BitFieldStyle : ubyte
    {
        Unspecified,
        DM,                   /// Digital Mars 32 bit C compiler
        MS,                   /// Microsoft 32 and 64 bit C compilers
                              /// https://docs.microsoft.com/en-us/cpp/c-language/c-bit-fields?view=msvc-160
                              /// https://docs.microsoft.com/en-us/cpp/cpp/cpp-bit-fields?view=msvc-160
        Gcc_Clang,            /// gcc and clang
    }
    bool  crtDestructorsSupported = true; /// Not all platforms support crt_destructor
    ubyte boolsize;           /// size of a C `_Bool` type
    ubyte shortsize;          /// size of a C `short` or `unsigned short` type
    ubyte intsize;            /// size of a C `int` or `unsigned int` type
    ubyte longsize;           /// size of a C `long` or `unsigned long` type
    ubyte long_longsize;      /// size of a C `long long` or `unsigned long long` type
    ubyte long_doublesize;    /// size of a C `long double`
    ubyte wchar_tsize;        /// size of a C `wchar_t` type
    Runtime runtime;          /// vendor of the C runtime to link against
    BitFieldStyle bitFieldStyle; /// different C compilers do it differently
}

////////////////////////////////////////////////////////////////////////////////
/**
 * Functions and variables specific to interface with extern(C++) ABI.
 */
struct TargetCPP
{
    import dmd.dsymbol : Dsymbol;
    import dmd.dclass : ClassDeclaration;
    import dmd.func : FuncDeclaration;
    import dmd.mtype : Type;

    enum Runtime : ubyte
    {
        Unspecified,
        Clang,
        DigitalMars,
        Gcc,
        Microsoft,
        Sun
    }
    bool reverseOverloads;    /// set if overloaded functions are grouped and in reverse order (such as in dmc and cl)
    bool exceptions;          /// set if catching C++ exceptions is supported
    bool twoDtorInVtable;     /// target C++ ABI puts deleting and non-deleting destructor into vtable
    bool splitVBasetable;     /// set if C++ ABI uses separate tables for virtual functions and virtual bases
    bool wrapDtorInExternD;   /// set if C++ dtors require a D wrapper to be callable from runtime
    Runtime runtime;          /// vendor of the C++ runtime to link against

    /**
     * Mangle the given symbol for C++ ABI.
     * Params:
     *      s = declaration with C++ linkage
     * Returns:
     *      string mangling of symbol
     */
    extern (C++) const(char)* toMangle(Dsymbol s);

    /**
     * Get RTTI mangling of the given class declaration for C++ ABI.
     * Params:
     *      cd = class with C++ linkage
     * Returns:
     *      string mangling of C++ typeinfo
     */
    extern (C++) const(char)* typeInfoMangle(ClassDeclaration cd);

    /**
     * Get mangle name of a this-adjusting thunk to the given function
     * declaration for C++ ABI.
     * Params:
     *      fd = function with C++ linkage
     *      offset = call offset to the vptr
     * Returns:
     *      string mangling of C++ thunk, or null if unhandled
     */
    extern (C++) const(char)* thunkMangle(FuncDeclaration fd, int offset);

    /**
     * Gets vendor-specific type mangling for C++ ABI.
     * Params:
     *      t = type to inspect
     * Returns:
     *      string if type is mangled specially on target
     *      null if unhandled
     */
    extern (C++) const(char)* typeMangle(Type t);

    /**
     * Get the type that will really be used for passing the given argument
     * to an `extern(C++)` function, or `null` if unhandled.
     * Params:
     *      t = type to be passed.
     * Returns:
     *      `Type` to use for type `t`.
     */
    extern (C++) Type parameterType(Type t);

    /**
     * Checks whether type is a vendor-specific fundamental type.
     * Params:
     *      t = type to inspect
     *      isFundamental = where to store result
     * Returns:
     *      true if isFundamental was set by function
     */
    extern (C++) bool fundamentalType(const Type t, ref bool isFundamental);

    /**
     * Get the starting offset position for fields of an `extern(C++)` class
     * that is derived from the given base class.
     * Params:
     *      baseClass = base class with C++ linkage
     * Returns:
     *      starting offset to lay out derived class fields
     */
    extern (C++) uint derivedClassOffset(ClassDeclaration baseClass);
}

////////////////////////////////////////////////////////////////////////////////
/**
 * Functions and variables specific to interface with extern(Objective-C) ABI.
 */
struct TargetObjC
{
    bool supported;     /// set if compiler can interface with Objective-C
}

////////////////////////////////////////////////////////////////////////////////
extern (C++) __gshared Target target;
