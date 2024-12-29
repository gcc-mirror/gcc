/**
 * Defines enums common to dmd and dmd as parse library.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/astenums.d, _astenums.d)
 * Documentation:  https://dlang.org/phobos/dmd_astenums.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/astenums.d
 */

module dmd.astenums;

enum Sizeok : ubyte
{
    none,               /// size of aggregate is not yet able to compute
    fwd,                /// size of aggregate is ready to compute
    inProcess,          /// in the midst of computing the size
    done,               /// size of aggregate is set correctly
}

/// D Language version
enum Edition : ubyte
{
    none,
    legacy,          /// Before the introduction of editions
    v2024,           /// Experimental first new edition
    latest = v2024   /// Newest edition that this compiler knows of
}

enum Baseok : ubyte
{
    none,               /// base classes not computed yet
    start,              /// in process of resolving base classes
    done,               /// all base classes are resolved
    semanticdone,       /// all base classes semantic done
}

enum MODFlags : int
{
    none         = 0,    // default (mutable)
    const_       = 1,    // type is const
    immutable_   = 4,    // type is immutable
    shared_      = 2,    // type is shared
    wild         = 8,    // type is wild
    wildconst    = (MODFlags.wild | MODFlags.const_), // type is wild const
    mutable      = 0x10, // type is mutable (only used in wildcard matching)
}

alias MOD = ubyte;

enum STC : ulong  // transfer changes to declaration.h
{
    undefined_          = 0,

    static_             = 1,   /// `static`
    extern_             = 2,   /// `extern`
    const_              = 4,   /// `const`
    final_              = 8,   /// `final`

    abstract_           = 0x10,   /// `abstract`
    parameter           = 0x20,   /// is function parameter
    field               = 0x40,   /// is field of struct, union or class
    override_           = 0x80,   /// `override`

    auto_               = 0x100,   /// `auto`
    synchronized_       = 0x200,   /// `synchronized`
    deprecated_         = 0x400,   /// `deprecated`
    in_                 = 0x800,   /// `in` parameter

    out_                = 0x1000,   /// `out` parameter
    lazy_               = 0x2000,   /// `lazy` parameter
    foreach_            = 0x4000,   /// variable for foreach loop
    variadic            = 0x8000,   /// the `variadic` parameter in: T foo(T a, U b, V variadic...)

    constscoperef       = 0x1_0000,   /// when `in` means const|scope|ref
    templateparameter   = 0x2_0000,   /// template parameter
    ref_                = 0x4_0000,   /// `ref`
    scope_              = 0x8_0000,   /// `scope`

    scopeinferred       = 0x20_0000,   /// `scope` has been inferred and should not be part of mangling, `scope_` must also be set
    return_             = 0x40_0000,   /// 'return ref' or 'return scope' for function parameters
    returnScope         = 0x80_0000,   /// if `ref return scope` then resolve to `ref` and `return scope`

    returninferred      = 0x100_0000,   /// `return` has been inferred and should not be part of mangling, `return_` must also be set
    immutable_          = 0x200_0000,   /// `immutable`
    //                  = 0x400_0000,
    manifest            = 0x800_0000,   /// manifest constant

    nodtor              = 0x1000_0000,   /// do not run destructor
    nothrow_            = 0x2000_0000,   /// `nothrow` meaning never throws exceptions
    pure_               = 0x4000_0000,   /// `pure` function
    tls                 = 0x8000_0000,   /// thread local

    alias_              = 0x1_0000_0000,   /// `alias` parameter
    shared_             = 0x2_0000_0000,   /// accessible from multiple threads
    gshared             = 0x4_0000_0000,   /// accessible from multiple threads, but not typed as `shared`
    wild                = 0x8_0000_0000,   /// for wild type constructor

    property            = 0x10_0000_0000,   /// `@property`
    safe                = 0x20_0000_0000,   /// `@safe`
    trusted             = 0x40_0000_0000,   /// `@trusted`
    system              = 0x80_0000_0000,   /// `@system`

    ctfe                = 0x100_0000_0000,   /// can be used in CTFE, even if it is static
    disable             = 0x200_0000_0000,   /// for functions that are not callable
    result              = 0x400_0000_0000,   /// for result variables passed to out contracts
    nodefaultctor       = 0x800_0000_0000,   /// must be set inside constructor

    temp                = 0x1000_0000_0000,   /// temporary variable
    rvalue              = 0x2000_0000_0000,   /// force rvalue for variables
    nogc                = 0x4000_0000_0000,   /// `@nogc`
    autoref             = 0x8000_0000_0000,   /// Mark for the already deduced `auto ref` parameter

    inference           = 0x1_0000_0000_0000,   /// do attribute inference
    exptemp             = 0x2_0000_0000_0000,   /// temporary variable that has lifetime restricted to an expression
    future              = 0x4_0000_0000_0000,   /// introducing new base class function
    local               = 0x8_0000_0000_0000,   /// do not forward (see dmd.dsymbol.ForwardingScopeDsymbol).

    live                = 0x10_0000_0000_0000,   /// function `@live` attribute
    register            = 0x20_0000_0000_0000,   /// `register` storage class (ImportC)
    volatile_           = 0x40_0000_0000_0000,   /// destined for volatile in the back end

    safeGroup = STC.safe | STC.trusted | STC.system,
    IOR  = STC.constscoperef | STC.in_ | STC.ref_ | STC.out_,
    TYPECTOR = (STC.const_ | STC.immutable_ | STC.shared_ | STC.wild),
    FUNCATTR = (STC.ref_ | STC.nothrow_ | STC.nogc | STC.pure_ | STC.property | STC.live |
                safeGroup),

    /* These are visible to the user, i.e. are expressed by the user
     */
    visibleStorageClasses =
        (STC.auto_ | STC.scope_ | STC.static_ | STC.extern_ | STC.const_ | STC.final_ | STC.abstract_ | STC.synchronized_ |
         STC.deprecated_ | STC.future | STC.override_ | STC.lazy_ | STC.alias_ | STC.out_ | STC.in_ | STC.manifest |
         STC.immutable_ | STC.shared_ | STC.wild | STC.nothrow_ | STC.nogc | STC.pure_ | STC.ref_ | STC.return_ | STC.tls | STC.gshared |
         STC.property | STC.safeGroup | STC.disable | STC.local | STC.live),

    /* These storage classes "flow through" to the inner scope of a Dsymbol
     */
    flowThruAggregate = STC.safeGroup,    /// for an AggregateDeclaration
    flowThruFunction = ~(STC.auto_ | STC.scope_ | STC.static_ | STC.extern_ | STC.abstract_ | STC.deprecated_ | STC.override_ |
                         STC.TYPECTOR | STC.final_ | STC.tls | STC.gshared | STC.ref_ | STC.return_ | STC.property |
                         STC.nothrow_ | STC.pure_ | STC.safe | STC.trusted | STC.system), /// for a FuncDeclaration

}

alias StorageClass = ulong;

/********
 * Determine if it's the ambigous case of where `return` attaches to.
 * Params:
 *   stc = STC flags
 * Returns:
 *   true if (`ref` | `out`) and `scope` and `return`
 */
@safe pure @nogc nothrow
bool isRefReturnScope(const ulong stc)
{
    return (stc & (STC.scope_ | STC.return_)) == (STC.scope_ | STC.return_) &&
           stc & (STC.ref_ | STC.out_);
}

/* This is different from the one in declaration.d, make that fix a separate PR */
static if (0)
__gshared const(StorageClass) STCStorageClass =
    (STC.auto_ | STC.scope_ | STC.static_ | STC.extern_ | STC.const_ | STC.final_ |
     STC.abstract_ | STC.synchronized_ | STC.deprecated_ | STC.override_ | STC.lazy_ |
     STC.alias_ | STC.out_ | STC.in_ | STC.manifest | STC.immutable_ | STC.shared_ |
     STC.wild | STC.nothrow_ | STC.nogc | STC.pure_ | STC.ref_ | STC.return_ | STC.tls |
     STC.gshared | STC.property | STC.live |
     STC.safeGroup | STC.disable);

enum TY : ubyte
{
    Tarray,     // slice array, aka T[]
    Tsarray,    // static array, aka T[dimension]
    Taarray,    // associative array, aka T[type]
    Tpointer,
    Treference,
    Tfunction,
    Tident,
    Tclass,
    Tstruct,
    Tenum,

    Tdelegate,
    Tnone,
    Tvoid,
    Tint8,
    Tuns8,
    Tint16,
    Tuns16,
    Tint32,
    Tuns32,
    Tint64,

    Tuns64,
    Tfloat32,
    Tfloat64,
    Tfloat80,
    Timaginary32,
    Timaginary64,
    Timaginary80,
    Tcomplex32,
    Tcomplex64,
    Tcomplex80,

    Tbool,
    Tchar,
    Twchar,
    Tdchar,
    Terror,
    Tinstance,
    Ttypeof,
    Ttuple,
    Tslice,
    Treturn,

    Tnull,
    Tvector,
    Tint128,
    Tuns128,
    Ttraits,
    Tmixin,
    Tnoreturn,
    Ttag,
}
enum TMAX = TY.max + 1;

alias Tarray = TY.Tarray;
alias Tsarray = TY.Tsarray;
alias Taarray = TY.Taarray;
alias Tpointer = TY.Tpointer;
alias Treference = TY.Treference;
alias Tfunction = TY.Tfunction;
alias Tident = TY.Tident;
alias Tclass = TY.Tclass;
alias Tstruct = TY.Tstruct;
alias Tenum = TY.Tenum;
alias Tdelegate = TY.Tdelegate;
alias Tnone = TY.Tnone;
alias Tvoid = TY.Tvoid;
alias Tint8 = TY.Tint8;
alias Tuns8 = TY.Tuns8;
alias Tint16 = TY.Tint16;
alias Tuns16 = TY.Tuns16;
alias Tint32 = TY.Tint32;
alias Tuns32 = TY.Tuns32;
alias Tint64 = TY.Tint64;
alias Tuns64 = TY.Tuns64;
alias Tfloat32 = TY.Tfloat32;
alias Tfloat64 = TY.Tfloat64;
alias Tfloat80 = TY.Tfloat80;
alias Timaginary32 = TY.Timaginary32;
alias Timaginary64 = TY.Timaginary64;
alias Timaginary80 = TY.Timaginary80;
alias Tcomplex32 = TY.Tcomplex32;
alias Tcomplex64 = TY.Tcomplex64;
alias Tcomplex80 = TY.Tcomplex80;
alias Tbool = TY.Tbool;
alias Tchar = TY.Tchar;
alias Twchar = TY.Twchar;
alias Tdchar = TY.Tdchar;
alias Terror = TY.Terror;
alias Tinstance = TY.Tinstance;
alias Ttypeof = TY.Ttypeof;
alias Ttuple = TY.Ttuple;
alias Tslice = TY.Tslice;
alias Treturn = TY.Treturn;
alias Tnull = TY.Tnull;
alias Tvector = TY.Tvector;
alias Tint128 = TY.Tint128;
alias Tuns128 = TY.Tuns128;
alias Ttraits = TY.Ttraits;
alias Tmixin = TY.Tmixin;
alias Tnoreturn = TY.Tnoreturn;
alias Ttag = TY.Ttag;

enum TFlags
{
    integral     = 1,
    floating     = 2,
    unsigned     = 4,
    real_        = 8,
    imaginary    = 0x10,
    complex      = 0x20,
}

enum PKG : int
{
    unknown,      /// not yet determined whether it's a package.d or not
    module_,      /// already determined that's an actual package.d
    package_,     /// already determined that's an actual package
}

enum ThreeState : ubyte
{
    none,  /// state is not yet computed
    no,    /// state is false
    yes,   /// state is true
}

enum TRUST : ubyte
{
    default_   = 0,
    system     = 1,    // @system (same as TRUST.default)
    trusted    = 2,    // @trusted
    safe       = 3,    // @safe
}

enum PURE : ubyte
{
    impure      = 0,    // not pure at all
    fwdref      = 1,    // it's pure, but not known which level yet
    weak        = 2,    // no mutable globals are read or written
    const_      = 3,    // parameters are values or const = strongly pure
}

// Whether alias this dependency is recursive or not
enum AliasThisRec : int
{
    no           = 0,    // no alias this recursion
    yes          = 1,    // alias this has recursive dependency
    fwdref       = 2,    // not yet known
    typeMask     = 3,    // mask to read no/yes/fwdref
    tracing      = 0x4,  // mark in progress of implicitConvTo/deduceWild
    tracingDT    = 0x8,  // mark in progress of deduceType
}

/***************
 * Variadic argument lists
 * https://dlang.org/spec/function.html#variadic
 */
enum VarArg : ubyte
{
    none     = 0,  /// fixed number of arguments
    variadic = 1,  /// (T t, ...)  can be C-style (core.stdc.stdarg) or D-style (core.vararg)
    typesafe = 2,  /// (T t ...) typesafe https://dlang.org/spec/function.html#typesafe_variadic_functions
                   ///   or https://dlang.org/spec/function.html#typesafe_variadic_functions
    KRvariadic = 3, /// K+R C style variadics (no function prototype)
}

/*************************
 * Identify Statement types with this enum rather than
 * virtual functions
 */
enum STMT : ubyte
{
    Error,
    Peel,
    Exp, DtorExp,
    Mixin,
    Compound, CompoundDeclaration, CompoundAsm,
    UnrolledLoop,
    Scope,
    Forwarding,
    While,
    Do,
    For,
    Foreach,
    ForeachRange,
    If,
    Conditional,
    StaticForeach,
    Pragma,
    StaticAssert,
    Switch,
    Case,
    CaseRange,
    Default,
    GotoDefault,
    GotoCase,
    SwitchError,
    Return,
    Break,
    Continue,
    Synchronized,
    With,
    TryCatch,
    TryFinally,
    ScopeGuard,
    Throw,
    Debug,
    Goto,
    Label,
    Asm, InlineAsm, GccAsm,
    Import,
}

/**********************
 * Discriminant for which kind of initializer
 */
enum InitKind : ubyte
{
    void_,
    default_,
    error,
    struct_,
    array,
    exp,
    C_,
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

/// Source file type
enum FileType : ubyte
{
    d,    /// normal D source file
    dhdr, /// D header file (.di)
    ddoc, /// Ddoc documentation file (.dd)
    c,    /// C source file
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

extern (C++) struct structalign_t
{
  private:
    ushort value = 0;  // unknown
    enum STRUCTALIGN_DEFAULT = 1234;   // default = match whatever the corresponding C compiler does
    bool pack;         // use #pragma pack semantics

  public:
  pure @safe @nogc nothrow:
    bool isDefault() const { return value == STRUCTALIGN_DEFAULT; }
    void setDefault()      { value = STRUCTALIGN_DEFAULT; }
    bool isUnknown() const { return value == 0; }  // value is not set
    void setUnknown()      { value = 0; }
    void set(uint value)   { this.value = cast(ushort)value; }
    uint get() const       { return value; }
    bool isPack() const    { return pack; }
    void setPack(bool pack) { this.pack = pack; }
}

/// Use to return D arrays from C++ functions
extern (C++) struct DArray(T)
{
    T[] data;
}
