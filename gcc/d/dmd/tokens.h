
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/tokens.h
 */

#pragma once

#include "root/dcompat.h"
#include "root/port.h"
#include "globals.h"

class Identifier;

/* Tokens:
        (       )
        [       ]
        {       }
        <       >       <=      >=      ==      !=      ===     !==
        <<      >>      <<=     >>=     >>>     >>>=
        +       -       +=      -=
        *       /       %       *=      /=      %=
        &       |       ^       &=      |=      ^=
        =       !       ~       @
        ^^      ^^=
        ++      --
        .       ->      :       ,       =>
        ?       &&      ||
 */

enum class TOK : unsigned char
{
    reserved,

    // Other
    leftParenthesis,
    rightParenthesis,
    leftBracket,
    rightBracket,
    leftCurly,
    rightCurly,
    colon,
    semicolon,
    dotDotDot,
    endOfFile,
    cast_,
    null_,
    assert_,
    true_,
    false_,
    throw_,
    new_,
    delete_,
    variable,
    slice,
    version_,
    module_,
    dollar,
    template_,
    typeof_,
    pragma_,
    typeid_,
    comment,

    // Operators
    lessThan,
    greaterThan,
    lessOrEqual,
    greaterOrEqual,
    equal,
    notEqual,
    identity,
    notIdentity,
    is_,

    leftShift,
    rightShift,
    leftShiftAssign,
    rightShiftAssign,
    unsignedRightShift,
    unsignedRightShiftAssign,
    concatenateAssign, // ~=
    add,
    min,
    addAssign,
    minAssign,
    mul,
    div,
    mod,
    mulAssign,
    divAssign,
    modAssign,
    and_,
    or_,
    xor_,
    andAssign,
    orAssign,
    xorAssign,
    assign,
    not_,
    tilde,
    plusPlus,
    minusMinus,
    dot,
    comma,
    question,
    andAnd,
    orOr,

    // Numeric literals
    int32Literal,
    uns32Literal,
    int64Literal,
    uns64Literal,
    int128Literal,
    uns128Literal,
    float32Literal,
    float64Literal,
    float80Literal,
    imaginary32Literal,
    imaginary64Literal,
    imaginary80Literal,

    // Char constants
    charLiteral,
    wcharLiteral,
    dcharLiteral,

    // Leaf operators
    identifier,
    string_,
    hexadecimalString,
    this_,
    super_,
    error,

    // Basic types
    void_,
    int8,
    uns8,
    int16,
    uns16,
    int32,
    uns32,
    int64,
    uns64,
    int128,
    uns128,
    float32,
    float64,
    float80,
    imaginary32,
    imaginary64,
    imaginary80,
    complex32,
    complex64,
    complex80,
    char_,
    wchar_,
    dchar_,
    bool_,

    // Aggregates
    struct_,
    class_,
    interface_,
    union_,
    enum_,
    import_,
    alias_,
    override_,
    delegate_,
    function_,
    mixin_,
    align_,
    extern_,
    private_,
    protected_,
    public_,
    export_,
    static_,
    final_,
    const_,
    abstract_,
    debug_,
    deprecated_,
    in_,
    out_,
    inout_,
    lazy_,
    auto_,
    package_,
    immutable_,

    // Statements
    if_,
    else_,
    while_,
    for_,
    do_,
    switch_,
    case_,
    default_,
    break_,
    continue_,
    with_,
    synchronized_,
    return_,
    goto_,
    try_,
    catch_,
    finally_,
    asm_,
    foreach_,
    foreach_reverse_,
    scope_,
    onScopeExit,
    onScopeFailure,
    onScopeSuccess,

    // Contracts
    invariant_,

    // Testing
    unittest_,

    // Added after 1.0
    argumentTypes,
    ref_,
    macro_,

    parameters,
    traits,
    pure_,
    nothrow_,
    gshared,
    line,
    file,
    fileFullPath,
    moduleString,   // __MODULE__
    functionString, // __FUNCTION__
    prettyFunction, // __PRETTY_FUNCTION__
    shared_,
    at,
    pow,
    powAssign,
    goesTo,
    vector,
    pound,

    arrow,      // ->
    colonColon, // ::
    wchar_tLiteral,
    endOfLine,  // \n, \r, \u2028, \u2029
    whitespace,

    // C only keywords
    inline_,
    register_,
    restrict_,
    signed_,
    sizeof_,
    typedef_,
    unsigned_,
    volatile_,
    _Alignas_,
    _Alignof_,
    _Atomic_,
    _Bool_,
    _Complex_,
    _Generic_,
    _Imaginary_,
    _Noreturn_,
    _Static_assert_,
    _Thread_local_,

    // C only extended keywords
    _assert,
    _import,
    cdecl_,
    declspec,
    stdcall,
    thread,
    pragma,
    int128_,
    attribute__,

    MAX,
};

enum class EXP : unsigned char
{
    reserved,

    // Other
    negate,
    cast_,
    null_,
    assert_,
    array,
    call,
    address,
    type,
    throw_,
    new_,
    delete_,
    star,
    symbolOffset,
    variable,
    dotVariable,
    dotIdentifier,
    dotTemplateInstance,
    dotType,
    slice,
    arrayLength,
    dollar,
    template_,
    dotTemplateDeclaration,
    declaration,
    dSymbol,
    typeid_,
    uadd,
    remove,
    newAnonymousClass,
    arrayLiteral,
    assocArrayLiteral,
    structLiteral,
    classReference,
    thrownException,
    delegatePointer,
    delegateFunctionPointer,

    // Operators
    lessThan,
    greaterThan,
    lessOrEqual,
    greaterOrEqual,
    equal,
    notEqual,
    identity,
    notIdentity,
    index,
    is_,

    leftShift,
    rightShift,
    leftShiftAssign,
    rightShiftAssign,
    unsignedRightShift,
    unsignedRightShiftAssign,
    concatenate,
    concatenateAssign, // ~=
    concatenateElemAssign,
    concatenateDcharAssign,
    add,
    min,
    addAssign,
    minAssign,
    mul,
    div,
    mod,
    mulAssign,
    divAssign,
    modAssign,
    and_,
    or_,
    xor_,
    andAssign,
    orAssign,
    xorAssign,
    assign,
    not_,
    tilde,
    plusPlus,
    minusMinus,
    construct,
    blit,
    dot,
    comma,
    question,
    andAnd,
    orOr,
    prePlusPlus,
    preMinusMinus,

    // Leaf operators
    identifier,
    string_,
    this_,
    super_,
    halt,
    tuple,
    error,

    // Basic types
    void_,
    int64,
    float64,
    complex80,
    import_,
    delegate_,
    function_,
    mixin_,
    in_,
    break_,
    continue_,
    goto_,
    scope_,

    traits,
    overloadSet,
    line,
    file,
    fileFullPath,
    moduleString,   // __MODULE__
    functionString, // __FUNCTION__
    prettyFunction, // __PRETTY_FUNCTION__
    pow,
    powAssign,
    vector,

    voidExpression,
    cantExpression,
    showCtfeContext,
    objcClassReference,
    vectorArray,
    compoundLiteral, // ( type-name ) { initializer-list }
    _Generic_,
    interval,

    MAX
};

#define TOKwild TOKinout

// Token has an anonymous struct, which is not strict ISO C++.
#if defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#endif

struct Token
{
    Token *next;
    Loc loc;
    const utf8_t *ptr;    // pointer to first character of this token within buffer
    TOK value;
    DString blockComment; // doc comment string prior to this token
    DString lineComment;  // doc comment for previous token
    union
    {
        // Integers
        sinteger_t intvalue;
        uinteger_t unsvalue;

        // Floats
        real_t floatvalue;

        struct
        {   utf8_t *ustring;     // UTF8 string
            unsigned len;
            unsigned char postfix;      // 'c', 'w', 'd'
        };

        Identifier *ident;
    };

    Token() : next(NULL) {}
    const char *toChars() const;

    static const char *toChars(TOK value);
};

#if defined(__GNUC__)
#pragma GCC diagnostic pop
#endif
