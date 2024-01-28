/**
 * Contains the `Id` struct with a list of predefined symbols the compiler knows about.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/id.d, _id.d)
 * Documentation:  https://dlang.org/phobos/dmd_id.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/id.d
 */

module dmd.id;

import dmd.identifier;
import dmd.tokens;

/**
 * Represents a list of predefined symbols the compiler knows about.
 *
 * All static fields in this struct represents a specific predefined symbol.
 */
extern (C++) struct Id
{
    static __gshared:

    mixin(msgtable.generate(&identifier));

    /**
     * Populates the identifier pool with all predefined symbols.
     *
     * An identifier that corresponds to each static field in this struct will
     * be placed in the identifier pool.
     */
    extern(C++) void initialize()
    {
        mixin(msgtable.generate(&initializer));
    }

    /**
     * Deinitializes the global state of the compiler.
     *
     * This can be used to restore the state set by `initialize` to its original
     * state.
     */
    extern (D) void deinitialize()
    {
        mixin(msgtable.generate(&deinitializer));
    }
}

private:


/**
 * Each element in this array will generate one static field in the `Id` struct
 * and a call to `Identifier.idPool` to populate the identifier pool in the
 * `Id.initialize` method.
 */
immutable Msgtable[] msgtable =
[
    { "IUnknown" },
    { "Object" },
    { "object" },
    { "_size_t", "size_t" },
    { "_ptrdiff_t", "ptrdiff_t" },
    { "string" },
    { "wstring" },
    { "dstring" },
    { "max" },
    { "min" },
    { "This", "this" },
    { "_super", "super" },
    { "ctor", "__ctor" },
    { "dtor", "__dtor" },
    { "__xdtor", "__xdtor" },
    { "__fieldDtor", "__fieldDtor" },
    { "__aggrDtor", "__aggrDtor" },
    { "cppdtor", "__cppdtor" },
    { "ticppdtor", "__ticppdtor" },
    { "postblit", "__postblit" },
    { "__xpostblit", "__xpostblit" },
    { "__fieldPostblit", "__fieldPostblit" },
    { "__aggrPostblit", "__aggrPostblit" },
    { "classInvariant", "__invariant" },
    { "unitTest", "__unitTest" },
    { "require", "__require" },
    { "ensure", "__ensure" },
    { "capture", "__capture" },
    { "this2", "__this" },
    { "_init", "init" },
    { "__sizeof", "sizeof" },
    { "__xalignof", "alignof" },
    { "_mangleof", "mangleof" },
    { "stringof" },
    { "_tupleof", "tupleof" },
    { "length" },
    { "remove" },
    { "ptr" },
    { "array" },
    { "funcptr" },
    { "dollar", "__dollar" },
    { "ctfe", "__ctfe" },
    { "offset" },
    { "offsetof" },
    { "ModuleInfo" },
    { "ClassInfo" },
    { "classinfo" },
    { "typeinfo" },
    { "outer" },
    { "Exception" },
    { "RTInfo" },
    { "Throwable" },
    { "Error" },
    { "withSym", "__withSym" },
    { "result", "__result" },
    { "returnLabel", "__returnLabel" },
    { "line" },
    { "empty", "" },
    { "dotdotdot", "..." }, // use for error messages
    { "p" },
    { "__vptr" },
    { "__monitor" },
    { "gate", "__gate" },
    { "__c_long" },
    { "__c_ulong" },
    { "__c_longlong" },
    { "__c_ulonglong" },
    { "__c_long_double" },
    { "__c_char" },
    { "__c_wchar_t" },
    { "__c_complex_float" },
    { "__c_complex_double" },
    { "__c_complex_real" },
    { "cpp_type_info_ptr", "__cpp_type_info_ptr" },
    { "_assert", "assert" },
    { "_unittest", "unittest" },
    { "_body", "body" },
    { "printf" },
    { "scanf" },

    { "TypeInfo" },
    { "TypeInfo_Class" },
    { "TypeInfo_Interface" },
    { "TypeInfo_Struct" },
    { "TypeInfo_Enum" },
    { "TypeInfo_Pointer" },
    { "TypeInfo_Vector" },
    { "TypeInfo_Array" },
    { "TypeInfo_StaticArray" },
    { "TypeInfo_AssociativeArray" },
    { "TypeInfo_Function" },
    { "TypeInfo_Delegate" },
    { "TypeInfo_Tuple" },
    { "TypeInfo_Const" },
    { "TypeInfo_Invariant" },
    { "TypeInfo_Shared" },
    { "TypeInfo_Wild", "TypeInfo_Inout" },
    { "elements" },
    { "_arguments_typeinfo" },
    { "_arguments" },
    { "_argptr" },
    { "destroy" },
    { "xopEquals", "__xopEquals" },
    { "xopCmp", "__xopCmp" },
    { "xtoHash", "__xtoHash" },
    { "__tmpfordtor" },

    { "LINE", "__LINE__" },
    { "FILE", "__FILE__" },
    { "MODULE", "__MODULE__" },
    { "FUNCTION", "__FUNCTION__" },
    { "PRETTY_FUNCTION", "__PRETTY_FUNCTION__" },
    { "DATE", "__DATE__" },
    { "TIME", "__TIME__" },
    { "TIMESTAMP", "__TIMESTAMP__" },
    { "VENDOR", "__VENDOR__" },
    { "VERSIONX", "__VERSION__" },
    { "EOFX", "__EOF__" },

    { "nan" },
    { "infinity" },
    { "dig" },
    { "epsilon" },
    { "mant_dig" },
    { "max_10_exp" },
    { "max_exp" },
    { "min_10_exp" },
    { "min_exp" },
    { "min_normal" },
    { "re" },
    { "im" },

    { "C" },
    { "D" },
    { "Windows" },
    { "System" },
    { "Objective" },

    { "exit" },
    { "success" },
    { "failure" },

    { "keys" },
    { "values" },
    { "rehash" },

    { "future", "__future" },
    { "property" },
    { "nogc" },
    { "live" },
    { "safe" },
    { "trusted" },
    { "system" },
    { "disable" },

    // For inline assembler
    { "___out", "out" },
    { "___in", "in" },
    { "__int", "int" },
    { "_dollar", "$" },
    { "__LOCAL_SIZE" },

    // For operator overloads
    { "uadd",    "opPos" },
    { "neg",     "opNeg" },
    { "com",     "opCom" },
    { "add",     "opAdd" },
    { "add_r",   "opAdd_r" },
    { "sub",     "opSub" },
    { "sub_r",   "opSub_r" },
    { "mul",     "opMul" },
    { "mul_r",   "opMul_r" },
    { "div",     "opDiv" },
    { "div_r",   "opDiv_r" },
    { "mod",     "opMod" },
    { "mod_r",   "opMod_r" },
    { "eq",      "opEquals" },
    { "cmp",     "opCmp" },
    { "iand",    "opAnd" },
    { "iand_r",  "opAnd_r" },
    { "ior",     "opOr" },
    { "ior_r",   "opOr_r" },
    { "ixor",    "opXor" },
    { "ixor_r",  "opXor_r" },
    { "shl",     "opShl" },
    { "shl_r",   "opShl_r" },
    { "shr",     "opShr" },
    { "shr_r",   "opShr_r" },
    { "ushr",    "opUShr" },
    { "ushr_r",  "opUShr_r" },
    { "cat",     "opCat" },
    { "cat_r",   "opCat_r" },
    { "assign",  "opAssign" },
    { "addass",  "opAddAssign" },
    { "subass",  "opSubAssign" },
    { "mulass",  "opMulAssign" },
    { "divass",  "opDivAssign" },
    { "modass",  "opModAssign" },
    { "andass",  "opAndAssign" },
    { "orass",   "opOrAssign" },
    { "xorass",  "opXorAssign" },
    { "shlass",  "opShlAssign" },
    { "shrass",  "opShrAssign" },
    { "ushrass", "opUShrAssign" },
    { "catass",  "opCatAssign" },
    { "postinc", "opPostInc" },
    { "postdec", "opPostDec" },
    { "index",   "opIndex" },
    { "indexass", "opIndexAssign" },
    { "slice",   "opSlice" },
    { "sliceass", "opSliceAssign" },
    { "call",    "opCall" },
    { "_cast",    "opCast" },
    { "opIn" },
    { "opIn_r" },
    { "opStar" },
    { "opDot" },
    { "opDispatch" },
    { "opDollar" },
    { "opUnary" },
    { "opIndexUnary" },
    { "opSliceUnary" },
    { "opBinary" },
    { "opBinaryRight" },
    { "opOpAssign" },
    { "opIndexOpAssign" },
    { "opSliceOpAssign" },
    { "pow", "opPow" },
    { "pow_r", "opPow_r" },
    { "powass", "opPowAssign" },

    { "classNew", "new" },
    { "classDelete", "delete" },

    // For foreach
    { "apply", "opApply" },
    { "applyReverse", "opApplyReverse" },

    // Ranges
    { "Fempty", "empty" },
    { "Ffront", "front" },
    { "Fback", "back" },
    { "FpopFront", "popFront" },
    { "FpopBack", "popBack" },

    // For internal functions
    { "aaLen", "_aaLen" },
    { "aaKeys", "_aaKeys" },
    { "aaValues", "_aaValues" },
    { "aaRehash", "_aaRehash" },
    { "_aaAsStruct" },
    { "monitorenter", "_d_monitorenter" },
    { "monitorexit", "_d_monitorexit" },
    { "criticalenter", "_d_criticalenter2" },
    { "criticalexit", "_d_criticalexit" },
    { "__ArrayPostblit" },
    { "__ArrayDtor" },
    { "_d_delThrowable" },
    { "_d_newThrowable" },
    { "_d_newclassT" },
    { "_d_newclassTTrace" },
    { "_d_newitemT" },
    { "_d_newitemTTrace" },
    { "_d_newarrayT" },
    { "_d_newarrayTTrace" },
    { "_d_newarraymTX" },
    { "_d_newarraymTXTrace" },
    { "_d_assert_fail" },
    { "dup" },
    { "_aaApply" },
    { "_aaApply2" },
    { "_d_arrayctor" },
    { "_d_arraysetctor" },
    { "_d_arraysetassign" },
    { "_d_arrayassign_l" },
    { "_d_arrayassign_r" },

    { "imported" },
    { "InterpolationHeader" },
    { "InterpolationFooter" },
    { "InterpolatedLiteral" },
    { "InterpolatedExpression" },

    // For pragma's
    { "Pinline", "inline" },
    { "lib" },
    { "linkerDirective" },
    { "mangle" },
    { "msg" },
    { "startaddress" },
    { "crt_constructor" },
    { "crt_destructor" },

    // For special functions
    { "tohash", "toHash" },
    { "tostring", "toString" },
    { "getmembers", "getMembers" },

    // Special functions
    { "__alloca", "alloca" },
    { "main" },
    { "WinMain" },
    { "DllMain" },
    { "CMain", "_d_cmain" },
    { "rt_init" },
    { "__cmp" },
    { "__equals"},
    { "__switch"},
    { "__switch_error"},
    { "__ArrayCast"},
    { "_d_HookTraceImpl" },
    { "_d_arraysetlengthTImpl"},
    { "_d_arraysetlengthT"},
    { "_d_arraysetlengthTTrace"},
    { "_d_arrayappendT" },
    { "_d_arrayappendTTrace" },
    { "_d_arrayappendcTX" },
    { "_d_arrayappendcTXTrace" },
    { "_d_arraycatnTX" },
    { "_d_arraycatnTXTrace" },

    // varargs implementation
    { "stdc" },
    { "stdarg" },
    { "va_start" },

    // Builtin functions
    { "std" },
    { "core" },
    { "config" },
    { "c_complex_float" },
    { "c_complex_double" },
    { "c_complex_real" },
    { "etc" },
    { "attribute" },
    { "atomic" },
    { "atomicOp" },
    { "math" },
    { "sin" },
    { "cos" },
    { "tan" },
    { "_sqrt", "sqrt" },
    { "_pow", "pow" },
    { "atan2" },
    { "rint" },
    { "ldexp" },
    { "rndtol" },
    { "exp" },
    { "expm1" },
    { "exp2" },
    { "yl2x" },
    { "yl2xp1" },
    { "log" },
    { "log2" },
    { "log10" },
    { "round" },
    { "floor" },
    { "trunc" },
    { "fmax" },
    { "fmin" },
    { "fma" },
    { "isnan" },
    { "isInfinity" },
    { "isfinite" },
    { "ceil" },
    { "copysign" },
    { "fabs" },
    { "toPrec" },
    { "simd" },
    { "__prefetch"},
    { "__simd_sto"},
    { "__simd"},
    { "__simd_ib"},
    { "bitop" },
    { "bsf" },
    { "bsr" },
    { "btc" },
    { "btr" },
    { "bts" },
    { "bswap" },
    { "volatile"},
    { "volatileLoad"},
    { "volatileStore"},
    { "_popcnt"},
    { "inp"},
    { "inpl"},
    { "inpw"},
    { "outp"},
    { "outpl"},
    { "outpw"},

    // Traits
    { "isAbstractClass" },
    { "isArithmetic" },
    { "isAssociativeArray" },
    { "isFinalClass" },
    { "isTemplate" },
    { "isPOD" },
    { "isDeprecated" },
    { "isDisabled" },
    { "isFuture" },
    { "isNested" },
    { "isFloating" },
    { "isIntegral" },
    { "isScalar" },
    { "isStaticArray" },
    { "isUnsigned" },
    { "isVirtualFunction" },
    { "isVirtualMethod" },
    { "isAbstractFunction" },
    { "isFinalFunction" },
    { "isOverrideFunction" },
    { "isStaticFunction" },
    { "isModule" },
    { "isPackage" },
    { "isRef" },
    { "isOut" },
    { "isLazy" },
    { "hasMember" },
    { "identifier" },
    { "fullyQualifiedName" },
    { "getProtection" },
    { "getVisibility" },
    { "parent" },
    { "child" },
    { "getMember" },
    { "getOverloads" },
    { "getVirtualFunctions" },
    { "getVirtualMethods" },
    { "classInstanceSize" },
    { "classInstanceAlignment" },
    { "allMembers" },
    { "derivedMembers" },
    { "isSame" },
    { "compiles" },
    { "getAliasThis" },
    { "getAttributes" },
    { "getFunctionAttributes" },
    { "getFunctionVariadicStyle" },
    { "getParameterStorageClasses" },
    { "getLinkage" },
    { "getUnitTests" },
    { "getVirtualIndex" },
    { "getPointerBitmap" },
    { "initSymbol" },
    { "getCppNamespaces" },
    { "isReturnOnStack" },
    { "isZeroInit" },
    { "getTargetInfo" },
    { "getLocation" },
    { "hasPostblit" },
    { "hasCopyConstructor" },
    { "isCopyable" },
    { "toType" },
    { "parameters" },

    // For C++ mangling
    { "allocator" },
    { "basic_string" },
    { "basic_istream" },
    { "basic_ostream" },
    { "basic_iostream" },
    { "char_traits" },

    // Compiler recognized UDA's
    { "udaGNUAbiTag", "gnuAbiTag" },
    { "udaSelector", "selector" },
    { "udaOptional", "optional"},
    { "udaMustUse", "mustuse" },
    { "udaStandalone", "standalone" },

    // C names, for undefined identifier error messages
    { "NULL" },
    { "TRUE" },
    { "FALSE" },
    { "unsigned" },
    { "wchar_t" },

    // for C compiler
    { "ImportC", "__C" },
    { "__tag" },
    { "dllimport" },
    { "dllexport" },
    { "naked" },
    { "thread" },
    { "vector_size" },
    { "__func__" },
    { "always_inline" },
    { "noinline" },
    { "noreturn" },
    { "_nothrow", "nothrow" },
    { "_deprecated", "deprecated" },
    { "_align", "align" },
    { "aligned" },
    { "__pragma", "pragma" },
    { "builtins", "__builtins" },
    { "builtin_va_list", "__builtin_va_list" },
    { "builtin_va_arg", "__builtin_va_arg" },
    { "va_list_tag", "__va_list_tag" },
    { "va_arg" },
    { "pack" },
    { "show" },
    { "push" },
    { "pop" },
    { "_pure", "pure" },
    { "define" },
    { "undef" },
    { "ident" },
];


/*
 * Tuple of DMD source code identifier and symbol in the D executable.
 *
 * The first element of the tuple is the identifier to use in the DMD source
 * code and the second element, if present, is the name to use in the D
 * executable. If second element, `name`, is not present the identifier,
 * `ident`, will be used instead
 */
struct Msgtable
{
    // The identifier to use in the DMD source.
    string ident;

    // The name to use in the D executable
    private string name_;

    /*
     * Returns: the name to use in the D executable, `name_` if non-empty,
     *  otherwise `ident`
     */
    string name() @safe
    {
        return name_ ? name_ : ident;
    }
}

/*
 * Iterates the given Msgtable array, passes each element to the given lambda
 * and accumulates a string from each return value of calling the lambda.
 * Appends a newline character after each call to the lambda.
 */
string generate(immutable(Msgtable)[] msgtable, string function(Msgtable) dg)
{
    string code;

    foreach (i, m ; msgtable)
    {
        if (i != 0)
            code ~= '\n';

        code ~= dg(m);
    }

    return code;
}

// Used to generate the code for each identifier.
string identifier(Msgtable m) @safe
{
    return "Identifier " ~ m.ident ~ ";";
}

// Used to generate the code for each initializer.
string initializer(Msgtable m) @safe
{
    return m.ident ~ ` = Identifier.idPool("` ~ m.name ~ `");`;
}

// Used to generate the code for each deinitializer.
string deinitializer(Msgtable m) @safe
{
    return m.ident ~ " = Identifier.init;";
}
