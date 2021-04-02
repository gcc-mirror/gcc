
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/idgen.c
 */

// Program to generate string files in d data structures.
// Saves much tedious typing, and eliminates typo problems.
// Generates:
//      id.h
//      id.c

#include "root/dsystem.h"

struct Msgtable
{
    const char* ident;      // name to use in DMD source
    const char* name;       // name in D executable
};

Msgtable msgtable[] =
{
    { "IUnknown", NULL },
    { "Object", NULL },
    { "object", NULL },
    { "string", NULL },
    { "wstring", NULL },
    { "dstring", NULL },
    { "max", NULL },
    { "min", NULL },
    { "This", "this" },
    { "_super", "super" },
    { "ctor", "__ctor" },
    { "dtor", "__dtor" },
    { "__xdtor", "__xdtor" },
    { "__fieldDtor", "__fieldDtor" },
    { "__aggrDtor", "__aggrDtor" },
    { "postblit", "__postblit" },
    { "__xpostblit", "__xpostblit" },
    { "__fieldPostblit", "__fieldPostblit" },
    { "__aggrPostblit", "__aggrPostblit" },
    { "classInvariant", "__invariant" },
    { "unitTest", "__unitTest" },
    { "require", "__require" },
    { "ensure", "__ensure" },
    { "_init", "init" },
    { "__sizeof", "sizeof" },
    { "__xalignof", "alignof" },
    { "_mangleof", "mangleof" },
    { "stringof", NULL },
    { "_tupleof", "tupleof" },
    { "length", NULL },
    { "remove", NULL },
    { "ptr", NULL },
    { "array", NULL },
    { "funcptr", NULL },
    { "dollar", "__dollar" },
    { "ctfe", "__ctfe" },
    { "offset", NULL },
    { "offsetof", NULL },
    { "ModuleInfo", NULL },
    { "ClassInfo", NULL },
    { "classinfo", NULL },
    { "typeinfo", NULL },
    { "outer", NULL },
    { "Exception", NULL },
    { "RTInfo", NULL },
    { "Throwable", NULL },
    { "Error", NULL },
    { "withSym", "__withSym" },
    { "result", "__result" },
    { "returnLabel", "__returnLabel" },
    { "line", NULL },
    { "empty", "" },
    { "p", NULL },
    { "q", NULL },
    { "__vptr", NULL },
    { "__monitor", NULL },
    { "gate", "__gate" },
    { "__c_long", NULL },
    { "__c_ulong", NULL },
    { "__c_longlong", NULL },
    { "__c_ulonglong", NULL },
    { "__c_long_double", NULL },
    { "__c_wchar_t", NULL },
    { "__c_complex_float", NULL },
    { "__c_complex_double", NULL },
    { "__c_complex_real", NULL },
    { "cpp_type_info_ptr", "__cpp_type_info_ptr" },
    { "_assert", "assert" },
    { "_unittest", "unittest" },
    { "_body", "body" },
    { "printf", NULL },
    { "scanf", NULL },

    { "TypeInfo", NULL },
    { "TypeInfo_Class", NULL },
    { "TypeInfo_Interface", NULL },
    { "TypeInfo_Struct", NULL },
    { "TypeInfo_Enum", NULL },
    { "TypeInfo_Pointer", NULL },
    { "TypeInfo_Vector", NULL },
    { "TypeInfo_Array", NULL },
    { "TypeInfo_StaticArray", NULL },
    { "TypeInfo_AssociativeArray", NULL },
    { "TypeInfo_Function", NULL },
    { "TypeInfo_Delegate", NULL },
    { "TypeInfo_Tuple", NULL },
    { "TypeInfo_Const", NULL },
    { "TypeInfo_Invariant", NULL },
    { "TypeInfo_Shared", NULL },
    { "TypeInfo_Wild", "TypeInfo_Inout" },
    { "elements", NULL },
    { "_arguments_typeinfo", NULL },
    { "_arguments", NULL },
    { "_argptr", NULL },
    { "destroy", NULL },
    { "xopEquals", "__xopEquals" },
    { "xopCmp", "__xopCmp" },
    { "xtoHash", "__xtoHash" },

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

    { "nan", NULL },
    { "infinity", NULL },
    { "dig", NULL },
    { "epsilon", NULL },
    { "mant_dig", NULL },
    { "max_10_exp", NULL },
    { "max_exp", NULL },
    { "min_10_exp", NULL },
    { "min_exp", NULL },
    { "min_normal", NULL },
    { "re", NULL },
    { "im", NULL },

    { "C", NULL },
    { "D", NULL },
    { "Windows", NULL },
    { "System", NULL },
    { "Objective", NULL },

    { "exit", NULL },
    { "success", NULL },
    { "failure", NULL },

    { "keys", NULL },
    { "values", NULL },
    { "rehash", NULL },

    { "future", "__future" },
    { "property", NULL },
    { "nogc", NULL },
    { "safe", NULL },
    { "trusted", NULL },
    { "system", NULL },
    { "disable", NULL },

    // For inline assembler
    { "___out", "out" },
    { "___in", "in" },
    { "__int", "int" },
    { "_dollar", "$" },
    { "__LOCAL_SIZE", NULL },

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
    { "opIn", NULL },
    { "opIn_r", NULL },
    { "opStar", NULL },
    { "opDot", NULL },
    { "opDispatch", NULL },
    { "opDollar", NULL },
    { "opUnary", NULL },
    { "opIndexUnary", NULL },
    { "opSliceUnary", NULL },
    { "opBinary", NULL },
    { "opBinaryRight", NULL },
    { "opOpAssign", NULL },
    { "opIndexOpAssign", NULL },
    { "opSliceOpAssign", NULL },
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
    { "monitorenter", "_d_monitorenter" },
    { "monitorexit", "_d_monitorexit" },
    { "criticalenter", "_d_criticalenter2" },
    { "criticalexit", "_d_criticalexit" },
    { "__ArrayEq", NULL },
    { "__ArrayPostblit", NULL },
    { "__ArrayDtor", NULL },
    { "dup", NULL },
    { "_aaApply", NULL },
    { "_aaApply2", NULL },

    // For pragma's
    { "Pinline", "inline" },
    { "lib", NULL },
    { "mangle", NULL },
    { "msg", NULL },
    { "startaddress", NULL },

    // For special functions
    { "tohash", "toHash" },
    { "tostring", "toString" },
    { "getmembers", "getMembers" },

    // Special functions
    { "__alloca", "alloca" },
    { "main", NULL },
    { "WinMain", NULL },
    { "DllMain", NULL },
    { "tls_get_addr", "___tls_get_addr" },
    { "entrypoint", "__entrypoint" },

    // varargs implementation
    { "stdc", NULL },
    { "stdarg", NULL },
    { "va_start", NULL },

    // Builtin functions
    { "std", NULL },
    { "core", NULL },
    { "attribute", NULL },
    { "math", NULL },
    { "sin", NULL },
    { "cos", NULL },
    { "tan", NULL },
    { "_sqrt", "sqrt" },
    { "_pow", "pow" },
    { "atan2", NULL },
    { "rint", NULL },
    { "ldexp", NULL },
    { "rndtol", NULL },
    { "exp", NULL },
    { "expm1", NULL },
    { "exp2", NULL },
    { "yl2x", NULL },
    { "yl2xp1", NULL },
    { "log", NULL },
    { "log2", NULL },
    { "log10", NULL },
    { "round", NULL },
    { "floor", NULL },
    { "trunc", NULL },
    { "fmax", NULL },
    { "fmin", NULL },
    { "fma", NULL },
    { "isnan", NULL },
    { "isInfinity", NULL },
    { "isfinite", NULL },
    { "ceil", NULL },
    { "copysign", NULL },
    { "fabs", NULL },
    { "toPrec", NULL },
    { "simd", NULL },
    { "__prefetch", NULL },
    { "__simd_sto", NULL },
    { "__simd", NULL },
    { "__simd_ib", NULL },
    { "bitop", NULL },
    { "bsf", NULL },
    { "bsr", NULL },
    { "btc", NULL },
    { "btr", NULL },
    { "bts", NULL },
    { "bswap", NULL },
    { "_volatile", "volatile" },
    { "volatileLoad", NULL },
    { "volatileStore", NULL },
    { "_popcnt", NULL },
    { "inp", NULL },
    { "inpl", NULL },
    { "inpw", NULL },
    { "outp", NULL },
    { "outpl", NULL },
    { "outpw", NULL },

    // Traits
    { "isAbstractClass", NULL },
    { "isArithmetic", NULL },
    { "isAssociativeArray", NULL },
    { "isFinalClass", NULL },
    { "isTemplate", NULL },
    { "isPOD", NULL },
    { "isDeprecated", NULL },
    { "isDisabled", NULL },
    { "isFuture" , NULL },
    { "isNested", NULL },
    { "isFloating", NULL },
    { "isIntegral", NULL },
    { "isScalar", NULL },
    { "isStaticArray", NULL },
    { "isUnsigned", NULL },
    { "isVirtualFunction", NULL },
    { "isVirtualMethod", NULL },
    { "isAbstractFunction", NULL },
    { "isFinalFunction", NULL },
    { "isOverrideFunction", NULL },
    { "isStaticFunction", NULL },
    { "isModule", NULL },
    { "isPackage", NULL },
    { "isRef", NULL },
    { "isOut", NULL },
    { "isLazy", NULL },
    { "hasMember", NULL },
    { "identifier", NULL },
    { "getProtection", NULL },
    { "getVisibility", NULL },
    { "parent", NULL },
    { "child", NULL },
    { "getMember", NULL },
    { "getOverloads", NULL },
    { "getVirtualFunctions", NULL },
    { "getVirtualMethods", NULL },
    { "classInstanceSize", NULL },
    { "allMembers", NULL },
    { "derivedMembers", NULL },
    { "isSame", NULL },
    { "compiles", NULL },
    { "getAliasThis", NULL },
    { "getAttributes", NULL },
    { "getFunctionAttributes", NULL },
    { "getFunctionVariadicStyle", NULL },
    { "getParameterStorageClasses", NULL },
    { "getLinkage", NULL },
    { "getUnitTests", NULL },
    { "getVirtualIndex", NULL },
    { "getPointerBitmap", NULL },
    { "isReturnOnStack", NULL },
    { "isZeroInit", NULL },
    { "getTargetInfo", NULL },
    { "getLocation", NULL },
    { "hasPostblit", NULL },
    { "isCopyable", NULL },
    { "toType", NULL },

    // For C++ mangling
    { "allocator", NULL },
    { "basic_string", NULL },
    { "basic_istream", NULL },
    { "basic_ostream", NULL },
    { "basic_iostream", NULL },
    { "char_traits", NULL },

    // Compiler recognized UDA's
    { "udaSelector", "selector" },

    // C names, for undefined identifier error messages
    { "C_NULL", "NULL" },
    { "C_TRUE", "TRUE" },
    { "C_FALSE", "FALSE" },
    { "C_unsigned", "unsigned" },
    { "C_wchar_t", "wchar_t" },
};


int main()
{
    {
        FILE *fp = fopen("id.h","wb");
        if (!fp)
        {
            printf("can't open id.h\n");
            exit(EXIT_FAILURE);
        }

        fprintf(fp, "// File generated by idgen.c\n");
        fprintf(fp, "#ifndef DMD_ID_H\n");
        fprintf(fp, "#define DMD_ID_H 1\n");
        fprintf(fp, "class Identifier;\n");
        fprintf(fp, "struct Id\n");
        fprintf(fp, "{\n");

        for (unsigned i = 0; i < sizeof(msgtable) / sizeof(msgtable[0]); i++)
        {
            const char *id = msgtable[i].ident;
            fprintf(fp,"    static Identifier *%s;\n", id);
        }

        fprintf(fp, "    static void initialize();\n");
        fprintf(fp, "};\n");
        fprintf(fp, "#endif\n");

        fclose(fp);
    }

    {
        FILE *fp = fopen("id.c","wb");
        if (!fp)
        {
            printf("can't open id.c\n");
            exit(EXIT_FAILURE);
        }

        fprintf(fp, "// File generated by idgen.c\n");
        fprintf(fp, "#include \"identifier.h\"\n");
        fprintf(fp, "#include \"id.h\"\n");
        fprintf(fp, "#include \"mars.h\"\n");

        for (unsigned i = 0; i < sizeof(msgtable) / sizeof(msgtable[0]); i++)
        {
            const char *id = msgtable[i].ident;
            const char *p = msgtable[i].name;

            if (!p)
                p = id;
            fprintf(fp,"Identifier *Id::%s;\n", id);
        }

        fprintf(fp, "void Id::initialize()\n");
        fprintf(fp, "{\n");

        for (unsigned i = 0; i < sizeof(msgtable) / sizeof(msgtable[0]); i++)
        {
            const char *id = msgtable[i].ident;
            const char *p = msgtable[i].name;

            if (!p)
                p = id;
            fprintf(fp,"    %s = Identifier::idPool(\"%s\");\n", id, p);
        }

        fprintf(fp, "}\n");

        fclose(fp);
    }

    {
        FILE *fp = fopen("id.d","wb");
        if (!fp)
        {
            printf("can't open id.d\n");
            exit(EXIT_FAILURE);
        }

        fprintf(fp, "// File generated by idgen.c\n");
        fprintf(fp, "\n");
        fprintf(fp, "module ddmd.id;\n");
        fprintf(fp, "\n");
        fprintf(fp, "import ddmd.identifier, ddmd.tokens;\n");
        fprintf(fp, "\n");
        fprintf(fp, "struct Id\n");
        fprintf(fp, "{\n");

        for (unsigned i = 0; i < sizeof(msgtable) / sizeof(msgtable[0]); i++)
        {
            const char *id = msgtable[i].ident;
            const char *p = msgtable[i].name;

            if (!p)
                p = id;
            fprintf(fp, "    extern (C++) static __gshared Identifier %s;\n", id);
        }

        fprintf(fp, "\n");
        fprintf(fp, "    extern (C++) static void initialize()\n");
        fprintf(fp, "    {\n");

        for (unsigned i = 0; i < sizeof(msgtable) / sizeof(msgtable[0]); i++)
        {
            const char *id = msgtable[i].ident;
            const char *p = msgtable[i].name;

            if (!p)
                p = id;
            fprintf(fp,"        %s = Identifier.idPool(\"%s\");\n", id, p);
        }

        fprintf(fp, "    }\n");
        fprintf(fp, "}\n");

        fclose(fp);
    }

    return EXIT_SUCCESS;
}
