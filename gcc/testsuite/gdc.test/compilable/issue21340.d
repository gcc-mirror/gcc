version (CppRuntime_Clang) version = CppMangle_Itanium;
version (CppRuntime_Gcc)   version = CppMangle_Itanium;
version (CppRuntime_Sun)   version = CppMangle_Itanium;

template ScopeClass(C)
if (is(C == class) && __traits(getLinkage, C) == "C++")
{

    extern(C++, class)
    extern(C++, __traits(getCppNamespaces,C))
    extern(C++, (ns))
    class ScopeClass { }
}
extern(C++) class Foo {}
extern(C++) void test(ScopeClass!Foo)
{
}
version(CppMangle_Itanium)
{
    static assert (test.mangleof == "_Z4testP10ScopeClassIP3FooE");
}
else version (CppRuntime_Microsoft)
{
    version (Win32)
    {
        static assert (test.mangleof == "?test@@YAXPAV?$ScopeClass@PAVFoo@@@@@Z");
    }
    version (Win64)
    {
        static assert (test.mangleof == "?test@@YAXPEAV?$ScopeClass@PEAVFoo@@@@@Z");
    }
}
alias AliasSeq(T...) = T;
alias ns = AliasSeq!();
immutable ns2 = AliasSeq!();
extern(C++,(ns)) class Bar {}
extern(C++,) class Baz {}
extern(C++, (ns2)) class Quux {}
