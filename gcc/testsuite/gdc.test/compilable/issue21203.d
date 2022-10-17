version (CppRuntime_Clang) version = CppMangle_Itanium;
version (CppRuntime_Gcc)   version = CppMangle_Itanium;
version (CppRuntime_Sun)   version = CppMangle_Itanium;

template ScopeClass(C , string name = C.stringof)
//if (is(C == class) && __traits(getLinkage, C) == "C++")
{
    //enum name = C.stringof;
    enum ns = __traits(getCppNamespaces,C);
    extern(C++, class)
    {
        extern(C++,(ns))
        {
            pragma(mangle, C, name)
            struct ScopeClass
            {
                char[__traits(classInstanceSize, C)] buffer;
                //... all the things ...
            }
        }
    }
}

// Basic tests
extern(C++)
{
    class MyClassA {}
    void funa(ScopeClass!MyClassA);           // mangles MyClass
    void funb(const ScopeClass!MyClassA);     // mangles const MyClass
    void func(ref ScopeClass!MyClassA);       // mangles MyClass&
    void fund(ref const ScopeClass!MyClassA); // mangles const MyClass&
    void fune(const(ScopeClass!MyClassA)*);
}

version (CppMangle_Itanium)
{
    static assert(funa.mangleof == "_Z4funa8MyClassA");
    static assert(funb.mangleof == "_Z4funb8MyClassA");
    static assert(func.mangleof == "_Z4funcR8MyClassA");
    static assert(fund.mangleof == "_Z4fundRK8MyClassA");
    static assert(fune.mangleof == "_Z4funePK8MyClassA");
}
else version (CppRuntime_Microsoft)
{
    static assert(funa.mangleof == "?funa@@YAXVMyClassA@@@Z");
    static assert(funb.mangleof == "?funb@@YAXVMyClassA@@@Z");
    static if (size_t.sizeof == ulong.sizeof)
    {
        static assert(func.mangleof == "?func@@YAXAEAVMyClassA@@@Z");
        static assert(fund.mangleof == "?fund@@YAXAEBVMyClassA@@@Z");
        static assert(fune.mangleof == "?fune@@YAXPEBVMyClassA@@@Z");
    }
    else
    {
        static assert(func.mangleof == "?func@@YAXAAVMyClassA@@@Z");
        static assert(fund.mangleof == "?fund@@YAXABVMyClassA@@@Z");
        static assert(fune.mangleof == "?fune@@YAXPBVMyClassA@@@Z");
    }
}

//Basic tests with a namespace
extern(C++, "ns")
{
    class MyClassB {}
    void funf(ScopeClass!MyClassB);           // mangles MyClass
    void fung(const ScopeClass!MyClassB);     // mangles const MyClass
    void funh(ref ScopeClass!MyClassB);       // mangles MyClass&
    void funi(ref const ScopeClass!MyClassB); // mangles const MyClass&
    void funj(const(ScopeClass!MyClassB)*);
}

version (CppMangle_Itanium)
{
    static assert(funf.mangleof == "_ZN2ns4funfENS_8MyClassBE");
    static assert(fung.mangleof == "_ZN2ns4fungENS_8MyClassBE");
    static assert(funh.mangleof == "_ZN2ns4funhERNS_8MyClassBE");
    static assert(funi.mangleof == "_ZN2ns4funiERKNS_8MyClassBE");
    static assert(funj.mangleof == "_ZN2ns4funjEPKNS_8MyClassBE");
}
else version (CppRuntime_Microsoft)
{
    static assert(funf.mangleof == "?funf@ns@@YAXVMyClassB@1@@Z");
    static assert(fung.mangleof == "?fung@ns@@YAXVMyClassB@1@@Z");
    static if (size_t.sizeof == ulong.sizeof)
    {
        static assert(funh.mangleof == "?funh@ns@@YAXAEAVMyClassB@1@@Z");
        static assert(funi.mangleof == "?funi@ns@@YAXAEBVMyClassB@1@@Z");
        static assert(funj.mangleof == "?funj@ns@@YAXPEBVMyClassB@1@@Z");
    }
    else
    {
        static assert(funh.mangleof == "?funh@ns@@YAXAAVMyClassB@1@@Z");
        static assert(funi.mangleof == "?funi@ns@@YAXABVMyClassB@1@@Z");
        static assert(funj.mangleof == "?funj@ns@@YAXPBVMyClassB@1@@Z");
    }
}

//Templates
extern(C++)
{
    void funTempl(T)();
    class MyClassC {}
    alias funTemplA = funTempl!(ScopeClass!MyClassC);
    alias funTemplB = funTempl!(const ScopeClass!MyClassC);
    alias funTemplC = funTempl!(const(ScopeClass!MyClassC)*);
    // N.B funTempl!([const] ref ScopeClass!MyClassC) is not permissable in D
}
version (CppMangle_Itanium)
{
    static assert(funTemplA.mangleof == "_Z8funTemplI8MyClassCEvv");
    static assert(funTemplB.mangleof == "_Z8funTemplIK8MyClassCEvv");
    static assert(funTemplC.mangleof == "_Z8funTemplIPK8MyClassCEvv");
}
else version (CppRuntime_Microsoft)
{
    static assert(funTemplA.mangleof == "??$funTempl@VMyClassC@@@@YAXXZ");
    static assert(funTemplB.mangleof == "??$funTempl@$$CBVMyClassC@@@@YAXXZ");
    static if (size_t.sizeof == ulong.sizeof)
        static assert(funTemplC.mangleof == "??$funTempl@PEBVMyClassC@@@@YAXXZ");
    else
        static assert(funTemplC.mangleof == "??$funTempl@PBVMyClassC@@@@YAXXZ");
}

template _function(F)
{
extern(C++, "std")
{
    extern(C++, struct)
    pragma(mangle, "function")
    class _function
    {
    }
}
}
template FunctionOf(F)
{
    F f;
    alias FunctionOf = typeof(*f);
}
extern(C++) void funk(ScopeClass!(_function!(FunctionOf!(void function(int))),"function") a ){ }

version (CppMangle_Itanium)
{
    static assert(funk.mangleof == "_Z4funkSt8functionIFviEE");
}
else version (CppRuntime_Microsoft)
{
    static assert(funk.mangleof == "?funk@@YAXV?$function@$$A6AXH@Z@std@@@Z");
}

extern(C++, "ns")
{
    pragma(mangle, "function")
    class _function2
    {
        public final void test();
    }
}

version (CppMangle_Itanium)
{
    static assert(_function2.test.mangleof == "_ZN2ns8function4testEv");
}
else version (CppRuntime_Microsoft)
{
    static if (size_t.sizeof == ulong.sizeof)
        static assert(_function2.test.mangleof == "?test@function@ns@@QEAAXXZ");
    else
        static assert(_function2.test.mangleof == "?test@function@ns@@QAEXXZ");
}

extern(C++, "ns")
{
    template _function3(T)
    {
        pragma(mangle, _function3, "function")
        class _function3
        {
            public final void test();
        }
    }
}

version (CppMangle_Itanium)
{
    static assert(_function3!(int).test.mangleof == "_ZN2ns8functionIiE4testEv");
}
else version (CppRuntime_Microsoft)
{
    static if (size_t.sizeof == ulong.sizeof)
        static assert(_function3!(int).test.mangleof == "?test@?$function@H@ns@@QEAAXXZ");
    else
        static assert(_function3!(int).test.mangleof == "?test@?$function@H@ns@@QAEXXZ");
}

extern(C++)
{
    struct Foo {}
    pragma(mangle, Foo)  struct Foo_Doppelganger {}

    void funl(Foo_Doppelganger f);
}
version (CppMangle_Itanium)
{
    static assert(funl.mangleof == "_Z4funl3Foo");
}
else version (CppRuntime_Microsoft)
{
    static assert(funl.mangleof == "?funl@@YAXUFoo@@@Z");
}
