// https://issues.dlang.org/show_bug.cgi?id=15512
// https://issues.dlang.org/show_bug.cgi?id=19893
// https://issues.dlang.org/show_bug.cgi?id=19920
module cppmangle3;

version (CppRuntime_LLVM)      version = CppMangle_Itanium;
version (CppRuntime_GNU)   version = CppMangle_Itanium;
version (CppRuntime_Microsoft)   version = CppMangle_MSVC;
version (CppRuntime_Sun)         version = CppMangle_Itanium;

extern(C++, "true")
{
}

extern(C++, "__traits")
{
}

extern(C++, "foo")
{
}

int foo; // no name clashing with above namespace

extern(C++, "std", "chrono")
{
    void func();
}

version(CppMangle_MSVC) static assert(func.mangleof == "?func@chrono@std@@YAXXZ");
else                    static assert(func.mangleof == "_ZNSt6chrono4funcEv");

struct Foo
{
    extern(C++, "namespace")
    {
        static void bar();
    }
}

alias Alias(alias a) = a;
alias Alias(T) = T;

static assert(is(Alias!(__traits(parent, Foo.bar)) == Foo));

extern(C++, "std"):
debug = def;
version = def;

extern(C++, "std")
{
    debug = def;
    version = def;
}

extern(C++, "foo")
extern(C++, "bar")
version = baz;
