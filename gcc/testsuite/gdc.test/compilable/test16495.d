// https://issues.dlang.org/show_bug.cgi?id=16495

void types()
{
    static assert(__traits(fullyQualifiedName, string) == "string");
    static assert(__traits(fullyQualifiedName, wstring) == "wstring");
    static assert(__traits(fullyQualifiedName, dstring) == "dstring");
    static assert(__traits(fullyQualifiedName, typeof(null)) == "typeof(null)");
    static assert(__traits(fullyQualifiedName, void) == "void");
    static assert(__traits(fullyQualifiedName, const(void)) == "const(void)");
    static assert(__traits(fullyQualifiedName, shared(void)) == "shared(void)");
    static assert(__traits(fullyQualifiedName, shared const(void)) == "shared(const(void))");
    static assert(__traits(fullyQualifiedName, shared inout(void)) == "shared(inout(void))");
    static assert(__traits(fullyQualifiedName, shared inout const(void)) == "shared(inout(const(void)))");
    static assert(__traits(fullyQualifiedName, inout(void)) == "inout(void)");
    static assert(__traits(fullyQualifiedName, inout const(void)) == "inout(const(void))");
    static assert(__traits(fullyQualifiedName, immutable(void)) == "immutable(void)");
}

struct QualifiedNameTests
{
    struct Inner
    {
        bool value;
    }

    ref const(Inner[string]) func( ref Inner var1, lazy scope string var2 );
    ref const(Inner[string]) retfunc( return ref Inner var1 );
    Inner inoutFunc(inout Inner) inout;
    shared(const(Inner[string])[]) data;
    const Inner delegate(double, string) @safe nothrow deleg;
    inout(int) delegate(inout int) inout inoutDeleg;
    Inner function(out double, string) funcPtr;
    extern(C) Inner function(double, string) cFuncPtr;

    extern(C) void cVarArg(int, ...);
    void dVarArg(...);
    void dVarArg2(int, ...);
    void typesafeVarArg(int[] ...);

    Inner[] array;
    Inner[16] sarray;
    Inner[Inner] aarray;
    const(Inner[const(Inner)]) qualAarray;

    shared(immutable(Inner) delegate(ref double, scope string) const shared @trusted nothrow) attrDeleg;

    struct Data(T) { int x; }
    void tfunc(T...)(T args) {}

    template Inst(alias A) { int x; }

    class Test12309(T, int x, string s) {}
}

void symbols()
{
    alias qnTests = QualifiedNameTests;
    enum prefix = "test16495.QualifiedNameTests.";
    static assert(__traits(fullyQualifiedName, qnTests.Inner)           == prefix ~ "Inner");
    static assert(__traits(fullyQualifiedName, qnTests.func)            == prefix ~ "func");

    static assert(__traits(fullyQualifiedName, qnTests.Data!int)        == prefix ~ "Data!int.Data");
    static assert(__traits(fullyQualifiedName, qnTests.Data!int.x)      == prefix ~ "Data!int.Data.x");
    static assert(__traits(fullyQualifiedName, qnTests.tfunc!(int[]))   == prefix ~ "tfunc!(int[]).tfunc");
    static assert(__traits(fullyQualifiedName, qnTests.Inst!(Object))   == prefix ~ "Inst!(Object)");
    static assert(__traits(fullyQualifiedName, qnTests.Inst!(Object).x) == prefix ~ "Inst!(Object).x");
    static assert(__traits(fullyQualifiedName, qnTests.Test12309!(int, 10, "str"))
                                                == prefix ~ "Test12309!(int, 10, \"str\").Test12309");
}

void names()
{
    enum prefix = "test16495.QualifiedNameTests";
    enum xx = prefix ~ ".Inner";
    with (QualifiedNameTests)
    {
        // Basic qualified name
        static assert(__traits(fullyQualifiedName, Inner) == xx);

        // Array types
        static assert(__traits(fullyQualifiedName, typeof(array)) == xx ~ "[]");
        static assert(__traits(fullyQualifiedName, typeof(sarray)) == xx ~ "[16]");
        static assert(__traits(fullyQualifiedName, typeof(aarray)) == xx ~ "[" ~ xx ~ "]");

        // qualified key for AA
        static assert(__traits(fullyQualifiedName, typeof(qualAarray)) == "const(" ~ xx ~ "[const(" ~ xx ~ ")])");

        // Qualified composed data types
        static assert(__traits(fullyQualifiedName, typeof(data)) == "shared(const(" ~ xx ~ "[string])[])");

        // Function types + function attributes
        static assert(__traits(fullyQualifiedName, typeof(func)) == "ref const(" ~ xx ~ "[string])(ref " ~ xx ~ ", lazy scope string)");
        static assert(__traits(fullyQualifiedName, typeof(retfunc)) == "ref const(" ~ xx ~ "[string])(return ref " ~ xx ~ ")");
        static assert(__traits(fullyQualifiedName, typeof(inoutFunc)) == "inout "~xx~"(inout("~xx~"))");
        static assert(__traits(fullyQualifiedName, typeof(deleg)) == "const(" ~ xx ~ " delegate(double, string) nothrow @safe)");
        static assert(__traits(fullyQualifiedName, typeof(inoutDeleg)) == "inout(int) delegate(inout(int)) inout");
        static assert(__traits(fullyQualifiedName, typeof(funcPtr)) == "" ~ xx ~ " function(out double, string)");
        static assert(__traits(fullyQualifiedName, typeof(cFuncPtr)) == "extern (C) " ~ xx ~ " function(double, string)");

        // Delegate type with qualified function type
        static assert(__traits(fullyQualifiedName, typeof(attrDeleg)) == "shared(immutable(" ~ xx ~ ") "~
            "delegate(ref double, scope string) shared const nothrow @trusted)");

        // Variable argument function types
        static assert(__traits(fullyQualifiedName, typeof(cVarArg)) == "extern (C) void(int, ...)");
        static assert(__traits(fullyQualifiedName, typeof(dVarArg)) == "void(...)");
        static assert(__traits(fullyQualifiedName, typeof(dVarArg2)) == "void(int, ...)");
        static assert(__traits(fullyQualifiedName, typeof(typesafeVarArg)) == "void(int[]...)");

        // SIMD vector
        static if (is(__vector(float[4])))
        {
            static assert(__traits(fullyQualifiedName, __vector(float[4])) == "__vector(float[4])");
        }
    }
}
