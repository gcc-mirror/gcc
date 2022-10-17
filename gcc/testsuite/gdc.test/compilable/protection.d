// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/protectionimp.d
import imports.protectionimp;

alias TypeTuple(T...) = T;

private
{
    void localF() {}
    class localC {}
    struct localS {}
    union localU {}
    interface localI {}
    enum localE { foo }
    mixin template localMT() {}

    class localTC(T) {}
    struct localTS(T) {}
    union localTU(T) {}
    interface localTI(T) {}
    void localTF(T)() {}
}

void main()
{
    // Private non-template declarations
    static assert(!__traits(compiles, privF()));
    static assert(!__traits(compiles, privC));
    static assert(!__traits(compiles, privS));
    static assert(!__traits(compiles, privU));
    static assert(!__traits(compiles, privI));
    static assert(!__traits(compiles, privE));
    static assert(!__traits(compiles, privMT));

    // Private local non-template declarations.
    static assert( __traits(compiles, localF()));
    static assert( __traits(compiles, localC));
    static assert( __traits(compiles, localS));
    static assert( __traits(compiles, localU));
    static assert( __traits(compiles, localI));
    static assert( __traits(compiles, localE));
    static assert( __traits(compiles, localMT));

    // Private template declarations.
    static assert(!__traits(compiles, privTF!int()));
    static assert(!__traits(compiles, privTC!int));
    static assert(!__traits(compiles, privTS!int));
    static assert(!__traits(compiles, privTU!int));
    static assert(!__traits(compiles, privTI!int));

    // Private local template declarations.
    static assert( __traits(compiles, localTF!int()));
    static assert( __traits(compiles, localTC!int));
    static assert( __traits(compiles, localTS!int));
    static assert( __traits(compiles, localTU!int));
    static assert( __traits(compiles, localTI!int));

    // Public template function with private type parameters.
    static assert(!__traits(compiles, publF!privC()));
    static assert(!__traits(compiles, publF!privS()));
    static assert(!__traits(compiles, publF!privU()));
    static assert(!__traits(compiles, publF!privI()));
    static assert(!__traits(compiles, publF!privE()));

    // Public template function with private alias parameters.
    static assert(!__traits(compiles, publFA!privC()));
    static assert(!__traits(compiles, publFA!privS()));
    static assert(!__traits(compiles, publFA!privU()));
    static assert(!__traits(compiles, publFA!privI()));
    static assert(!__traits(compiles, publFA!privE()));

    // Private alias.
    static assert(!__traits(compiles, privA));

    // Public template mixin.
    static assert( __traits(compiles, publMT));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14169

template staticMap14169(alias fun, T...)
{
    static if (T.length > 0)
        alias staticMap14169 = TypeTuple!(fun!(T[0]), staticMap14169!(fun, T[1..$]));
    else
        alias staticMap14169 = TypeTuple!();
}

class C14169
{
    private struct InnerStruct(string NameS)
    {
        alias Name = NameS;
    }
    alias DimensionNames = staticMap14169!(GetName14169, InnerStruct!"A");
}
