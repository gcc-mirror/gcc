template Info(T, int line) {
    static assert(__traits(getLinkage, T) == "C++");
    alias Info = void;
}

// Forward reference
static assert(__traits(getLinkage, Klass) == "C++");
alias info1 = Info!(Klass, __LINE__);

extern (C++) class Klass { void derp() {} }

// Backward reference
static assert(__traits(getLinkage, Klass) == "C++");
alias info2 = Info!(Klass, __LINE__);
