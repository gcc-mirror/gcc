// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=94777
// { dg-additional-options "-fmain -funittest" }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void testVariadic(T)(int nargs, ...)
{
    import core.stdc.stdarg;
    foreach(i; 0 .. nargs)
    {
        auto arg = va_arg!T(_argptr);
        static if (__traits(compiles, arg.value))
        {
            assert(arg.value == i);
        }
        else static if (__traits(compiles, arg[0]))
        {
            foreach (value; arg)
                assert(value == i);
        }
        else
        {
            assert(arg == T.init);
        }
    }
}

/******************************************/

struct Constructor
{
    static int count;
    int value;
    this(int v) { count++; this.value = v; }
}

unittest
{
    auto a0 = Constructor(0);
    auto a1 = Constructor(1);
    auto a2 = Constructor(2);
    testVariadic!Constructor(3, a0, a1, a2);
    assert(Constructor.count == 3);
}

/******************************************/

struct Postblit
{
    static int count = 0;
    int value;
    this(this) { count++; }
}

unittest
{
    auto a0 = Postblit(0);
    auto a1 = Postblit(1);
    auto a2 = Postblit(2);
    testVariadic!Postblit(3, a0, a1, a2);
    assert(Postblit.count == 3);
}

/******************************************/

struct Destructor
{
    static int count = 0;
    int value;
    ~this() { count++; }
}

unittest
{
    {
        auto a0 = Destructor(0);
        auto a1 = Destructor(1);
        auto a2 = Destructor(2);
        static assert(!__traits(compiles, testVariadic!Destructor(3, a0, a1, a2)));
    }
    assert(Destructor.count == 3);
}

/******************************************/

struct CopyConstructor 
{
    static int count = 0;
    int value;
    this(int v) { this.value = v; }
    this(ref typeof(this) other) { count++; this.value = other.value; }
}

unittest
{
    auto a0 = CopyConstructor(0);
    auto a1 = CopyConstructor(1);
    auto a2 = CopyConstructor(2);
    testVariadic!CopyConstructor(3, a0, a1, a2);
    // NOTE: Cpctors are not implemented yet.
    assert(CopyConstructor.count == 0 || CopyConstructor.count == 3);
}

/******************************************/

unittest
{
    struct Nested
    {
        int value;
    }

    auto a0 = Nested(0);
    auto a1 = Nested(1);
    auto a2 = Nested(2);
    testVariadic!Nested(3, a0, a1, a2);
}

/******************************************/

unittest
{
    struct Nested2
    {
        int value;
    }

    void testVariadic2(int nargs, ...)
    {
        import core.stdc.stdarg;
        foreach(i; 0 .. nargs)
        {
            auto arg = va_arg!Nested2(_argptr);
            assert(arg.value == i);
        }
    }

    auto a0 = Nested2(0);
    auto a1 = Nested2(1);
    auto a2 = Nested2(2);
    testVariadic2(3, a0, a1, a2);
}

/******************************************/

struct EmptyStruct
{
}

unittest
{
    auto a0 = EmptyStruct();
    auto a1 = EmptyStruct();
    auto a2 = EmptyStruct();
    testVariadic!EmptyStruct(3, a0, a1, a2);
}

/******************************************/

alias StaticArray = int[4];

unittest
{
    StaticArray a0 = 0;
    StaticArray a1 = 1;
    StaticArray a2 = 2;
    // XBUG: Front-end rewrites static arrays as dynamic arrays.
    //testVariadic!StaticArray(3, a0, a1, a2);
}

/******************************************/

alias EmptyArray = void[0];

unittest
{
    auto a0 = EmptyArray.init;
    auto a1 = EmptyArray.init;
    auto a2 = EmptyArray.init;
    testVariadic!EmptyArray(3, a0, a1, a2);
}
