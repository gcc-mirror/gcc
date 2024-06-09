// EXTRA_CPP_SOURCES: test24292.cpp

extern(C++) struct List(T)
{
    // Any of the following static ifs can trigger the problem.
    static if (T.sizeof > 4) {}
    static if (__traits(isZeroInit, T)) {}
    static if (__traits(isPOD, T)) {}

    T* begin;
}

extern(C++) struct StructWithDestructor
{
    ~this();

    alias L = List!StructWithDestructor;
    int i;
}

extern(C++) struct StructWithCopyCtor
{
    this(ref const(StructWithCopyCtor));

    alias L = List!StructWithCopyCtor;
    int i;
}

extern(D) struct StructWithPostblit
{
    this(this) {}

    alias L = List!StructWithPostblit;
    int i;
}

static assert(!__traits(isPOD, StructWithDestructor));
static assert(!__traits(isPOD, StructWithCopyCtor));
static assert(!__traits(isPOD, StructWithPostblit));

extern(C++) StructWithDestructor getStructWithDestructor();
extern(C++) StructWithCopyCtor getStructWithCopyCtor();

void main()
{
    StructWithDestructor structWithDestructor = getStructWithDestructor();
    assert(structWithDestructor.i == 12345);
    StructWithCopyCtor structWithCopyCtor = getStructWithCopyCtor();
    assert(structWithCopyCtor.i == 54321);
}
