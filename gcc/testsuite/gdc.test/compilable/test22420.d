// https://issues.dlang.org/show_bug.cgi?id=22420

struct File
{
    ~this()
    {
    }
    File impl()
    {
        return File.init;
    }
    alias impl this;
}
struct Variable
{
    this(File)(File) { }
    this(File)(File[]) { }
}
Variable wrapFunctionReturn(alias handler)(Variable params)
{
    return Variable(handler(params));
}
void registerFile()
{
    wrapFunctionReturn!((Variable) {
            return File.init;
        })(Variable.init);
}

// Reduction from an 'automem' test

struct Issue156 {}

void test2()
{
    RefCounted!Issue156 s;
    auto r1 = repeat(s);
    zip(r1);
}

struct RefCounted(RefCountedType)
{
    ~this() {}
    alias _impl this;

    struct Impl {}
    alias ImplType = Impl;

    private ImplType* _impl;

}
template Tuple(Specs)
{
    struct Tuple
    {
        this(U)(U) {}
        this()(int) {}
    }
}

template ElementType(R)
{
    static if (is(typeof(R.init) T))
        alias ElementType = T;
}

struct Repeat(T)
{
    inout(T) front() inout {assert(0);}
}

Repeat!T repeat(T)(T ) {assert(0);}

auto zip(Ranges)(Ranges )
{
    return ZipShortest!Ranges();
}

struct ZipShortest(Ranges...)
{
    Ranges ranges;
    alias ElementType = Tuple!(.ElementType!(Ranges[0]));

    ElementType front()
    {
        return typeof(return)(ranges[0].front);
    }
}
