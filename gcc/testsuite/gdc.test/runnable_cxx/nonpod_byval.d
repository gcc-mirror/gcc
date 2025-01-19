// EXTRA_CPP_SOURCES: cpp_nonpod_byval.cpp
// CXXFLAGS(linux osx freebsd dragonflybsd): -std=c++11

extern (C) int printf(const(char)*, ...);

extern (C++):

template Foo(T)
{
    T fooCpp(T param); // calls fooD() with a new, doubled literal

    T fooD(T param)
    {
        printf("fooD   %d [%p]\n", param.a, &param);
        assert(param.a == 2 * 123);
        static if (__traits(compiles, { T copy = param; }))
            return param; // invokes postblit
        else
            return T(param.a);
    }
}

void test(T)()
{
    printf(".: %.*s\n", cast(int) T.stringof.length, T.stringof.ptr);

    {
        auto result = fooCpp(T(123));
        assert(result.a == 246);
    }

    static if (__traits(hasMember, T, "numDtor"))
    {
        // fooCpp param + fooD param + result => 3 T instances.
        // There may be an additional destruction of the moved-from T literal
        // in fooCpp, depending on in-place construction vs. move.
        assert(T.numDtor == 3 || T.numDtor == 4);
    }
}

struct POD
{
    int a;
}
mixin Foo!POD;

struct CtorOnly
{
    int a;
    this(int a) { this.a = a; }
}
mixin Foo!CtorOnly;

struct DtorOnly
{
    static __gshared int numDtor = 0;
    int a;
    ~this() { printf("dtor   %d [%p]\n", a, &this); ++numDtor; }
}
mixin Foo!DtorOnly;

struct CtorDtor
{
    static __gshared int numDtor = 0;
    int a;
    this(int a) { this.a = a; }
    ~this() { printf("dtor   %d [%p]\n", a, &this); ++numDtor; }
}
mixin Foo!CtorDtor;

struct Copy
{
    static __gshared int numDtor = 0;
    int a;
    this(int a) { this.a = a; }
    ~this() { printf("dtor   %d [%p]\n", a, &this); ++numDtor; }
    this(this) { printf("post   %d [%p]\n", a, &this); }
}
mixin Foo!Copy;

struct CopyAndMove
{
    static __gshared int numDtor = 0;
    int a;
    this(int a) { this.a = a; }
    ~this() { printf("dtor   %d [%p]\n", a, &this); ++numDtor; }
    this(this) { printf("post   %d [%p]\n", a, &this); }
}
mixin Foo!CopyAndMove;

struct MoveOnly
{
    static __gshared int numDtor = 0;
    int a;
    this(int a) { this.a = a; }
    ~this() { printf("dtor   %d [%p]\n", a, &this); ++numDtor; }
    this(this) @disable;
}
mixin Foo!MoveOnly;

struct MemberWithCtor
{
    int a;
    CtorOnly m;
}
mixin Foo!MemberWithCtor;

void main()
{
    test!POD();
    test!CtorOnly();
    test!DtorOnly();
    test!CtorDtor();
    test!Copy();
    test!CopyAndMove();
    test!MoveOnly();
    test!MemberWithCtor();
}
