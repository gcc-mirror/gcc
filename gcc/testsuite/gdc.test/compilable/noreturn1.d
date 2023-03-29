/*
REQUIRED_ARGS: -w
TEST_OUTPUT:
---
noreturn
---

Basic properties and usage mentioned in the DIP:
https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1034.md
*/

alias noreturn = typeof(*null);
pragma(msg, noreturn);

static assert(!is(noreturn == void));

// Fails
// static assert(is( typeof([]) == noreturn[] ));
// static assert(is( typeof([][0]) == noreturn ));

static assert(is( typeof(assert(0)) == noreturn ));

static assert(is( typeof(throw new Exception("")) == noreturn ));

static assert(is(noreturn == noreturn));
static assert(!is(noreturn == const noreturn));
static assert(is(noreturn : const noreturn));

static assert(!is(noreturn == int));
static assert(is(noreturn : int));

// Covariance
static assert(is(noreturn[] : int[]));
static assert(is(noreturn* : int*));
static assert(is(noreturn function() : int function()));
static assert(is(noreturn delegate() : int delegate()));

// Reject inverse conversions
static assert(!is(int[]          : noreturn[]));
static assert(!is(int*           : noreturn*));
static assert(!is(int function() : noreturn function()));
static assert(!is(int delegate() : noreturn delegate()));

static assert(noreturn.mangleof == "Nn"); // Changed from b due to conflicts with bool
static assert(noreturn.sizeof == 0);
static assert(noreturn.alignof == 0);

static assert((noreturn*).sizeof == (int*).sizeof);
static assert((noreturn[]).sizeof == (int[]).sizeof);

static assert(is(typeof(noreturn.init) == noreturn));
static assert(is(typeof((const noreturn).init) == const noreturn));
static assert(is(typeof((immutable noreturn).init) == immutable noreturn));
static assert(is(typeof((shared noreturn).init) == shared noreturn));

version (DigitalMars)
    noreturn exits(int* p)
    {
        *p = 3;
        assert(false); // *p could be valid
    }

noreturn exit();

noreturn pureexits() @nogc nothrow pure @safe { assert(0); }

noreturn callpureexits() { pureexits(); }

noreturn returnExits()
{
    return pureexits();
}

void alsoExits()
{
    return assert(0);
}

int thisAlsoExits()
{
    return assert(0);
}

void cast_()
{
    noreturn n;
    int i = n;
}

int test1(int i)
{
    if (exit())
        return i + 1;
    return i - 1;
}

noreturn tlsNoreturn;
__gshared noreturn globalNoreturn;

template CreateTLS(A)
{
    A a;
}

void* useTls()
{
    alias Tnr = CreateTLS!noreturn;
    void* a1 = &Tnr.a;
    void* a2 = &tlsNoreturn;
    void* a3 = &globalNoreturn;
    return a1 < a2 ? a2 : a3;
}

/***************************************************/

noreturn testfn(noreturn function() fn)
{
    fn();
}

noreturn testdg(noreturn delegate() dg)
{
    dg();
}

noreturn func()
{
    while(1)
    {
    }
}
alias AliasSeq(T...) = T;
alias Types = AliasSeq!(bool, byte, ubyte, short, ushort, int, uint,
                        long, ulong, char, wchar, dchar, float, double,
                        real);
void noreturnImplicit()
{
    /*
        Testing both ways because, although the underlying table
        is symmetrical the code that calls into it may be buggy.
    */
    {
        int x = 2 + func();
        int y = func() + 2;
    }
    foreach(T; Types)
    {
        T value;
        auto x = value + throw new Exception("Hello");
        auto y = (throw new Exception("wow")) + value;
    }
}

// https://issues.dlang.org/show_bug.cgi?id=23549
int foo(int g = assert(0)) {
    return g;
}

// https://issues.dlang.org/show_bug.cgi?id=22587
int front(int param)
{
    return param ? 1 : assert(0);
}
