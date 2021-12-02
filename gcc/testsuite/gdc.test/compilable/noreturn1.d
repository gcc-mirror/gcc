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

// Does not parse yet
// static assert(is( typeof(throw new Exception()) == noreturn ));

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

version (DigitalMars)
    noreturn exits(int* p) { *p = 3; }

noreturn exit();

noreturn pureexits() @nogc nothrow pure @safe { assert(0); }

noreturn callpureexits() { pureexits(); }

int test1(int i)
{
    if (exit())
        return i + 1;
    return i - 1;
}
