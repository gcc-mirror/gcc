// PERMUTE_ARGS:
// POST_SCRIPT: runnable/extra-files/statictor-postscript.sh

private import std.stdio;

class Foo
{
        static this() {printf("Foo static ctor\n");}
        static ~this() {printf("Foo static dtor\n");}
}

static this() {printf("static ctor\n");}
static ~this() {printf("static dtor\n");}

shared static this()
{
    printf("shared static this()\n");
}

shared static ~this()
{
    printf("shared static this()\n");
}

class Bar
{
        static this() {printf("Bar static ctor\n");}
        static ~this() {printf("Bar static dtor\n");}
}

/***********************************************/
// 6677

int global6677;

static this() nothrow pure @safe
{
    int* p;
    static assert(!__traits(compiles, ++p));
    static assert(!__traits(compiles, ++global6677));
    auto throwit = { throw new Exception("sup"); };
    static assert(!__traits(compiles, throwit() ));
}

shared static this() nothrow pure @safe
{
    int* p;
    static assert(!__traits(compiles, ++p));
    static assert(!__traits(compiles, ++global6677));
}

/***********************************************/
// 7533
struct Foo7533(int n)
{
    pure static this() { }
}

alias Foo7533!5 Bar7533;

/***********************************************/

int main()
{
    return 0;
}

