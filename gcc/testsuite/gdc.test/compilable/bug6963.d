// PERMUTE_ARGS:
// REQUIRED_ARGS:

/*
TEST_OUTPUT:
---
output foo: 1e: pure nothrow @nogc @safe void(int x)
output foo: 3e: pure nothrow @nogc @safe void(int x)
---
*/

alias void function(int) pure nothrow @safe @nogc FuncPtrType;

void foo1a(X)(X x) {}
void foo1b(X)(X x) {}
void foo1c(X)(X x) {}
void foo1d(X)(X x) {}
void foo1e(X)(X x) {}

// module level declaration with type inference
auto fptr1 = &foo1a!int;
static assert(is(typeof(fptr1) == FuncPtrType));

// array initializer
auto fptrlist1 = [&foo1b!int];
static assert(is(typeof(fptrlist1) == FuncPtrType[]));

// static assert
static assert(is(typeof(&foo1c!int) == FuncPtrType));

// static if
static if(is(typeof(&foo1d!int) PF))
    static assert(is(PF == FuncPtrType));
else
    static assert(0);

// pragma test
pragma(msg, "output foo: 1e: ", typeof(foo1e!int).stringof);

void foo2a(X)(X x) {}
void foo2b(X)(X x) {}
void foo2c(X)(X x) {}

FuncPtrType fptr3 = &foo2a!int;     // most similar to original issue

FuncPtrType[] fptrlist3 = [&foo2b!int];

struct S{ FuncPtrType fp; }
S s = { &foo2c!int };

void foo3a(X)(X x) {}
void foo3b(X)(X x) {}
void foo3c(X)(X x) {}
void foo3d(X)(X x) {}
void foo3e(X)(X x) {}

void main()
{
    auto fptr2 = &foo3a!int;
    static assert(is(typeof(fptr2) == FuncPtrType));

    auto fptrlist2 = [&foo3b!int];
    static assert(is(typeof(fptrlist2) == FuncPtrType[]));

    static assert(is(typeof(&foo1c!int) == FuncPtrType));

    static if(is(typeof(&foo1d!int) PF))
        static assert(is(PF == FuncPtrType));
    else
        static assert(0);

    pragma(msg, "output foo: 3e: ", typeof(foo3e!int));
}
