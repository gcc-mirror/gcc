/*
TEST_OUTPUT:
---
fail_compilation/ice18753.d(21): Error: variable `ice18753.isInputRange!(Group).isInputRange` type `void` is inferred from initializer `ReturnType(func...)`, and variables cannot be of type `void`
fail_compilation/ice18753.d(23): Error: template instance `ice18753.isInputRange!(Group)` error instantiating
fail_compilation/ice18753.d(18):        instantiated from here: `isForwardRange!(Group)`
fail_compilation/ice18753.d(18):        while evaluating: `static assert(isForwardRange!(Group))`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18753

struct ChunkByImpl
{
    struct Group
    { }
    
    static assert(isForwardRange!Group);
}

enum isInputRange(R) = ReturnType;

enum isForwardRange(R) = isInputRange!R is ReturnType!(() => r);

template ReturnType(func...)
{
    static if (is(FunctionTypeOf!func R == return))
        ReturnType R;
}

template FunctionTypeOf(func...)
{
    static if (is(typeof(func[0]) T))
        static if (is(T Fptr ) )
            alias FunctionTypeOf = Fptr;
}

template Select()
{ }
