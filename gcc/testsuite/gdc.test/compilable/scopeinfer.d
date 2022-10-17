// PERMUTE_ARGS: -preview=dip1000

// Mangling should be the same with or without inference of `return scope`

@safe:

auto foo(void* p) { return 0; }
static assert(typeof(foo).mangleof == "FNaNbNiNfPvZi");

auto bar(void* p) { return p; }
static assert(typeof(bar).mangleof == "FNaNbNiNfPvZQd");

// https://issues.dlang.org/show_bug.cgi?id=19857

struct Stack()
{
@safe:
    int** data;
    ref int* top()
    {
        return *data;
    }
}

alias S = Stack!();

//pragma(msg, S.top.mangleof);

version (Win32)
static assert(S.top.mangleof == "_D10scopeinfer__T5StackZQh3topMFNaNbNcNiNfZPi");
