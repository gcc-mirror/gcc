/* REQUIRED_ARGS: -preview=dip1000 -preview=in -mcpu=native
 */

import core.stdc.time;

void fun(in int* inParam) @safe;
static assert([__traits(getParameterStorageClasses, fun, 0)] == ["in"]);
static assert (is(typeof(fun) P == __parameters) && is(P[0] == const int*));

struct Foo
{
    int a;
    double[100] b;
}
void fun2(in Foo inParam) @safe;
static assert([__traits(getParameterStorageClasses, fun2, 0)] == ["in"]);
static assert (is(typeof(fun2) P == __parameters) && is(P[0] == const Foo));

void test()
{
    withDefaultValue(42);
    withDefaultValue();
    withDefaultRef(TimeRef.init);
    withDefaultRef();

    withInitDefaultValue();
    withInitDefaultRef();
}

struct FooBar
{
    string toString() const
    {
        string result;
        // Type is const
        this.toString((in char[] buf) {
            static assert(is(typeof(buf) == const(char[])));
            result ~= buf;
        });
        // Type inference works
        this.toString((in buf) { result ~= buf; });
        return result;
    }

    void toString(scope void delegate(in char[]) sink) const
    {
        sink("Hello world");
    }
}

// Ensure that default parameter works even if non CTFEable
void withDefaultValue(in time_t currTime = time(null)) {}
struct TimeRef { time_t now; ulong[4] bloat; }
void withDefaultRef(in TimeRef currTime = TimeRef(time(null))) {}

// Ensure that default parameters work with `.init`
void withInitDefaultValue(in size_t defVal = size_t.init) {}
void withInitDefaultRef(in TimeRef defVal = TimeRef.init) {}

// Ensure that temporary aren't needlessly created
// (if they are, it'll trigger the "goto skips declaration" error)
void checkNotIdentity(in void* p1, in void* p2) { assert(p1 !is p2); }
void checkTemporary()
{
    int* p = new int;
    if (p is null)
        goto LError;
    // Should not generate temporary, pass the pointers by value
    checkNotIdentity(/*lvalue*/ p, /*rvalue*/ null);
    checkNotIdentity(new int, null);
LError:
    assert(0);
}


// Some ABI-specific tests:

version (Win64)
{
    void checkReal(in real p)
    {
    }

    struct RGB { ubyte r, g, b; }
    void checkNonPowerOf2(in RGB p)
    {
    }
}
else version (X86_64) // Posix x86_64
{
    struct Empty {} // 1 dummy byte passed on the stack
    void checkEmptyStruct(in Empty p)
    {
    }

    static if (is(__vector(double[4])))
    {
        struct AvxVectorWrapper { __vector(double[4]) a; } // 256 bits
        void checkAvxVector(in AvxVectorWrapper p)
        {
        }
    }
}
else version (AArch64)
{
    alias HVA = __vector(float[4])[4]; // can be passed in 4 vector registers
    void checkHVA(in HVA p)
    {
    }
}
