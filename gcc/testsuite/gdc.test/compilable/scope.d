/*
REQUIRED_ARGS: -preview=dip1000
*/

struct Cache
{
    ubyte[1] v;

    ubyte[] set(ubyte[1] v) return
    {
        return this.v[] = v[];
    }
}

/*********************************/

// https://github.com/dlang/dmd/pull/9220

@safe:

struct OnlyResult
{
    private this(Values)(scope ref Values values)
    {
        this.s = values;
    }

    string s;
}

auto only(Values)(Values vv)
{
    return OnlyResult(vv);
}


void test() @nogc @safe pure
{
    only(null);
}

/************************************/

// https://github.com/dlang/dmd/pull/9220

auto callWrappedOops(scope string dArgs) {

    string callWrappedImpl() {
        return dArgs;
    }
}

/************************************/

struct Constant
{
    int* member;

    this(Repeat!(int*) grid) @safe
    {
        foreach(ref x; grid)
            member = x;

        foreach(ref x; grid)
            x = member;
    }

    int* foo(return scope Repeat!(int*) grid) @safe
    {
        foreach(ref x; grid)
            x = member;

        foreach(ref x; grid)
            return x;

        return null;
    }

    alias Repeat(T...) = T;
}


/************************************/

// https://issues.dlang.org/show_bug.cgi?id=19387

struct C
{
  void* u;
  this(this) @safe
  {
  }
}

struct S
{
  C c;
}

void foo(scope S s) @safe
{
}

void bar(scope S s) @safe
{
  foo(s);
}

/************************************/

// https://issues.dlang.org/show_bug.cgi?id=20675

struct D
{
    int pos;
    char* p;
}

void test(scope ref D d) @safe
{
    D[] da;
    da ~= D(d.pos, null);
}

/************************************/

void withEscapes()
{
    static D get() @safe;

    with (get())
    {
    }
}

/************************************/

// https://issues.dlang.org/show_bug.cgi?id=20682

int f1_20682(return scope ref D d) @safe
{
    return d.pos;
}

ref int f2_20682(return ref scope D d) @safe
{
    return d.pos;
}

void test_20682(scope ref D d) @safe
{
    int[] a;
    a ~= f1_20682(d);
    a ~= f2_20682(d);
    a ~= cast(int) d.p;
}

// Phobos failure
void formattedWrite(immutable char[2] args) @safe
{
    scope immutable char[] val = args;
}

void ctfeRead(const ubyte[2] array) @safe
{
    short result;

    foreach_reverse (b; array)
        result = cast(short) ((result << 8) | b);

    foreach (b; array)
        result = cast(short) ((result << 8) | b);
}

void demangle() @safe
{
    static struct DotSplitter
    {
        const(char)[] s;

        @safe:
        @property bool empty() const { return !s.length; }

        @property const(char)[] front() const return
        {
            immutable i = indexOfDot();
            return s;
        }

        void popFront() {}

        private ptrdiff_t indexOfDot() const
        {
            return -1;
        }
    }

    foreach (comp; DotSplitter(""))
    {
        const copy = comp;
    }
}

void fileCtor() @safe
{
    static struct S
    {
        int i;
    }

    // static S get()
    // {
    //     return S();
    // }

    with (S())
    {
        int* ptr = &i;
    }
}

// Missing test coverage
int*[4] testArray() @safe
{
    return typeof(return).init;
}

/************************************/

// https://issues.dlang.org/show_bug.cgi?id=21209
void testForeach(T)(const(T)[] ts)
{
    static int g;
    g++; // force impure for https://issues.dlang.org/show_bug.cgi?id=20150
    foreach (c; ts)
    {

    }
    foreach_reverse(c0; ts)
    {
        foreach(c1; ts)
        {

        }
    }
}

@safe
void main21209()
{
    char[10] cs;
    float[10] fs;
    testForeach(cs);
    testForeach(fs);
}

struct S23669
{
    string[] a;
    @safe void reserve() scope
    {
        a.length += 1;
    }
}

/************************************/

// Calling `hasPointers` in escape.d at some point caused
// a "circular `typeof` definition" error in std.typecons
// https://github.com/dlang/dmd/pull/16719
struct Unique
{
    int* p;
    alias ValueType = typeof({ return p; } ());
    static if (is(ValueType == int*)) {}
}

// Without handling tuple expansion in escape.d, this error occurs:
// Error: returning `(ref Tuple!(int, int) __tup2 = this.tuple.get();) , &__tup2.__expand_field_0` escapes a reference to local variable `__tup2`
struct Tuple(Types...)
{
    Types expand;
    ref Tuple!Types get() { return this; }
}

struct S2
{
    Tuple!(int, int) tuple;
    int* f() return { return &tuple.get().expand[0]; }
}

/************************************/

// https://issues.dlang.org/show_bug.cgi?id=23300

auto array(Range)(Range r)
{
    int[] result;
    foreach (e; r)
        result ~= e;
    return result;
}

struct Range
{
    int* ptr;
    int front = 3;
    enum empty = true;
    void popFront() @safe scope {}
}

auto f() @safe
{
    scope Range r;
    return r.array;
}

/************************************/

// Test that there's no endless loop forwarding `__appendtmp1 => __appendtmp1.this(null)`
struct SD
{
    int* p;
    this(int*) return scope {}
}

auto fsd() @safe
{
    SD[] a;
    a ~= SD(null);
}

/************************************/

// Test that `return scope` inference from assignment doesn't force the function to be `scope`
struct Scape
{
    int* p;

    this()(int* input)
    {
        p = input; // return scope inferred here
        notScope(); // not-scope inferred here
    }

    void notScope() @safe
    {
        static int* g;
        g = p;
    }
}

@safe testScape()
{
    Scape(null);
}

/************************************/

// Test `scope` inference in presence of nested function returning `this`:
// `save()` can still be called on a `scope Result`
struct Result
{
    int* source;
    auto save()
    {
        int* saveI() { return this.source; }
        auto saveResult = Result(saveI());
    }
}

@safe escapeNested()
{
    int s;
    auto r = Result(&s);
    r.save();
}
