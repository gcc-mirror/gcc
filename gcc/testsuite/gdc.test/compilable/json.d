// PERMUTE_ARGS:
// REQUIRED_ARGS: -d -preview=dip1000 -o- -X -Xf-
// EXTRA_FILES: imports/jsonimport1.d imports/jsonimport2.d imports/jsonimport3.d imports/jsonimport4.d
// TRANSFORM_OUTPUT: sanitize_json
// TEST_OUTPUT_FILE: extra-files/json.json
module json;

shared static this() {}
static this() {}
shared static ~this() {}
static ~this() {}

template X(T)
{
    shared static this() {}
    static this() {}
    shared static ~this() {}
    static ~this() {}
}

alias SSCDX = X!int;

class SSCDClass
{
    shared static this() {}
    static this() {}
    shared static ~this() {}
    static ~this() {}
}

#line 17

alias int myInt;
myInt x; // https://issues.dlang.org/show_bug.cgi?id=3404

struct Foo(T) { T t; }
class  Bar(int T) { int t = T; }
interface Baz(T...) { T[0] t() const; } // https://issues.dlang.org/show_bug.cgi?id=3466

template P(alias T) {}

class Bar2 : Bar!1, Baz!(int, 2, null) {
    this() {}
    ~this() {} // https://issues.dlang.org/show_bug.cgi?id=4178

    static foo() {}
    protected abstract Foo!int baz();
    override int t() const { return 0; }
}

class Bar3 : Bar2 {
    private int val;
    this(int i) { val = i; }

    protected override Foo!int baz() { return Foo!int(val); }
}

struct Foo2 {
    Bar2 bar2;
    union U {
        struct {
            short s;
            int i;
        }
        Object o;
    }
}

struct Foo3(bool b) {
    version(D_Ddoc) {
        /// Doc 1
        void method1();
    }
    static if (b) {
        /// Doc 2
        void method2();
    } else {
        /// Doc 3
        void method3();
    }

    /// Doc 4
    void method4();
}

/++
 + Documentation test
 +/
@trusted myInt bar(ref uint blah, Bar2 foo = new Bar3(7)) // https://issues.dlang.org/show_bug.cgi?id=4477
{
    return -1;
}

@property int outer() nothrow
in {
    assert(true);
}
out(result) {
    assert(result == 18);
}
do {
    int x = 8;
    int inner(void* v) nothrow
    {
        int y = 2;
        assert(true);
        return x + y;
    }
    int z = inner(null);
    return x + z;
}

/** Issue 9484 - selective and renamed imports */
import imports.jsonimport1 : target1, target2;
import imports.jsonimport2 : alias1 = target1, alias2 = target2;
import imports.jsonimport3 : alias3 = target1, alias4 = target2, target3;
import imports.jsonimport4;

struct S
{
    /** Issue 9480 - Template name should be stripped of parameters */
    this(T)(T t) { }
}

/** Issue 9755 - Protection not emitted properly for Templates. */
private struct S1_9755(T) { }
package struct S2_9755(T) { }

class C_9755
{
    protected static class CI_9755(T) { }
}

/** Issue 10011 - init property is wrong for object initializer. */
const Object c_10011 = new Object();

///
enum Numbers
{
    unspecified1,
    one  = 2,
    two  = 3,
    FILE_NOT_FOUND = 101,
    unspecified3,
    unspecified4,
    four = 4,
}

template IncludeConstraint(T) if (T == string) {}

static foreach(enum i; 0..3)
{
    mixin("int a" ~ i.stringof ~ " = 1;");
}

alias Seq(T...) = T;

static foreach(int i, alias a; Seq!(a0, a1, a2))
{
	mixin("alias b" ~ i.stringof ~ " = a;");
}

// return ref, return scope, return ref scope
ref int foo(return ref int a) @safe
{
	return a;
}

int* foo(return scope int* a) @safe
{
	return a;
}

ref int* foo(scope return ref int* a) @safe
{
	return a;
}

struct SafeS
{
@safe:
    ref SafeS foo() return
    {
        return this;
    }

    SafeS foo2() return scope
    {
        return this;
    }

    ref SafeS foo3() return scope
    {
        return this;
    }

	int* p;
}

extern int vlinkageDefault;
extern(D) int vlinkageD;
extern(C) int vlinakgeC;
extern(C++) __gshared int vlinkageCpp;
extern(Windows) int vlinkageWindows;
extern(Objective-C) int vlinkageObjc;

extern int flinkageDefault();
extern(D) int flinkageD();
extern(C) int linakgeC();
extern(C++) int flinkageCpp();
extern(Windows) int flinkageWindows();
extern(Objective-C) int flinkageObjc();

mixin template test18211(int n)
{
    static foreach (i; 0 .. n>10 ? 10 : n)
    {
        mixin("enum x" ~ cast(char)('0' + i));
    }
    static if (true) {}
}

alias F = size_t function (size_t a);
