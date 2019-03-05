// PERMUTE_ARGS:
// REQUIRED_ARGS: -o- -X -Xf${RESULTS_DIR}/compilable/json.out
// POST_SCRIPT: compilable/extra-files/json-postscript.sh
// EXTRA_FILES: imports/jsonimport1.d imports/jsonimport2.d imports/jsonimport3.d imports/jsonimport4.d

module json;


static this() {}

static ~this() {}


alias int myInt;
myInt x; // bug 3404

struct Foo(T) { T t; }
class  Bar(int T) { int t = T; }
interface Baz(T...) { T[0] t() const; } // bug 3466

template P(alias T) {}

class Bar2 : Bar!1, Baz!(int, 2, null) {
    this() {}
    ~this() {} // bug 4178

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

/++
 + Documentation test
 +/
@trusted myInt bar(ref uint blah, Bar2 foo = new Bar3(7)) // bug 4477
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
body {
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
