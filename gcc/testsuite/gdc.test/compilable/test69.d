// PERMUTE_ARGS:

// ICE(expression.c) DMD 0.110
// https://www.digitalmars.com/d/archives/digitalmars/D/bugs/2966.html

string str255() { return "\255"; }
void fromFail49()
{
    switch("abc")
    {
	case "":
	case str255():
	    break;
    default:
        break;
    }
}

// https://issues.dlang.org/show_bug.cgi?id=5735

struct A {}
void b() {}

void foo(bool cond) {}

void main()
{
    A a;
    int i;

    static assert(!__traits(compiles, assert(a)));
    static assert(!__traits(compiles, assert(i || a)));
    static assert(!__traits(compiles, assert(0 || a)));
    static assert(!__traits(compiles, assert(i && a)));
    static assert(!__traits(compiles, assert(1 && a)));

    static assert(!__traits(compiles, foo(a)));
    static assert(!__traits(compiles, foo(i || a)));
    static assert(!__traits(compiles, foo(0 || a)));
    static assert(!__traits(compiles, foo(i && a)));
    static assert(!__traits(compiles, foo(1 && a)));

    static assert(!__traits(compiles, assert(b)));
    static assert(!__traits(compiles, assert(i || b)));
    static assert(!__traits(compiles, assert(0 || b)));
    static assert(!__traits(compiles, assert(i && b)));
    static assert(!__traits(compiles, assert(1 && b)));

    static assert(!__traits(compiles, foo(b)));
    static assert(!__traits(compiles, foo(i || b)));
    static assert(!__traits(compiles, foo(0 || b)));
    static assert(!__traits(compiles, foo(i && b)));
    static assert(!__traits(compiles, foo(1 && b)));
}
