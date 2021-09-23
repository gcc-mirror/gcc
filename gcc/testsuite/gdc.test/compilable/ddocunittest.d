// PERMUTE_ARGS: -unittest
// REQUIRED_ARGS: -D -w -o- -c -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh unittest

module ddocunittest;

/* Insert test-cases for documented unittests feature here. */

/// foo function - 1 example
int foo(int a, int b) { return a + b; }

///
unittest
{
    assert(foo(1, 1) == 2);
}

/// bar function - 1 example
bool bar() { return true; }

///
unittest
{
    // documented
    assert(bar());
}

/// placeholder
unittest
{
}

/// doo function - no examples
void doo() { }

///
private unittest
{
    // undocumented
    doo();
}

unittest
{
    // undocumented
    doo();
}

/**
add function - 3 examples

Examples:

----
assert(add(1, 1) == 2);
----
*/
int add(int a, int b) { return a + b; }

///
unittest
{
    // documented
    assert(add(3, 3) == 6);
    assert(add(4, 4) == 8);
}

unittest
{
    // undocumented
    assert(add(2, 2) + add(2, 2) == 8);
}

///
unittest
{
    // documented
    assert(add(5, 5) == 10);
    assert(add(6, 6) == 12);
}

/// class Foo
immutable pure nothrow class Foo
{
    int x;

    ///
    unittest
    {
        // another foo example
        Foo foo = new Foo;
    }
}

///
unittest
{
    Foo foo = new Foo;
}

pure
{
    const
    {
        immutable
        {
            /// some class - 1 example
            class SomeClass {}
        }
    }
}

///
unittest
{
    SomeClass sc = new SomeClass;
}

/// Outer - 1 example
class Outer
{
    /// Inner
    static class Inner
    {
    }

    ///
    unittest
    {
        Inner inner = new Inner;
    }
}

///
unittest
{
    Outer outer = new Outer;
}

/** foobar - no examples */
void foobar()
{
}

unittest
{
    foobar();
}

/**
func - 4 examples
Examples:
---
foo(1);
---

Examples:
---
foo(2);
---
*/
void foo(int x) {  }

///
unittest
{
    foo(2);
}

///
unittest
{
    foo(4);
}

// ------------------------------------
// insert import declaration between documented function and unittests

///
void fooImport() {}
import core.stdc.stdio;
/// test
unittest { fooImport(); }

///
void fooStaticImport() {}
static import core.stdc.stdlib;
/// test
unittest { fooStaticImport(); }

///
void fooPublicImport() {}
public import core.stdc.string;
/// test
unittest { fooPublicImport(); }

///
void fooSelectiveImport() {}
import core.stdc.ctype : isalpha;
/// test
unittest { fooSelectiveImport(); }

///
void fooRenamedImport() {}
import io = core.stdc.stdio;
/// test
unittest { fooRenamedImport(); }

// ------------------------------------
// documented unittest after conditional declarations

static if (true)
  void fooConditionalDecl1a() {} /** */
unittest { int x1a; }   ///

static if (true)
{ void fooConditionalDecl1b() {} /** */ }
unittest { int x1b; }   ///

static if (false)
  void fooConditionalDecl2a() {} /** */
unittest { int x2a; }   ///

static if (false)
{ void fooConditionalDecl2b() {} /** */ }
unittest { int x2b; }   ///

static if (true)
{ void fooConditionalDecl3a() {} /** */ }
else
{ void barConditionalDecl3a() {} /** */ }
unittest { int x3a; }   ///

static if (true)
{ void fooConditionalDecl3b() {} /** */ }
else
{ void barConditionalDecl3b() {} /** */ }
unittest { int x3b; }   ///

static if (false)
  void fooConditionalDecl4a() {} /** */
else
  void barConditionalDecl4a() {} /** */
unittest { int x4a; }   ///

static if (false)
{ void fooConditionalDecl4b() {} /** */ }
else
{ void barConditionalDecl4b() {} /** */ }
unittest { int x4b; }   ///

static if (true)
{}
else
  void barConditionalDecl5a() {} /** */
unittest { int x5a; }   ///

static if (true)
{}
else
{ void barConditionalDecl5b() {} /** */ }
unittest { int x5b; }   ///

static if (false)
{}
else
  void barConditionalDecl6a() {} /** */
///
unittest { int x6a; }

static if (false)
{}
else
{ void barConditionalDecl6b() {} /** */ }
///
unittest { int x6b; }

// ------------------------------------
// 9474

///
void foo9474() { }

version(none)
unittest { }

/// Example
unittest { foo9474(); }

/// doc
void bar9474() { }

version(none)
unittest { }

/// Example
unittest { bar9474(); }

///
struct S9474
{
}
///
unittest { S9474 s; }

///
auto autovar9474 = 1;
///
unittest { int v = autovar9474; }

///
auto autofun9474() { return 1; }
///
    unittest { int n = autofun9474(); }

///
template Template9474()
{
    /// Shouldn't link following unittest to here
    void foo() {}
}
///
unittest { alias Template9474!() T; }

// ------------------------------------
// 9713

///
void fooNoDescription() {}
///
unittest { fooNoDescription(); }
///
unittest { if (true) {fooNoDescription(); } /* comment */ }

// ------------------------------------

/// test for bugzilla 9757
void foo9757() {}
/// ditto
void bar9757() {}
/// ditto
void baz9757() {}
///
unittest { foo9757(); bar9757(); }
///
unittest { bar9757(); foo9757(); }

/// with template functions
auto redBlackTree(E)(E[] elems...)
{
    return 1;
}
/// ditto
auto redBlackTree(bool allowDuplicates, E)(E[] elems...)
{
    return 2;
}
/// ditto
auto redBlackTree(alias less, E)(E[] elems...)
if (__traits(compiles, (E a, E b) => mixin(less)))
{
    return 3;
}
///
unittest
{
    auto rbt1 = redBlackTree(0, 1, 5, 7);
    auto rbt2 = redBlackTree!string("hello", "world");
    auto rbt3 = redBlackTree!true(0, 1, 5, 7, 5);
    auto rbt4 = redBlackTree!"a > b"(0, 1, 5, 7);
}

// ------------------------------------
// Issue 9758

/// test
void foo(){}

///
unittest {  }

// ------------------------------------
// Issue 10519

///
bool balancedParens10519(string, char, char) { return true; }
///
unittest
{
    auto s = "1 + (2 * (3 + 1 / 2)";
    assert(!balancedParens10519(s, '(', ')'));
}

// ------------------------------------
// Issue 12097

/// declaration
struct S12097
{
    /// method
    void foo() {}
}

/// ditto
void f12097() {}

/// ddoc code 1
unittest
{
    int a = 1;
}

/// ditto
struct T12097(T) {}

/// ddoc code 2
unittest
{
    int[] arr;
}

// ------------------------------------
// 14594

/*******************
 * testA
 */
void fun14594a()() {}
///
unittest { fun14594a(); }

/*******************
 * testB
 */
void fun14594b()() {}
/// ditto
void fun14594b(T)(T) {}
///
unittest { fun14594b(); fun14594b(1); }

/*******************
 * testC
 */
void fun14594c()() {}
///
unittest { fun14594c(); fun14594c(1); }
/// ditto
void fun14594c(T)(T) {}

/*******************
 * testD
 */
void fun14594d()() {}
///
unittest { fun14594d(); }
/// ditto
void fun14594d(T)(T) {}
///
unittest { fun14594d(1); }

/*******************
 * testE
 */
template fun14594e()
{
    /// concatenated doc-comment fun14594e
    void fun14594e() {}
    /// ignored-unittest fun14594e
    unittest { fun14594e(); }
}
/// doc-unittest fun14594e
unittest { fun14594e(); }

/*******************
 * testF
 */
template fun14594f()
{
    /// concatenated doc-comment fun14594f
    void fun14594f() {}
    /// ignored-unittest fun14594f
    unittest { fun14594f(); }
}
/// ditto
template fun14594f(T)
{
    /// ignored doc-comment fun14594f
    void fun14594f(T) {}
    /// ignored-unittest fun14594f
    unittest { fun14594f(1); }
}
/// doc-unittest fun14594f
unittest { fun14594f(); }

// ------------------------------------

void main() { }
