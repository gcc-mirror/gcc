/*
  TEST_OUTPUT:
  ---
fail_compilation/fail20609.d(26): Error: none of the overloads of `this` are callable using argument types `(int)`
fail_compilation/fail20609.d(23):        Candidate is: `fail20609.Foo.this(string[] args)`
fail_compilation/fail20609.d(27): Error: none of the overloads of `this` are callable using argument types `(int)`
fail_compilation/fail20609.d(22):        Candidates are: `fail20609.Foo.this(Object _param_0)`
fail_compilation/fail20609.d(23):                        `fail20609.Foo.this(string[] args)`
fail_compilation/fail20609.d(37): Error: none of the overloads of `this` are callable using argument types `(int)`
fail_compilation/fail20609.d(37):        All possible candidates are marked as `deprecated` or `@disable`
fail_compilation/fail20609.d(43): Error: undefined identifier `deprecatedTypo_`
fail_compilation/fail20609.d(44): Error: undefined identifier `deprecatedTypo_`, did you mean function `deprecatedTypo`?
fail_compilation/fail20609.d(45): Error: undefined identifier `disabledTypo_`
---
 */

// Only show `this(string[])` in non-deprecated context.
// Show both `this(string[])` and ` this(Object)` in deprecated context.
struct Foo
{
    @disable this();
    deprecated this(Object) {}
    this(string[] args) {}
}

void test1() { auto f = Foo(42); }
deprecated void test2() { auto f = Foo(42); }

// Make sure we do not show a message promising candidates,
// then no candidates in the special case where nothing
// would be usable
struct WhoDoesThat
{
    @disable this();
    deprecated this(Object) {}
}
void test3() { auto f = WhoDoesThat(42); }

// Make sure we don't suggest disabled or deprecated functions
deprecated void deprecatedTypo () {}
@disable   void disabledTypo   () {}

void test4 () { deprecatedTypo_("42"); }
deprecated void test5 () { deprecatedTypo_("42"); }
void test6 () { disabledTypo_("42"); }
