
/*
TEST_OUTPUT:
---
fail_compilation/parseStc4.d(14): Error: redundant attribute `pure`
fail_compilation/parseStc4.d(14): Error: redundant attribute `nothrow`
fail_compilation/parseStc4.d(14): Error: conflicting attribute `@system`
fail_compilation/parseStc4.d(14): Error: redundant attribute `@nogc`
fail_compilation/parseStc4.d(14): Error: redundant attribute `@property`
---
*/
pure nothrow @safe   @nogc @property
int foo()
pure nothrow @system @nogc @property
{
    return 0;
}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc4.d(35): Error: redundant attribute `const`
fail_compilation/parseStc4.d(36): Error: redundant attribute `const`
fail_compilation/parseStc4.d(36): Deprecation: `const` postblit is deprecated. Please use an unqualified postblit.
fail_compilation/parseStc4.d(37): Error: redundant attribute `const`
fail_compilation/parseStc4.d(39): Error: redundant attribute `pure`
fail_compilation/parseStc4.d(40): Error: redundant attribute `@safe`
fail_compilation/parseStc4.d(41): Error: redundant attribute `nothrow`
fail_compilation/parseStc4.d(42): Error: conflicting attribute `@trusted`
---
*/

struct S
{
    const this(int) const {}
    const this(this) const {}
    const ~this() const {}

    pure static this() pure {}
    @safe static ~this() @safe {}
    nothrow shared static this() nothrow {}
    @system shared static ~this() @trusted {}
}
