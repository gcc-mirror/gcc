
/*
TEST_OUTPUT:
---
fail_compilation/parseStc4.d(14): Deprecation: redundant attribute `pure`
fail_compilation/parseStc4.d(14): Deprecation: redundant attribute `nothrow`
fail_compilation/parseStc4.d(14): Error: conflicting attribute `@system`
fail_compilation/parseStc4.d(14): Deprecation: redundant attribute `@nogc`
fail_compilation/parseStc4.d(14): Deprecation: redundant attribute `@property`
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
fail_compilation/parseStc4.d(34): Deprecation: redundant attribute `const`
fail_compilation/parseStc4.d(35): Deprecation: redundant attribute `const`
fail_compilation/parseStc4.d(36): Deprecation: redundant attribute `const`
fail_compilation/parseStc4.d(38): Deprecation: redundant attribute `pure`
fail_compilation/parseStc4.d(39): Deprecation: redundant attribute `@safe`
fail_compilation/parseStc4.d(40): Deprecation: redundant attribute `nothrow`
fail_compilation/parseStc4.d(41): Error: conflicting attribute `@trusted`
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
