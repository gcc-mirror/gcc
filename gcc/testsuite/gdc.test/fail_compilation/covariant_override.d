/++
https://issues.dlang.org/show_bug.cgi?id=21538

TEST_OUTPUT:
---
fail_compilation/covariant_override.d(25): Error: function `@safe void covariant_override.CI.f(void delegate() @safe dg)` does not override any function
fail_compilation/covariant_override.d(19):        did you mean to override `@safe void covariant_override.I.f(void delegate() @system dg)`?
fail_compilation/covariant_override.d(36): Error: function `@safe void covariant_override.CA.f(void delegate() @safe dg)` does not override any function
fail_compilation/covariant_override.d(30):        did you mean to override `@safe void covariant_override.A.f(void delegate() @system dg)`?
fail_compilation/covariant_override.d(22): Error: class `covariant_override.CI` interface function `void f(void delegate() @system dg) @safe` is not implemented
---
++/

static assert(!is(void delegate() @system : void delegate() @safe));
static assert( is(void delegate() @safe : void delegate() @system));

interface I
{
    void f(void delegate() @system dg) @safe;
}

class CI : I
{
    // this overrride should not be legal
    override void f(void delegate() @safe dg) @safe { }
}

abstract class A
{
    void f(void delegate() @system dg) @safe;
}

class CA : A
{
    // this overrride should not be legal
    override void f(void delegate() @safe dg) @safe { }
}
