/*
Informative error messages if the compiler inserted an optional destructor call into the constructor.

REQUIRED_ARGS: -preview=dtorfields
TEST_OUTPUT:
---
fail_compilation/dtorfields_attributes.d(117): Error: `pure` constructor `dtorfields_attributes.Strict.this` cannot call impure destructor `dtorfields_attributes.Strict.~this`
fail_compilation/dtorfields_attributes.d(119):        generated `Strict.~this` is impure because of the following field's destructors:
fail_compilation/dtorfields_attributes.d(115):         - HasDtor member
fail_compilation/dtorfields_attributes.d(103):           impure `HasDtor.~this` is declared here
fail_compilation/dtorfields_attributes.d(117): Error: `@safe` constructor `dtorfields_attributes.Strict.this` cannot call `@system` destructor `dtorfields_attributes.Strict.~this`
fail_compilation/dtorfields_attributes.d(103):        which calls `dtorfields_attributes.HasDtor.~this`
fail_compilation/dtorfields_attributes.d(119):        `dtorfields_attributes.Strict.~this` is declared here
fail_compilation/dtorfields_attributes.d(119):        generated `Strict.~this` is @system because of the following field's destructors:
fail_compilation/dtorfields_attributes.d(115):         - HasDtor member
fail_compilation/dtorfields_attributes.d(103):           @system `HasDtor.~this` is declared here
fail_compilation/dtorfields_attributes.d(117): Error: `@nogc` constructor `dtorfields_attributes.Strict.this` cannot call non-@nogc destructor `dtorfields_attributes.Strict.~this`
fail_compilation/dtorfields_attributes.d(119):        generated `Strict.~this` is non-@nogc because of the following field's destructors:
fail_compilation/dtorfields_attributes.d(115):         - HasDtor member
fail_compilation/dtorfields_attributes.d(103):           non-@nogc `HasDtor.~this` is declared here
---
*/
#line 100

struct HasDtor
{
    ~this()
    {
        // Enforce @system, ... just to be sure
        __gshared int i;
        if (++i)
            throw new Exception(new immutable(char)[](10));
    }
}

// The user-defined dtor matches the ctor attributes
struct Strict
{
    HasDtor member;

    this(int) pure @nogc @safe {} // nothrow doesn't generate dtor call

    ~this() pure @nogc @safe {}
}
