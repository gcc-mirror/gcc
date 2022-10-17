/**
Checks that code still compiles when -preview=dtorfields is enabled by default
but issues an appropriate deprecation message.

Remove this test when the deprecations period ends, see visit(CtorDeclaration)
in semantic3.d

TEST_OUTPUT:
---
compilable/dtorfields_deprecation.d(30): Deprecation: `dtorfields_deprecation.Pure.this` has stricter attributes than its destructor (`pure`)
compilable/dtorfields_deprecation.d(30):        The destructor will be called if an exception is thrown
compilable/dtorfields_deprecation.d(30):        Either make the constructor `nothrow` or adjust the field destructors
compilable/dtorfields_deprecation.d(42): Deprecation: `dtorfields_deprecation.NoGC.this` has stricter attributes than its destructor (`@nogc`)
compilable/dtorfields_deprecation.d(42):        The destructor will be called if an exception is thrown
compilable/dtorfields_deprecation.d(42):        Either make the constructor `nothrow` or adjust the field destructors
compilable/dtorfields_deprecation.d(48): Deprecation: `dtorfields_deprecation.Safe.this` has stricter attributes than its destructor (`@system`)
compilable/dtorfields_deprecation.d(48):        The destructor will be called if an exception is thrown
compilable/dtorfields_deprecation.d(48):        Either make the constructor `nothrow` or adjust the field destructors
---
**/

struct HasDtor
{
    ~this() {}
}

struct Pure
{
    HasDtor member;
    this(int) pure {}
}

struct Nothrow
{
    HasDtor member;
    this(int) nothrow {}
}

struct NoGC
{
    HasDtor member;
    this(int) @nogc {}
}

struct Safe
{
    HasDtor member;
    this(int) @safe {}
}
