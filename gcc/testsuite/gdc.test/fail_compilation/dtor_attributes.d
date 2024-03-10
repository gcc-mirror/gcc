/*
Informative error messages if the compiler generated destructor overrides a user-defined one.

TEST_OUTPUT:
---
fail_compilation/dtor_attributes.d(118): Error: `pure` function `dtor_attributes.test1` cannot call impure destructor `dtor_attributes.Strict.~this`
fail_compilation/dtor_attributes.d(113):        generated `Strict.~this` is impure because of the following field's destructors:
fail_compilation/dtor_attributes.d(111):         - HasDtor member
fail_compilation/dtor_attributes.d(103):           impure `HasDtor.~this` is declared here
fail_compilation/dtor_attributes.d(118): Error: `@safe` function `dtor_attributes.test1` cannot call `@system` destructor `dtor_attributes.Strict.~this`
fail_compilation/dtor_attributes.d(113):        `dtor_attributes.Strict.~this` is declared here
fail_compilation/dtor_attributes.d(113):        generated `Strict.~this` is @system because of the following field's destructors:
fail_compilation/dtor_attributes.d(111):         - HasDtor member
fail_compilation/dtor_attributes.d(103):           @system `HasDtor.~this` is declared here
fail_compilation/dtor_attributes.d(118): Error: `@nogc` function `dtor_attributes.test1` cannot call non-@nogc destructor `dtor_attributes.Strict.~this`
fail_compilation/dtor_attributes.d(113):        generated `Strict.~this` is non-@nogc because of the following field's destructors:
fail_compilation/dtor_attributes.d(111):         - HasDtor member
fail_compilation/dtor_attributes.d(103):           non-@nogc `HasDtor.~this` is declared here
fail_compilation/dtor_attributes.d(118): Error: destructor `dtor_attributes.Strict.~this` is not `nothrow`
fail_compilation/dtor_attributes.d(113):        generated `Strict.~this` is not nothrow because of the following field's destructors:
fail_compilation/dtor_attributes.d(111):         - HasDtor member
fail_compilation/dtor_attributes.d(103):           not nothrow `HasDtor.~this` is declared here
fail_compilation/dtor_attributes.d(116): Error: function `dtor_attributes.test1` may throw but is marked as `nothrow`
---
*/
#line 100

struct HasDtor
{
    ~this() {}
}

// The user-defined dtor is overridden by a generated dtor calling both
// - HasDtor.~this
// - Strict.~this
struct Strict
{
    HasDtor member;

    ~this() pure nothrow @nogc @safe {}
}

void test1() pure nothrow @nogc @safe
{
    Strict s;
}

/*
Works for clases as well.

TEST_OUTPUT:
---
fail_compilation/dtor_attributes.d(209): Error: `pure` function `dtor_attributes.test2` cannot call impure destructor `dtor_attributes.StrictClass.~this`
fail_compilation/dtor_attributes.d(204):        generated `StrictClass.~this` is impure because of the following field's destructors:
fail_compilation/dtor_attributes.d(203):         - HasDtor member
fail_compilation/dtor_attributes.d(103):           impure `HasDtor.~this` is declared here
---
*/
#line 200

class StrictClass
{
    HasDtor member;
    ~this() pure {}
}

void test2() pure
{
    scope instance = new StrictClass();
}

/*
Ignores members whose destructors are not called.

TEST_OUTPUT:
---
fail_compilation/dtor_attributes.d(321): Error: `pure` function `dtor_attributes.test3` cannot call impure destructor `dtor_attributes.StrictStructRef.~this`
fail_compilation/dtor_attributes.d(316):        generated `StrictStructRef.~this` is impure because of the following field's destructors:
fail_compilation/dtor_attributes.d(310):         - HasDtor structMember
fail_compilation/dtor_attributes.d(103):           impure `HasDtor.~this` is declared here
---
*/
#line 300

class HasDtorClass
{
    ~this() {}
}

struct Empty {}

struct StrictStructRef
{
    HasDtor structMember;
    HasDtorClass classMember;
    int intMember;
    int[2] arrayMember;
    Empty e;

    ~this() pure {}
}

void test3() pure
{
    StrictStructRef structInstance;
}

/*
Types from nested types work as well.

TEST_OUTPUT:
---
fail_compilation/dtor_attributes.d(411): Error: `pure` function `dtor_attributes.test4` cannot call impure destructor `dtor_attributes.StrictNested.~this`
fail_compilation/dtor_attributes.d(406):        generated `StrictNested.~this` is impure because of the following field's destructors:
fail_compilation/dtor_attributes.d(403):         - HasDtor[4] arrayMember
fail_compilation/dtor_attributes.d(103):           impure `HasDtor.~this` is declared here
---
*/
#line 400

struct StrictNested
{
    HasDtor[4] arrayMember;
    HasDtorClass[4] classMember;

    ~this() pure {}
}

void test4() pure
{
    StrictNested structInstance;
}

/*
Ignores member destructors when the user-defined one is permissive enough (e.g. both impure)

TEST_OUTPUT:
---
fail_compilation/dtor_attributes.d(509): Error: `pure` function `dtor_attributes.test5` cannot call impure destructor `dtor_attributes.Permissive.~this`
---
*/
#line 500

struct Permissive
{
    HasDtor[4] arrayMember;
    ~this() {}
}

void test5() pure
{
    Permissive structInstance;
}

/*
Works with destructors generated through multiple layers

TEST_OUTPUT:
---
fail_compilation/dtor_attributes.d(618): Error: `pure` function `dtor_attributes.test6` cannot call impure destructor `dtor_attributes.HasNestedDtor3.~this`
fail_compilation/dtor_attributes.d(611):        generated `HasNestedDtor3.~this` is impure because of the following field's destructors:
fail_compilation/dtor_attributes.d(613):         - HasNestedDtor2 member3
fail_compilation/dtor_attributes.d(606):        generated `HasNestedDtor2.~this` is impure because of the following field's destructors:
fail_compilation/dtor_attributes.d(608):         - HasNestedDtor1 member2
fail_compilation/dtor_attributes.d(601):        generated `HasNestedDtor1.~this` is impure because of the following field's destructors:
fail_compilation/dtor_attributes.d(603):         - HasDtor member1
fail_compilation/dtor_attributes.d(103):           impure `HasDtor.~this` is declared here
---
*/
#line 600

struct HasNestedDtor1
{
    HasDtor member1;
}

struct HasNestedDtor2
{
    HasNestedDtor1 member2;
}

struct HasNestedDtor3
{
    HasNestedDtor2 member3;
}

void test6() pure
{
    HasNestedDtor3 instance;
}
