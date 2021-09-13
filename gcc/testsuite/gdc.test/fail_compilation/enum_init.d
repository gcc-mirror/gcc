/*
https://issues.dlang.org/show_bug.cgi?id=8511

TEST_OUTPUT:
---
fail_compilation/enum_init.d(5): Error: type `SQRTMAX` has no value
---
*/
#line 1

real hypot()
{
    enum SQRTMAX;
    SQRTMAX/2;
}

/*
https://issues.dlang.org/show_bug.cgi?id=21785

TEST_OUTPUT:
---
fail_compilation/enum_init.d(106): Error: enum `enum_init.NoBase` is opaque and has no default initializer
---
*/
#line 100

enum NoBase;

void fooNB()
{
	NoBase nbv = void;
	NoBase nb;
}

/*
https://issues.dlang.org/show_bug.cgi?id=21785

TEST_OUTPUT:
---
fail_compilation/enum_init.d(206): Error: enum `enum_init.Xobj` is opaque and has no default initializer
---
*/
#line 200

enum Xobj : void*;

void main()
{
	Xobj vv = void;
	Xobj var;
}


/*
https://issues.dlang.org/show_bug.cgi?id=21785

TEST_OUTPUT:
---
fail_compilation/enum_init.d(306): Error: variable `enum_init.fooOB.ob` no definition of struct `S`
fail_compilation/enum_init.d(302):        required by type `OpaqueBase`
---
*/
#line 300

struct S;
enum OpaqueBase : S;

void fooOB()
{
	OpaqueBase ob;
}

/*
TEST_OUTPUT:
---
fail_compilation/enum_init.d(405): Error: enum `enum_init.forwardRef.Foo` forward reference of `Foo.init`
---
*/
#line 400

void forwardRef()
{
    enum Foo
    {
        a = Foo.init
    }
}

/*
https://issues.dlang.org/show_bug.cgi?id=21792

TEST_OUTPUT:
---
fail_compilation/enum_init.d(503): Error: circular reference to enum base type `Bar`
---
*/
#line 500

void forwardRef2()
{
    enum Bar : Bar
    {
        a
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/enum_init.d(606): Error: enum member `enum_init.forwardRef3.Foo.b` is forward referenced looking for `.min`
fail_compilation/enum_init.d(607): Error: enum member `enum_init.forwardRef3.Foo.c` is forward referenced looking for `.min`
---
*/
#line 600

void forwardRef3()
{
    enum Foo
    {
        a,
        b = Foo.min,
        c
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/enum_init.d(711): Error: circular reference to enum base type `int[Bar.sizeof]`
---
*/
#line 700

void forwardRef4()
{
    enum Foo
    {
        a = Foo.sizeof,
        c
    }
    // pragma(msg, typeof(Foo.sizeof));
    // static assert(is(Foo Base == enum) && is(Base == int));

    enum Bar : int[Bar.sizeof]
    {
        a
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/enum_init.d(809): Error: enum `enum_init.opaqueProperties.Foo` is opaque and has no default initializer
fail_compilation/enum_init.d(810): Error: enum `enum_init.opaqueProperties.Foo` is opaque and has no `.min`
fail_compilation/enum_init.d(811): Error: enum `enum_init.opaqueProperties.Foo` is opaque and has no `.max`
---
*/
#line 800

void opaqueProperties()
{
    enum Foo;

    // Valid
    enum size = Foo.sizeof;
    enum s = Foo.mangleof;

    Foo f = Foo.init;
    int min = Foo.min;
    int max = Foo.max;
}
