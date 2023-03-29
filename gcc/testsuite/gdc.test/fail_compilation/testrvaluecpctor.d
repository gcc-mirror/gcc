// https://issues.dlang.org/show_bug.cgi?id=22593

/*
TEST_OUTPUT:
---
fail_compilation/testrvaluecpctor.d(16): Error: cannot define both an rvalue constructor and a copy constructor for `struct Foo`
fail_compilation/testrvaluecpctor.d(24):        Template instance `testrvaluecpctor.Foo!int.Foo.__ctor!(immutable(Foo!int), immutable(Foo!int))` creates an rvalue constructor for `struct Foo`
fail_compilation/testrvaluecpctor.d(24): Error: none of the overloads of `__ctor` are callable using a `immutable` object
fail_compilation/testrvaluecpctor.d(18):        Candidates are: `testrvaluecpctor.Foo!int.Foo.this(ref Foo!int rhs)`
fail_compilation/testrvaluecpctor.d(16):                        `__ctor(Rhs, this This)(scope Rhs rhs)`
---
*/

struct Foo(T)
{
    this(Rhs, this This)(scope Rhs rhs){}

    this(ref scope typeof(this) rhs){}
}

void fail22593()
{
    immutable Foo!int a;
    a.__ctor(a);
}

// https://issues.dlang.org/show_bug.cgi?id=21613

/*
TEST_OUTPUT:
---
fail_compilation/testrvaluecpctor.d(40): Error: cannot define both an rvalue constructor and a copy constructor for `struct Test`
fail_compilation/testrvaluecpctor.d(46):        Template instance `testrvaluecpctor.Test.__ctor!()` creates an rvalue constructor for `struct Test`
---
*/

struct Test
{
    this(ref const typeof(this) rhs){}
    this()(const typeof(this) rhs){}    // rvalue ctor
}

void fail21613()
{
    const Test cb;
    Test b = cb;
}
