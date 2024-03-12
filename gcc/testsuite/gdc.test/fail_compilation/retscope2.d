/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/retscope2.d(102): Error: scope variable `s` assigned to `ref` variable `p` with longer lifetime
fail_compilation/retscope2.d(107): Error: address of variable `s` assigned to `p` with longer lifetime
---
*/

#line 100
@safe foo1(ref char[] p, scope char[] s)
{
    p = s;
}

@safe bar1(ref char* p, char s)
{
    p = &s;
}

/**********************************************/

// https://issues.dlang.org/show_bug.cgi?id=17123

void test200()
{
    char[256] buffer;

    char[] delegate() read = () {
        return buffer[];
    };
}

/**********************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(302): Error: scope variable `a` assigned to return scope `b`
---
*/

#line 300
@safe int* test300(scope int* a, return scope int* b)
{
    b = a;
    return b;
}

/**********************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(403): Error: scope variable `a` assigned to return scope `c`
---
*/

#line 400
@safe int* test400(scope int* a, return scope int* b)
{
    auto c = b; // infers 'return scope' for 'c'
    c = a;
    return c;
}

/**********************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(504): Error: scope variable `c` may not be returned
---
*/

#line 500
@safe int* test500(scope int* a, return scope int* b)
{
    scope c = b; // does not infer 'return' for 'c'
    c = a;
    return c;
}

/**********************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(604): Error: scope variable `__param_0` assigned to non-scope anonymous parameter calling `foo600`
fail_compilation/retscope2.d(604): Error: scope variable `__param_1` assigned to non-scope anonymous parameter calling `foo600`
fail_compilation/retscope2.d(614): Error: template instance `retscope2.test600!(int*, int*)` error instantiating
---
*/

#line 600
@safe test600(A...)(scope A args)
{
    foreach (i, Arg; A)
    {
        foo600(args[i]);
    }
}

@safe void foo600(int*);

@safe bar600()
{
    scope int* p;
    scope int* q;
    test600(p, q);
}

/*************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(719): Error: returning `get2(s)` escapes a reference to local variable `s`
fail_compilation/retscope2.d(721): Error: returning `s.get1()` escapes a reference to local variable `s`
---
*/

#line 700
// https://issues.dlang.org/show_bug.cgi?id=17049

@safe S700* get2(return ref S700 _this)
{
    return &_this;
}

struct S700
{
    @safe S700* get1() scope return
    {
        return &this;
    }
}

S700* escape700(int i) @safe
{
    S700 s;
    if (i)
        return s.get2(); // 719
    else
        return s.get1(); // 721
}

/*************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(804): Error: scope variable `e` may not be thrown
---
*/

#line 800

void foo800() @safe
{
    scope Exception e;
    throw e;
}

/*************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(907): Error: address of variable `this` assigned to `p17568` with longer lifetime
---
*/

#line 900

int* p17568;
struct T17568
{
    int a;
    void escape() @safe scope
    {
        p17568 = &a;
    }
}

/*************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(1005): Error: scope variable `p` assigned to non-scope `this._p`
fail_compilation/retscope2.d(1021): Error: scope variable `p` assigned to non-scope `c._p`
fail_compilation/retscope2.d(1024): Error: scope variable `p` assigned to non-scope `d._p`
---
*/

#line 1000

class C17428
{
    void set(scope int* p) @safe
    {
        _p = p;
    }

    int* _p;
}

class C17428b
{
    int* _p;
}

void test17428() @safe
{
        int x;
        int* p = &x;
        scope C17428b c;
        c._p = p;   // bad

        C17428b d;
        d._p = p;   // bad
}



/*************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(1107): Error: scope variable `dg` may not be returned
---
*/

#line 1100

struct S17430 { void foo() {} }

void delegate() test17430() @safe
{
    S17430 s;
    auto dg = &s.foo; // infer dg as scope
    return dg;
}

/****************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(1216): Error: returning `s.foo()` escapes a reference to local variable `s`
fail_compilation/retscope2.d(1233): Error: returning `t.foo()` escapes a reference to local variable `t`
---
*/

#line 1200
// https://issues.dlang.org/show_bug.cgi?id=17388

struct S17388
{
    //int*
    auto
        foo() return @safe
    {
        return &x;
    }
    int x;
}

@safe int* f17388()
{
    S17388 s;
    return s.foo();
}

struct T17388
{
    //int[]
    auto
        foo() return @safe
    {
        return x[];
    }
    int[4] x;
}

@safe int[] g17388()
{
    T17388 t;
    return t.foo();
}

/****************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope2.d(1306): Error: copying `& i` into allocated memory escapes a reference to local variable `i`
---
*/

#line 1300

// https://issues.dlang.org/show_bug.cgi?id=17370

void test1300() @safe
{
    int i;
    auto p = new S1300(&i).oops;
}

struct S1300
{
    int* oops;
//    this(int* p) @safe { oops = p; }
}
