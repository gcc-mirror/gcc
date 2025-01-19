/*
REQUIRED_ARGS: -preview=dip1000
*/

/*
TEST_OUTPUT:
---
fail_compilation/retscope6.d(6007): Error: escaping a reference to local variable `i` by copying `& i` into allocated memory is not allowed in a `@safe` function
---
*/

#line 6000

// https://issues.dlang.org/show_bug.cgi?id=17795

int* test() @safe
{
    int i;
    int*[][] arr = new int*[][](1);
    arr[0] ~= &i;
    return arr[0][0];
}

/* TEST_OUTPUT:
---
fail_compilation/retscope6.d(7034): Error: assigning address of variable `i` to `s` with longer lifetime is not allowed in a `@safe` function
fail_compilation/retscope6.d(7035): Error: assigning address of variable `i` to `s` with longer lifetime is not allowed in a `@safe` function
fail_compilation/retscope6.d(7025): Error: assigning scope variable `__param_2` to `ref` variable `t` with longer lifetime is not allowed in a `@safe` function
fail_compilation/retscope6.d(7037): Error: template instance `retscope6.S.emplace4!(int*)` error instantiating
fail_compilation/retscope6.d(7037): Error: assigning address of variable `i` to `s` with longer lifetime is not allowed in a `@safe` function
---
*/

#line 7000

alias T = int*;

struct S
{
    T payload;

    static void emplace(Args...)(ref S s, Args args) @safe
    {
        s.payload = args[0];
    }

    void emplace2(Args...)(Args args) @safe
    {
        payload = args[0];
    }

    static void emplace3(Args...)(S s, Args args) @safe
    {
        s.payload = args[0];
    }

    static void emplace4(Args...)(scope ref S s, scope out S t, scope Args args) @safe
    {
        s.payload = args[0];
        t.payload = args[0];
    }

}

void foo() @safe
{
    S s;
    int i;
    s.emplace(s, &i);
    s.emplace2(&i);
    s.emplace3(s, &i);
    s.emplace4(s, s, &i);
}


/* TEST_OUTPUT:
---
fail_compilation/retscope6.d(8016): Error: assigning address of variable `i` to `p` with longer lifetime is not allowed in a `@safe` function
fail_compilation/retscope6.d(8031): Error: assigning reference to local variable `i` to non-scope parameter `p` calling `betty` is not allowed in a `@safe` function
fail_compilation/retscope6.d(8031): Error: assigning reference to local variable `j` to non-scope parameter `q` calling `betty` is not allowed in a `@safe` function
fail_compilation/retscope6.d(8023):        which is not `scope` because of `p = q`
fail_compilation/retscope6.d(8048): Error: assigning reference to local variable `i` to non-scope parameter `p` calling `archie` is not allowed in a `@safe` function
fail_compilation/retscope6.d(8039):        which is not `scope` because of `r = p`
fail_compilation/retscope6.d(8048): Error: assigning reference to local variable `j` to non-scope parameter `q` calling `archie` is not allowed in a `@safe` function
fail_compilation/retscope6.d(8038):        which is not `scope` because of `p = q`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=19035

#line 8000
@safe
{

void escape(int*);

/**********************/

void frank()(ref scope int* p, int* s)
{
    p = s;  // should error here
}

void testfrankly()
{
    int* p;
    int i;
    frank(p, &i);
}

/**********************/

void betty()(int* p, int* q)
{
    p = q;
    escape(p);
}

void testbetty()
{
    int i;
    int j;
    betty(&i, &j); // should error on i and j
}

/**********************/

void archie()(int* p, int* q, int* r)
{
    p = q;
    r = p;
    escape(q);
}

void testarchie()
{
    int i;
    int j;
    int k;
    archie(&i, &j, &k); // should error on j
}

}

/* TEST_OUTPUT:
---
fail_compilation/retscope6.d(9023): Error: escaping a reference to local variable `i` by returning `fred(& i)`  is not allowed in a `@safe` function
---
*/

#line 9000

@safe
{

alias T9 = S9!(); struct S9()
{
     this(int* q)
     {
        this.p = q;
     }

     int* p;
}

auto fred(int* r)
{
    return T9(r);
}

T9 testfred()
{
    int i;
    auto j = fred(&i); // ok
    return fred(&i);   // error
}

/* TEST_OUTPUT:
---
fail_compilation/retscope6.d(10003): Error: assigning scope variable `values` to non-scope parameter `values` calling `escape` is not allowed in a `@safe` function
---
*/

#line 10000

void variadicCaller(int[] values...)
{
    escape(values);
}

void escape(int[] values) {}

/* TEST_OUTPUT:
---
fail_compilation/retscope6.d(11004): Error: assigning address of variable `buffer` to `secret` with longer lifetime is not allowed in a `@safe` function
---
*/

#line 11000

void hmac(scope ubyte[] secret)
{
    ubyte[10] buffer;
    secret = buffer[];
}
}

/* TEST_OUTPUT:
---
fail_compilation/retscope6.d(12011): Error: escaping a reference to local variable `x` by returning `escape_m_20150(& x)`  is not allowed in a `@safe` function
fail_compilation/retscope6.d(12022): Error: escaping a reference to local variable `x` by returning `escape_c_20150(& x)`  is not allowed in a `@safe` function
---
*/

#line 12000

// https://issues.dlang.org/show_bug.cgi?id=20150

int* escape_m_20150(int* r) @safe pure nothrow
{
    return r;
}

int* f_m_20150() @safe nothrow
{
    int x = 42;
    return escape_m_20150(&x);
}

const(int)* escape_c_20150(const int* r) @safe pure nothrow
{
    return r;
}

const(int)* f_c_20150() @safe nothrow
{
    int x = 42;
    return escape_c_20150(&x);
}

/* TEST_OUTPUT:
---
fail_compilation/retscope6.d(13010): Error: assigning reference to local variable `str` to non-scope parameter `x` calling `f_throw` is not allowed in a `@safe` function
---
*/

#line 13000
// https://issues.dlang.org/show_bug.cgi?id=22221

void f_throw(string x) @safe pure
{
    throw new Exception(x);
}

void escape_throw_20150() @safe
{
    immutable(char)[4] str;
    f_throw(str[]);
}

/* TEST_OUTPUT:
---
fail_compilation/retscope6.d(14019): Error: assigning scope variable `scopePtr` to non-scope parameter `x` calling `noInfer23021` is not allowed in a `@safe` function
fail_compilation/retscope6.d(14009):        which is not `scope` because of `*escapeHole = cast(const(int)*)x`
fail_compilation/retscope6.d(14022): Error: returning scope variable `scopePtr` is not allowed in a `@safe` function
---
*/

#line 14000
// https://issues.dlang.org/show_bug.cgi?id=23021

ref int infer23021(ref int* x) @safe pure nothrow
{
    return *x;
}

ref int noInfer23021(ref int* x, const(int)** escapeHole = null) @safe pure nothrow
{
    *escapeHole = x;
    return *x;
}

ref int escape23021() @safe
{
    scope int* scopePtr;
    int* nonScopePtr = null;

    // don't infer scope
    cast(void) noInfer23021(scopePtr); // error

    // ensure we infer return scope
    return infer23021(scopePtr); // error

    // ensure we do not infer return ref
    return infer23021(nonScopePtr); // no error
}

/******************************/

/* TEST_OUTPUT:
---
fail_compilation/retscope6.d(14050): Error: assigning scope variable `z` to non-scope parameter `y` calling `f23294` is not allowed in a `@safe` function
fail_compilation/retscope6.d(14044):        which is not `scope` because of `x = y`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23294

@safe:
int g23294;

auto f23294(int* x, int* y)
{
    x = y;
    g23294++; // make sure it's not inferring scope from pure
}

void escape23294(scope int* z)
{
    f23294(z, z); // passes
}
