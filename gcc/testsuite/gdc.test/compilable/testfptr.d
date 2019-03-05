// PERMUTE_ARGS:

ref int frvv();
class A {}
class B : A {}

B restrictedfunc(in const(int)) @safe pure nothrow;
A relaxedfunc(in int);

void bug3797()
{
    // Cannot convert if the return type or parameters are different

    void function() vv;
    void function(int) vi;
    int function() iv;
    const(int) function() cv;
    immutable(int) function() xv;

    static assert( is(typeof( vv = vv )));
    static assert(!is(typeof( vv = vi )));
    static assert(!is(typeof( vv = iv )));
    static assert(!is(typeof( vv = cv )));
    static assert(!is(typeof( vv = xv )));

    static assert(!is(typeof( vi = vv )));
    static assert( is(typeof( vi = vi )));
    static assert(!is(typeof( vi = iv )));
    static assert(!is(typeof( vi = cv )));
    static assert(!is(typeof( vi = cx )));

    static assert(!is(typeof( iv = vv )));
    static assert(!is(typeof( iv = vi )));
    static assert( is(typeof( iv = iv )));
    static assert( is(typeof( iv = cv )));
    static assert( is(typeof( iv = xv )));

    static assert(!is(typeof( cv = vv )));
    static assert( is(typeof( cv = iv )));
    static assert(!is(typeof( cv = vi )));
    static assert( is(typeof( cv = cv )));
    static assert( is(typeof( cv = xv )));

    static assert(!is(typeof( xv = vv )));
    static assert( is(typeof( xv = iv )));
    static assert(!is(typeof( xv = vi )));
    static assert( is(typeof( xv = cv )));
    static assert( is(typeof( xv = xv )));

    int* function() ipfunc;
    const(int*) function() cipfunc;

    static assert( is(typeof( cipfunc = ipfunc )) );
    static assert(!is(typeof( ipfunc = cipfunc )) );

    // functions with different linkages can't convert

    extern(C) void function() cfunc;
    extern(D) void function() dfunc;

    static assert(!is(typeof( cfunc = dfunc )));
    static assert(!is(typeof( dfunc = cfunc )));

    // ref return can't convert to non-ref return

    typeof(&frvv) rvv;

    static assert(!is(typeof( rvv = iv )));
    static assert(!is(typeof( rvv = cv )));

    static assert(!is(typeof( iv = rvv )));
    static assert(!is(typeof( cv = rvv )));

    // variadic functions don't mix

    void function(...) vf;

    static assert(!is(typeof( vf = vv )));
    static assert(!is(typeof( vv = vf )));

    // non-nothrow -> nothrow

    void function() nothrow ntf;

    static assert(!is(typeof( ntf = vv )));
    static assert( is(typeof( vv = ntf )));

    // @safe <-> @trusted -> @system

    void function() @system systemfunc;
    void function() @trusted trustedfunc;
    void function() @safe safefunc;

    static assert( is(typeof( trustedfunc = safefunc )));
    static assert( is(typeof( systemfunc = trustedfunc )));
    static assert( is(typeof( systemfunc = safefunc )));
    static assert( is(typeof( safefunc = trustedfunc )));

    static assert(!is(typeof( trustedfunc = systemfunc )));
    static assert(!is(typeof( safefunc = systemfunc )));

    // pure -> non-pure

    void function() nonpurefunc;
    void function() pure purefunc;

    static assert(!is(typeof( purefunc = nonpurefunc )));
    static assert( is(typeof( nonpurefunc = purefunc )));

    // Cannot convert parameter storage classes (except const to in and in to const)

    void function(const(int)) constfunc;
    void function(in int) infunc;
    void function(out int) outfunc;
    void function(ref int) reffunc;
    void function(lazy int) lazyfunc;

    static assert(is(typeof( infunc = constfunc )));
    static assert(is(typeof( constfunc = infunc )));

    static assert(!is(typeof( infunc = outfunc )));
    static assert(!is(typeof( infunc = reffunc )));
    static assert(!is(typeof( infunc = lazyfunc )));

    static assert(!is(typeof( outfunc = infunc )));
    static assert(!is(typeof( outfunc = reffunc )));
    static assert(!is(typeof( outfunc = lazyfunc )));

    static assert(!is(typeof( reffunc = infunc )));
    static assert(!is(typeof( reffunc = outfunc )));
    static assert(!is(typeof( reffunc = lazyfunc )));

    static assert(!is(typeof( lazyfunc = infunc )));
    static assert(!is(typeof( lazyfunc = outfunc )));
    static assert(!is(typeof( lazyfunc = reffunc )));

    // Test class covariance

    A function() afunc;
    B function() bfunc;

    static assert( is(typeof( afunc = bfunc )));
    static assert(!is(typeof( bfunc = afunc )));

    // Test all the conversions at once
    typeof(&restrictedfunc) prestrictedfunc;
    typeof(&relaxedfunc) prelaxedfunc = prestrictedfunc;
}

void bug3797dg()
{
    ref int frvv() { return *(new int); }
    B restrictedfunc(in const(int)) @safe pure nothrow { return null; }
    A relaxedfunc(in int) { return null; }
    // Cannot convert if the return type or parameters are different

    void delegate() vv;
    void delegate(int) vi;
    int delegate() iv;
    const(int) delegate() cv;
    immutable(int) delegate() xv;

    static assert( is(typeof( vv = vv )));
    static assert(!is(typeof( vv = vi )));
    static assert(!is(typeof( vv = iv )));
    static assert(!is(typeof( vv = cv )));
    static assert(!is(typeof( vv = xv )));

    static assert(!is(typeof( vi = vv )));
    static assert( is(typeof( vi = vi )));
    static assert(!is(typeof( vi = iv )));
    static assert(!is(typeof( vi = cv )));
    static assert(!is(typeof( vi = cx )));

    static assert(!is(typeof( iv = vv )));
    static assert(!is(typeof( iv = vi )));
    static assert( is(typeof( iv = iv )));
    static assert( is(typeof( iv = cv )));
    static assert( is(typeof( iv = xv )));

    static assert(!is(typeof( cv = vv )));
    static assert( is(typeof( cv = iv )));
    static assert(!is(typeof( cv = vi )));
    static assert( is(typeof( cv = cv )));
    static assert( is(typeof( cv = xv )));

    static assert(!is(typeof( xv = vv )));
    static assert( is(typeof( xv = iv )));
    static assert(!is(typeof( xv = vi )));
    static assert( is(typeof( xv = cv )));
    static assert( is(typeof( xv = xv )));

    int* delegate() ipfunc;
    const(int*) delegate() cipfunc;

    static assert( is(typeof( cipfunc = ipfunc )) );
    static assert(!is(typeof( ipfunc = cipfunc )) );

    // delegates with different linkages can't convert

    extern(C) void delegate() cfunc;
    extern(D) void delegate() dfunc;

    static assert(!is(typeof( cfunc = dfunc )));
    static assert(!is(typeof( dfunc = cfunc )));

    // ref return can't convert to non-ref return

    typeof(&frvv) rvv;

    static assert(!is(typeof( rvv = iv )));
    static assert(!is(typeof( rvv = cv )));

    static assert(!is(typeof( iv = rvv )));
    static assert(!is(typeof( cv = rvv )));

    // variadic delegates don't mix

    void delegate(...) vf;

    static assert(!is(typeof( vf = vv )));
    static assert(!is(typeof( vv = vf )));

    // non-nothrow -> nothrow

    void delegate() nothrow ntf;

    static assert(!is(typeof( ntf = vv )));
    static assert( is(typeof( vv = ntf )));

    // @safe <-> @trusted -> @system

    void delegate() @system systemfunc;
    void delegate() @trusted trustedfunc;
    void delegate() @safe safefunc;

    static assert( is(typeof( trustedfunc = safefunc )));
    static assert( is(typeof( systemfunc = trustedfunc )));
    static assert( is(typeof( systemfunc = safefunc )));
    static assert( is(typeof( safefunc = trustedfunc )));

    static assert(!is(typeof( trustedfunc = systemfunc )));
    static assert(!is(typeof( safefunc = systemfunc )));

    // pure -> non-pure

    void delegate() nonpurefunc;
    void delegate() pure purefunc;

    static assert(!is(typeof( purefunc = nonpurefunc )));
    static assert( is(typeof( nonpurefunc = purefunc )));

    // Cannot convert parameter storage classes (except const to in and in to const)

    void delegate(const(int)) constfunc;
    void delegate(in int) infunc;
    void delegate(out int) outfunc;
    void delegate(ref int) reffunc;
    void delegate(lazy int) lazyfunc;

    static assert(is(typeof( infunc = constfunc )));
    static assert(is(typeof( constfunc = infunc )));

    static assert(!is(typeof( infunc = outfunc )));
    static assert(!is(typeof( infunc = reffunc )));
    static assert(!is(typeof( infunc = lazyfunc )));

    static assert(!is(typeof( outfunc = infunc )));
    static assert(!is(typeof( outfunc = reffunc )));
    static assert(!is(typeof( outfunc = lazyfunc )));

    static assert(!is(typeof( reffunc = infunc )));
    static assert(!is(typeof( reffunc = outfunc )));
    static assert(!is(typeof( reffunc = lazyfunc )));

    static assert(!is(typeof( lazyfunc = infunc )));
    static assert(!is(typeof( lazyfunc = outfunc )));
    static assert(!is(typeof( lazyfunc = reffunc )));

    // Test class covariance

    A delegate() afunc;
    B delegate() bfunc;

    static assert( is(typeof( afunc = bfunc )));
    static assert(!is(typeof( bfunc = afunc )));

    // Test all the conversions at once
    typeof(&restrictedfunc) prestrictedfunc;
    typeof(&relaxedfunc) prelaxedfunc = prestrictedfunc;
}

void bug3268()
{
    auto a = &bug3268;
    const b = a;
    assert(a == a);
    assert(a == b);
    assert(b == b);
    immutable c = cast(immutable)a;
    assert(a == c);
    assert(b == c);
    assert(c == c);

    static assert(is(typeof(*a) == typeof(*b)));
    static assert(is(typeof(*a) == typeof(*c)));
}

void bug3268dg()
{
    void bug3268x() {}
    auto a = &bug3268x;
    const b = a;
    assert(a == a);
    assert(a == b);
    assert(b == b);
    immutable c = cast(immutable)a;
    assert(a == c);
    assert(b == c);
    assert(c == c);
}

void bug3833()
{
    bool b;

    void function() func;
    void function() pure purefunc;
    void function() nothrow nothrowfunc;
    void function() @safe safefunc;
    void function() @trusted trustedfunc;

    static assert( is(typeof( b ? func : purefunc )     == typeof( func )));
    static assert( is(typeof( b ? func : nothrowfunc )  == typeof( func )));
    static assert( is(typeof( b ? func : safefunc )     == typeof( func )));
    static assert( is(typeof( b ? func : trustedfunc )  == typeof( func )));

    static assert( is(typeof( b ? purefunc : nothrowfunc )  == typeof( func )));
    static assert( is(typeof( b ? purefunc : safefunc )     == typeof( func )));
    static assert( is(typeof( b ? purefunc : trustedfunc )  == typeof( func )));

    static assert( is(typeof( b ? nothrowfunc : safefunc )      == typeof( func )));
    static assert( is(typeof( b ? nothrowfunc : trustedfunc )   == typeof( func )));

    static assert( is(typeof( b ? safefunc : trustedfunc )      == typeof( trustedfunc )));

    auto arr = [func, purefunc, nothrowfunc, safefunc, trustedfunc];

    static assert( is(typeof( arr ) == typeof(func)[]) );
}

void bug3833dg()
{
    bool b;

    void delegate() func;
    void delegate() pure purefunc;
    void delegate() nothrow nothrowfunc;
    void delegate() @safe safefunc;
    void delegate() @trusted trustedfunc;

    static assert( is(typeof( b ? func : purefunc )     == typeof( func )));
    static assert( is(typeof( b ? func : nothrowfunc )  == typeof( func )));
    static assert( is(typeof( b ? func : safefunc )     == typeof( func )));
    static assert( is(typeof( b ? func : trustedfunc )  == typeof( func )));

    static assert( is(typeof( b ? purefunc : nothrowfunc )  == typeof( func )));
    static assert( is(typeof( b ? purefunc : safefunc )     == typeof( func )));
    static assert( is(typeof( b ? purefunc : trustedfunc )  == typeof( func )));

    static assert( is(typeof( b ? nothrowfunc : safefunc )      == typeof( func )));
    static assert( is(typeof( b ? nothrowfunc : trustedfunc )   == typeof( func )));

    static assert( is(typeof( b ? safefunc : trustedfunc )      == typeof( trustedfunc )));

    auto arr = [func, purefunc, nothrowfunc, safefunc, trustedfunc];

    static assert( is(typeof( arr ) == typeof(func)[]) );
}

void bug4838()
{
    void delegate() const dgc;
    static assert(typeof(dgc).stringof == "void delegate() const");

    void delegate() immutable dgi;
    static assert(typeof(dgi).stringof == "void delegate() immutable");

    void delegate() shared dgs;
    static assert(typeof(dgs).stringof == "void delegate() shared");

    void delegate() shared const dgsc;
    static assert(typeof(dgsc).stringof == "void delegate() shared const");

    void delegate() inout dgw;
    static assert(typeof(dgw).stringof == "void delegate() inout");

    void delegate() shared inout dgsw;
    static assert(typeof(dgsw).stringof == "void delegate() shared inout");
}

void test8822()
{
    struct S { void foo() const {} }
    S s;
    void delegate() const dg = &s.foo;      // OK

    void foo(void delegate() const dg){}    // OK

    struct Foo(T) {}
    alias Foo!(void delegate() const) X;    // NG -> OK
}

void main()
{
    static assert(is(typeof(&main) P : U*, U));
    auto x = cast(void*)&main;

    const void * p = &main;

    __gshared void function() gp = null;
    __gshared void delegate() gp2 = null;
}
