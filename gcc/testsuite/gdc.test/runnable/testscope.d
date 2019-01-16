// PERMUTE_ARGS:
// REQUIRED_ARGS: -d -dip1000

extern(C) int printf(const char*, ...);

class Eh : Exception
{
    this()
    {
        super("Eh thrown");
    }
}


/********************************************/

class Foo
{
    static int x;

    this()
    {
        assert(x == 0);
        x++;
        printf("Foo.this()\n");
        throw new Eh();
        assert(0);
    }

    ~this()
    {
        printf("Foo.~this()\n");
    }
}

void test1()
{
    try
    {
        scope Foo f = new Foo();
        assert(0);
    }
    catch (Eh)
    {
        assert(Foo.x == 1);
        Foo.x++;
    }
    finally
    {
        assert(Foo.x == 2);
        Foo.x++;
    }
    assert(Foo.x == 3);
}

/********************************************/

void test2()
{
    int x;
    {
        scope (exit) { printf("test1\n"); assert(x == 3); x = 4; }
        scope (exit) { printf("test2\n"); assert(x == 2); x = 3; }
        scope (exit) { printf("test3\n"); assert(x == 1); x = 2; }
        printf("test4\n"); assert(x == 0); x = 1;
    }
    assert(x == 4);
}

/********************************************/

void test3()
{
    int x;
    {
        scope (success) { printf("test1\n"); assert(x == 3); x = 4; }
        scope (success) { printf("test2\n"); assert(x == 2); x = 3; }
        scope (success) { printf("test3\n"); assert(x == 1); x = 2; }
        printf("test4\n"); assert(x == 0); x = 1;
    }
    assert(x == 4);
}

/********************************************/

void test4()
{
    int x;
    try
    {
        scope (exit) { printf("test1\n"); assert(x == 3); x = 4; }
        scope (exit) { printf("test2\n"); assert(x == 2); x = 3; }
        x = 2;
        throw new Eh;
        scope (exit) { printf("test3\n"); assert(x == 1); x = 2; }
        printf("test4\n"); assert(x == 0); x = 1;
    }
    catch (Eh e)
    {
    }
    assert(x == 4);
}

/********************************************/

void test5()
{
    int x;
    try
    {
        scope (success) { printf("test1\n"); assert(x == 3); x = 4; }
        scope (success) { printf("test2\n"); assert(x == 2); x = 3; }
        x = 2;
        throw new Eh;
        scope (success) { printf("test3\n"); assert(x == 1); x = 2; }
        printf("test4\n"); assert(x == 0); x = 1;
    }
    catch (Eh e)
    {
    }
    assert(x == 2);
}

/********************************************/

void test6()
{
    int x;
    scope (failure) { assert(0); }
    try
    {
        scope (failure) { printf("test1\n"); assert(x == 3); x = 4; }
        scope (failure) { printf("test2\n"); assert(x == 2); x = 3; }
        x = 2;
        throw new Eh;
        scope (failure) { printf("test3\n"); assert(x == 1); x = 2; }
        printf("test4\n"); assert(x == 0); x = 1;
    }
    catch (Eh e)
    {
    }
    assert(x == 4);
}

/********************************************/

void test7()
{   int i;
    int x;

    void foo()
    {
        scope (success) { assert(x == 1); x = 2; }
        i = 2;
        if (i == 2)
            return;
    }

    i = 1;
    x = 1;
    foo();
    assert(x == 2);
}

/********************************************/


void test8()
{
    int i;
    {
        version (all)
        {
            scope (exit) i += 2;
        }
        assert(i == 0);
        i += 1;
        printf("betty\n");
    }
    assert(i == 3);
}

/********************************************/

char[] r9;

int scp( int n )
{
    if( n==0 ) return 0;
    scope(exit)
    {   printf("%d",n);
        r9 ~= cast(char)(n + '0');
    }
    return scp(n-1);
}

void test9()
{
    scp(5);
    assert(r9 == "12345");
}

/********************************************/

alias real T;

T readMessageBegin() { return 3.0; }

T bar10() { return 8.0; }

T foo10() {
  // Send RPC request, etc.
  readMessageBegin();
  scope (exit) readMessageEnd();

  T result = bar10();
  // Read message off the wire.
  return result;
}

void test10()
{
    if (foo10() != 8.0)
        assert(0);
}

T readMessageEnd() {
    static T d;
    d = 4.0;
    d = (((((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))+(((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d)))))/((((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))+(((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d)))))+((((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))+(((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d)))))/((((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))+(((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))))*(((((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))+(((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d)))))/((((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))+(((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d)))))+((((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))+(((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d)))))/((((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))+(((d-(2*d))+(d-(2*d)))*((d-(2*d))+(d-(2*d))))));
    return 4.0;
}

/********************************************/

void test7435() {
  scope(failure)
    debug printf("error\n");

  printf("do something\n");
}

/********************************************/

char[] dup12()(char[] a) // although inferred pure, don't infer a is 'return'
{
    char[] res;
    foreach (ref e; a)
    {}
    return res;
}

char[] foo12()
{
    char[10] buf;
    return dup12(buf);
}

/********************************************/

void test7049() @safe
{
    int count = 0;
    @safe void foo()
    {
        scope (failure) { count++; }
        scope (failure) { count++; }
        throw new Exception("failed");
    }

    try {
        foo();
    } catch(Exception e) {
    }
    assert(count == 2);
}

/********************************************/

// https://issues.dlang.org/show_bug.cgi?id=16747

void test16747() @safe
{
    scope o = new Object();
}

/********************************************/

void bar11(int*, int*) { }

void test11()
{
    static int* p;
    static int i;
    bar11(p, &i);

    bar11((i,p), &i);  // comma expressions are deprecated, but need to test them
}

/********************************************/
// https://issues.dlang.org/show_bug.cgi?id=17432

int test17432(scope int delegate() dg)
{
	return dg();
}

// stripped down version of std.traits.Parameters
template Parameters(alias func)
{
    static if (is(typeof(func) P == function))
        alias Parameters = P;
    else
        static assert(0, "unsupported");
}

alias op = Parameters!(test17432)[0];
enum typeString = op.stringof;
static assert(typeString == "int delegate()"); // no scope added?
mixin(typeString ~ " dg;");
alias ty = typeof(dg);

static assert(op.stringof == ty.stringof);
static assert(op.mangleof == ty.mangleof);

void test17432_2()(scope void delegate () dg) { dg(); }

static assert(typeof(&test17432_2!()).stringof == "void function(scope void delegate() dg) @system");

/********************************************/

byte typify13(T)(byte val) { return val; }
alias INT8_C13  = typify13!byte;

/********************************************/

template test14(T)
{
    alias test14 = int;
}

test14!(char[] function(return char[])) x14;

/********************************************/

void main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();
    test10();
    test7435();
    test7049();
    test16747();
    test11();

    printf("Success\n");
}
