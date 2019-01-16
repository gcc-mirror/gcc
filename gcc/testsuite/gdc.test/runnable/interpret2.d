
//import std.stdio;
extern(C) int printf(const char*, ...);

template Tuple(A...)
{
    alias A Tuple;
}

template eval(A...)
{
    const typeof(A[0]) eval = A[0];
}

/************************************************/

int foo1()
{
    int x;
    foreach (i; 0 .. 10)
        x += i;
    return x;
}

int bar1()
{
    int x;
    foreach_reverse (i; 0 .. 10)
    {
        x <<= 1;
        x += i;
    }
    return x;
}

void test1()
{
    const y = foo1();
    //writeln(y);
    assert(y == 45);

    auto y1 = foo1();
    //writeln(y1);
    assert(y1 == 45);

    const z = bar1();
    //writeln(z);
    assert(z == 8194);

    auto z1 = bar1();
    //writeln(z1);
    assert(z1 == 8194);
}

/***** Bug 2850 *********************************/

/* These tests are not passing, and shouldn't pass. A non-first field in a union
being initialized cannot be converted to an expression, at least not until there are
improvements to StructLiterals.
 */

version (none)
{
struct Bug2850
{
    union
    {
        int c;
        double d;
    }
    int b;
    int a;
}

static assert(is(typeof(
 () { enum Bug2850 w = {b:47, 714, d:4}; return w; }
)));
static assert(is(typeof(
 () { enum Bug2850 w = {b:47, d:4}; return w; }
)));
// union not initialized
static assert(!is(typeof(
 () { enum Bug2850 w = {b:47, 4}; return w; }
)));
// initializers for two fields in same union
static assert(!is(typeof(
 () { enum Bug2850 w = {b:47, 4, c:5, 9}; return w; }
)));

enum Bug2850 test2850 = {b:47, 714, d:23.1e-17};

struct Horrid2850
{
    union
    {
        int a;
        int b;
        struct
        {
            int c;
            int d;
        }
    }
    int f;
    double q;
}

enum Horrid2850 horrid2850 = {c:5,6};
Horrid2850 m2850 = {47, f:6};
Horrid2850 z2850 = {q:5, c:4, d:5};

static assert(!is(typeof(
 () { enum Horrid2850 w = {c:47, d:5, a:7}; return w; }
)));

void test2()
{
    assert(test2850.a == 714);
    assert(test2850.b == 47);
    assert(test2850.d == 23.1e-17);
    assert(test2850.c != 0);
}
}

/***** Bug 3779 *********************************/

static const bug3779 = ["123"][0][$-1];

/***** Bug 1880 *********************************/


enum Property1880 {First=1,Second=2}

struct CompileTimeCheck1880(Property1880 Prop)
{
    alias Prop prop;
}
Property1880 junkprop1880;
static assert(!is(CompileTimeCheck1880!(junkprop1880)));

int main()
{
    test1();
//    test2();

    printf("Success\n");
    return 0;
}
