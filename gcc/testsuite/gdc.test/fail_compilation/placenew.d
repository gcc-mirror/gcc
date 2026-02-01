/* TEST_OUTPUT:
---
fail_compilation/placenew.d(26): Error: PlacementExpression `3` is an rvalue, but must be an lvalue
fail_compilation/placenew.d(31): Error: undefined identifier `x`
fail_compilation/placenew.d(39): Error: `new ( i )` PlacementExpression cannot be evaluated at compile time
fail_compilation/placenew.d(42):        called from here: `xxx()`
fail_compilation/placenew.d(42):        while evaluating: `static assert(xxx() == 1)`
fail_compilation/placenew.d(51): Error: new placement size 24 must be >= object size 40
fail_compilation/placenew.d(57): Error: placement new cannot be used with associative arrays
fail_compilation/placenew.d(70): Error: new placement size 4 must be >= class object size $?:32=16|64=24$
fail_compilation/placenew.d(80): Error: placement `new` is not allowed in a `@safe` function
fail_compilation/placenew.d(89): Error: PlacementExpression cannot be a dynamic array
fail_compilation/placenew.d(92): Error: placement new cannot be used with dynamic arrays
fail_compilation/placenew.d(95): Error: PlacementExpression `s` of type `const(int)` must be unshared and mutable
---
*/

void test0()
{
    int i;
    int* pi = new (i) int; // OK
}

void test1()
{
    int* pi = new (3) int;
}

void test2()
{
    int* px = new (x) int;
}

void test3()
{
    int xxx()
    {
        int i;
        int* pi = new (i) int(1);
        return 1;
    }
    static assert(xxx() == 1);
}

struct S { int[6] a; }
struct T { int[10] a; }

void test4()
{
    S s;
    new (s) T();
}

void test5()
{
    T p;
    auto aa = new(p) int[int*];
}

/*************************************************/

class C6
{
    int i, j;
}

int test6()
{
    int k;
    C6 c = new(k) C6;
    return c.j;
}

/*************************************************/

@safe
void test7()
{
    int i;
    int* p = new(i) int;
}

/*************************************************/

void test8()
{
    void[] a;
    a.reserve(int.sizeof);
    int* ps = new(a) int;

    ubyte[a.sizeof] sa;
    int[] ia = new(sa) int[4];

    const int s;
    ps = new(s) int;
}
