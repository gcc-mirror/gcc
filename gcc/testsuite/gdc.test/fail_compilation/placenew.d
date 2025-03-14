/* TEST_OUTPUT:
---
fail_compilation/placenew.d(23): Error: PlacementExpression `3` is an rvalue, but must be an lvalue
fail_compilation/placenew.d(28): Error: undefined identifier `x`
fail_compilation/placenew.d(36): Error: `new ( i )` PlacementExpression cannot be evaluated at compile time
fail_compilation/placenew.d(39):        called from here: `xxx()`
fail_compilation/placenew.d(39):        while evaluating: `static assert(xxx() == 1)`
fail_compilation/placenew.d(48): Error: new placement size 24 must be >= object size 40
fail_compilation/placenew.d(54): Error: placement new cannot be used with associative arrays
fail_compilation/placenew.d(67): Error: new placement size 4 must be >= class object size $?:32=16|64=24$
fail_compilation/placenew.d(77): Error: `@safe` function `test7` cannot use placement `new` is not allowed in a `@safe` function
---
*/

void test0()
{
    int i;
    int* pi = new (i) int;
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
