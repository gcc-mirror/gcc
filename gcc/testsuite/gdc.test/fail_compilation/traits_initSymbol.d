/********************************************
TEST_OUTPUT:
---
fail_compilation/traits_initSymbol.d(105): Error: struct / class type expected as argument to __traits(initSymbol) instead of `int`
fail_compilation/traits_initSymbol.d(106): Error: struct / class type expected as argument to __traits(initSymbol) instead of `S[2]`
fail_compilation/traits_initSymbol.d(107): Error: struct / class type expected as argument to __traits(initSymbol) instead of `123`
---
*/
#line 100

struct S { int i = 4; }

void test1()
{
    const void[] initInt   = __traits(initSymbol, int);
    const void[] initArray = __traits(initSymbol, S[2]);
    const void[] initValue = __traits(initSymbol, 123);
}

/********************************************
TEST_OUTPUT:
---
fail_compilation/traits_initSymbol.d(203): Error: cannot determine the address of the initializer symbol during CTFE
fail_compilation/traits_initSymbol.d(203):        called from here: `(*function () pure nothrow @nogc @safe => S)()`
---
*/
#line 200

void test2()
{
    enum initLen = (() => __traits(initSymbol, S))();
}

/********************************************
TEST_OUTPUT:
---
fail_compilation/traits_initSymbol.d(305): Error: struct / class type expected as argument to __traits(initSymbol) instead of `traits_initSymbol.Interface`
---
*/
#line 300

interface Interface {}

void test3()
{
    const void[] initInterface = __traits(initSymbol, Interface);
}

/********************************************
TEST_OUTPUT:
---
fail_compilation/traits_initSymbol.d(404): Error: expected 1 arguments for `initSymbol` but had 0
fail_compilation/traits_initSymbol.d(405): Error: expected 1 arguments for `initSymbol` but had 2
---
*/
#line 400


void test4()
{
    const void[] tmp = __traits(initSymbol);
    const void[] tmo = __traits(initSymbol, Interface, S);
}
