// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

/***************** AssignExp *******************/

/*
TEST_OUTPUT:
---
fail_compilation/nogc3.d(16): Error: setting 'length' in @nogc function 'nogc3.testArrayLength' may cause GC allocation
fail_compilation/nogc3.d(17): Error: setting 'length' in @nogc function 'nogc3.testArrayLength' may cause GC allocation
fail_compilation/nogc3.d(18): Error: setting 'length' in @nogc function 'nogc3.testArrayLength' may cause GC allocation
---
*/
@nogc void testArrayLength(int[] a)
{
    a.length = 3;
    a.length += 1;
    a.length -= 1;
}

/***************** CallExp *******************/

void barCall();

/*
TEST_OUTPUT:
---
fail_compilation/nogc3.d(35): Error: @nogc function 'nogc3.testCall' cannot call non-@nogc function pointer 'fp'
fail_compilation/nogc3.d(36): Error: @nogc function 'nogc3.testCall' cannot call non-@nogc function 'nogc3.barCall'
---
*/
@nogc void testCall()
{
    auto fp = &barCall;
    (*fp)();
    barCall();
}

/****************** Closure ***********************/

@nogc void takeDelegate2(scope int delegate() dg) {}
@nogc void takeDelegate3(      int delegate() dg) {}

/*
TEST_OUTPUT:
---
fail_compilation/nogc3.d(53): Error: function nogc3.testClosure1 is @nogc yet allocates closures with the GC
fail_compilation/nogc3.d(56):        nogc3.testClosure1.bar closes over variable x at fail_compilation/nogc3.d(55)
fail_compilation/nogc3.d(65): Error: function nogc3.testClosure3 is @nogc yet allocates closures with the GC
fail_compilation/nogc3.d(68):        nogc3.testClosure3.bar closes over variable x at fail_compilation/nogc3.d(67)
---
*/
@nogc auto testClosure1()
{
    int x;
    int bar() { return x; }
    return &bar;
}
@nogc void testClosure2()
{
    int x;
    int bar() { return x; }
    takeDelegate2(&bar);     // no error
}
@nogc void testClosure3()
{
    int x;
    int bar() { return x; }
    takeDelegate3(&bar);
}

/****************** ErrorExp ***********************/

/*
TEST_OUTPUT:
---
fail_compilation/nogc3.d(86): Error: array literal in @nogc function 'nogc3.foo13702' may cause GC allocation
fail_compilation/nogc3.d(87): Error: array literal in @nogc function 'nogc3.foo13702' may cause GC allocation
fail_compilation/nogc3.d(93): Error: array literal in @nogc function 'nogc3.bar13702' may cause GC allocation
fail_compilation/nogc3.d(92): Error: array literal in @nogc function 'nogc3.bar13702' may cause GC allocation
---
*/
int[] foo13702(bool b) @nogc
{
    if (b)
        return [1];     // error
    return 1 ~ [2];     // error
}
int[] bar13702(bool b) @nogc
{
    if (b)
        return [1];     // error <- no error report
    auto aux = 1 ~ [2]; // error
    return aux;
}
