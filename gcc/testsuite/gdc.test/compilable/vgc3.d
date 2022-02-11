// REQUIRED_ARGS: -vgc -o-
// PERMUTE_ARGS:

/***************** AssignExp *******************/

/*
TEST_OUTPUT:
---
compilable/vgc3.d(16): vgc: setting `length` may cause a GC allocation
compilable/vgc3.d(17): vgc: setting `length` may cause a GC allocation
compilable/vgc3.d(18): vgc: setting `length` may cause a GC allocation
---
*/
void testArrayLength(int[] a)
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
---
*/


void testCall()
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
compilable/vgc3.d(51): vgc: using closure causes GC allocation
compilable/vgc3.d(63): vgc: using closure causes GC allocation
---
*/
auto testClosure1()
{
    int x;
    int bar() { return x; }
    return &bar;
}
void testClosure2()
{
    int x;
    int bar() { return x; }
    takeDelegate2(&bar);     // no error
}
void testClosure3()
{
    int x;
    int bar() { return x; }
    takeDelegate3(&bar);
}
