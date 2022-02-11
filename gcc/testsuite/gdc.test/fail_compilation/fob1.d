// REQUIRED_ARGS: -preview=dip1021

/* TEST_OUTPUT:
---
fail_compilation/fob1.d(104): Error: variable `fob1.foo1.p` has undefined state and cannot be read
fail_compilation/fob1.d(104): Error: variable `fob1.foo1.p` is returned but is Undefined
---
*/

#line 100

@live int* foo1()
{
    int* p = void;
    return p;
}

/* TEST_OUTPUT:
---
fail_compilation/fob1.d(203): Error: variable `fob1.foo2.p` is left dangling at return
---
*/

#line 200

@live void foo2()
{
    int* p;
    p = null;
}


/* TEST_OUTPUT:
---
fail_compilation/fob1.d(304): Error: variable `fob1.foo3.p` has undefined state and cannot be read
fail_compilation/fob1.d(304): Error: variable `fob1.foo3.p` is returned but is Undefined
fail_compilation/fob1.d(303): Error: variable `fob1.foo3.q` is left dangling at return
---
*/

#line 300

@live int* foo3(int* p)
{
    int* q = p;
    return p;
}

/* TEST_OUTPUT:
---
fail_compilation/fob1.d(405): Error: variable `fob1.foo4.bq` has undefined state and cannot be read
---
*/

#line 400

@live int* foo4(int* p)
{
    scope int* bq = p;
    scope const int* cq = p;
    *bq = 1;
    return p;
}
