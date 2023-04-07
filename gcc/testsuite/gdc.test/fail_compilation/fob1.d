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
fail_compilation/fob1.d(204): Error: variable `fob1.foo2.p` assigning to Owner without disposing of owned value
fail_compilation/fob1.d(203): Error: variable `fob1.foo2.p` is not disposed of before return
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
fail_compilation/fob1.d(303): Error: variable `fob1.foo3.q` is not disposed of before return
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

/* TEST_OUTPUT:
---
fail_compilation/fob1.d(503): Error: more than one mutable reference to `a` in arguments to `fob1.foo5()`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=20781

#line 500

void test5() {
    int a;
    foo5(a, a);
}

@live void foo5(ref int, ref int);
