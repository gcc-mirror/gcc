/* REQUIRED_ARGS: -preview=dip1021
 */

@safe:

/* TEST_OUTPUT:
---
fail_compilation/test1021.d(1009): Error: more than one mutable reference of `p` in arguments to `test1021.fooa()`
fail_compilation/test1021.d(1010): Error: mutable and const references of `p` in arguments to `test1021.foob()`
fail_compilation/test1021.d(1011): Error: mutable and const references of `p` in arguments to `test1021.fooc()`
fail_compilation/test1021.d(1013): Error: more than one mutable reference of `p` in arguments to `test1021.fooe()`
---
*/

#line 1000

void fooa(int*, int*);
void foob(const(int)*, int*);
void fooc(int*, const(int)*);
void food(const(int)*, const(int)*);
void fooe(int*, ...);

void test1(int* p)
{
    fooa(p, p); // error
    foob(p, p); // error
    fooc(p, p); // error
    food(p, p); // ok
    fooe(p, p); // error
}

/***********************************/

/* TEST_OUTPUT:
---
fail_compilation/test1021.d(2010): Error: more than one mutable reference to `i` in arguments to `test1021.fopa()`
fail_compilation/test1021.d(2011): Error: mutable and const references to `i` in arguments to `test1021.fopb()`
fail_compilation/test1021.d(2012): Error: mutable and const references to `i` in arguments to `test1021.fopc()`
fail_compilation/test1021.d(2014): Error: more than one mutable reference to `i` in arguments to `test1021.fope()`
---
*/

#line 2000

void fopa(ref int, scope int*);
void fopb(ref int, scope const int*);
void fopc(ref const int, scope int*);
void fopd(ref const int, scope const int*);
inout(int) fope(ref inout int, scope int*);
void test2()
{
    int i;
    @trusted int* toPtr(ref int i) { return &i; }
    fopa(i, toPtr(i)); // error
    fopb(i, toPtr(i)); // error
    fopc(i, toPtr(i)); // error
    fopd(i, toPtr(i)); // ok
    fope(i, toPtr(i)); // error
}

/***********************************/

/* TEST_OUTPUT:
---
fail_compilation/test1021.d(3015): Error: more than one mutable reference to `s` in arguments to `test1021.S.method()`
fail_compilation/test1021.d(3019): Error: more than one mutable reference of `c` in arguments to `test1021.C.method()`
---
*/

#line 3000

struct S
{
    void method(ref S s);
}

class C
{
    void method(C c);
}

void test3()
{
    S s;
    S* ps;
    s.method(s);  // error
    ps.method(s); // ok

    C c;
    c.method(c);  // error
}

/***********************************/

/* TEST_OUTPUT:
---
fail_compilation/test1021.d(4008): Error: more than one mutable reference to `i` in arguments to `test1021.test4.nested()`
---
*/

#line 4000

void test4()
{
    int i, k;
    int nested(ref int j)
    {
        return i + j;
    }
    nested(i); // error
    nested(k); // ok
}

/***********************************/

/* TEST_OUTPUT:
---
fail_compilation/test1021.d(5012): Error: more than one mutable reference of `s` in arguments to `test1021.foo5()`
---
*/

#line 5000

struct S5
{
    int i;
    int* p;
}

void foo5(S5, S5);

void test5()
{
    S5 s;
    foo5(s, s);
}

alias A5 = void delegate() const;

void foo5(A5, A5);

void test5a()
{
    A5 a;
    foo5(a, a);
}

alias B5 = void function();

void foo5(B5, B5);

void test5b()
{
    B5 b;
    foo5(b, b);
}

struct S5c
{
    void function() fp;
}

void foo5(S5c, S5c);

void test5c()
{
    S5c s;
    foo5(s, s);
}
