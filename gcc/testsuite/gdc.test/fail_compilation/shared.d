/* REQUIRED_ARGS: -preview=nosharedaccess
 * TEST_OUTPUT:
---
fail_compilation/shared.d(1010): Error: direct access to shared `j` is not allowed, see `core.atomic`
fail_compilation/shared.d(1011): Error: direct access to shared `j` is not allowed, see `core.atomic`
fail_compilation/shared.d(1012): Error: direct access to shared `*p` is not allowed, see `core.atomic`
fail_compilation/shared.d(1013): Error: direct access to shared `a[0]` is not allowed, see `core.atomic`
fail_compilation/shared.d(1014): Error: direct access to shared `s.si` is not allowed, see `core.atomic`
fail_compilation/shared.d(1015): Error: direct access to shared `t.i` is not allowed, see `core.atomic`
---
*/

#line 1000

struct S
{
    shared(int) si;
    int i;
}

int test1(shared int j, shared(int)* p, shared(int)[] a, ref S s, ref shared S t)
{
    int i;
    j = i;
    i = j;
    i = *p;
    i = a[0];
    i = s.si;
    return t.i;
}

/**************************************/

void byref(ref shared int);
void byptr(shared(int)*);

shared int x;

void test2()
{
    byref(x);   // ok
    byptr(&x);  // ok
}

/**************************************/

/*
 * TEST_OUTPUT:
---
fail_compilation/shared.d(2008): Error: direct access to shared `i` is not allowed, see `core.atomic`
fail_compilation/shared.d(2009): Error: direct access to shared `j` is not allowed, see `core.atomic`
fail_compilation/shared.d(2010): Error: direct access to shared `k` is not allowed, see `core.atomic`
---
 */

#line 2000

void func(int);

shared int i;

void test3(shared int k)
{
    shared int j = void;
    func(i);
    func(j);
    func(k);
}

/**************************************/

void test4() // no errors for initialization
{
    shared int x;
    shared int y = 3;
}

/*
 * TEST_OUTPUT:
---
fail_compilation/shared.d(2105): Error: direct access to shared `*pi` is not allowed, see `core.atomic`
fail_compilation/shared.d(2112): Error: direct access to shared `**pi` is not allowed, see `core.atomic`
fail_compilation/shared.d(2136): Error: direct access to shared `*c` is not allowed, see `core.atomic`
fail_compilation/shared.d(2142): Error: direct access to shared `*c` is not allowed, see `core.atomic`
fail_compilation/shared.d(2148): Error: direct access to shared `*c` is not allowed, see `core.atomic`
fail_compilation/shared.d(2154): Error: direct access to shared `*c.c1` is not allowed, see `core.atomic`
fail_compilation/shared.d(2160): Error: direct access to shared `*c.c1.c1` is not allowed, see `core.atomic`
fail_compilation/shared.d(2181): Error: direct access to shared `k` is not allowed, see `core.atomic`
fail_compilation/shared.d(2187): Error: direct access to shared `k.k2.k1.value` is not allowed, see `core.atomic`
fail_compilation/shared.d(2194): Error: direct access to shared `(new shared(K2)).k1` is not allowed, see `core.atomic`
fail_compilation/shared.d(2202): Error: direct access to shared `c` is not allowed, see `core.atomic`
fail_compilation/shared.d(2206): Error: function `shared.test_inference_2` function returns `shared` but cannot be inferred `ref`
fail_compilation/shared.d(2208): Error: returning `c` escapes a reference to parameter `c`
fail_compilation/shared.d(2214): Error: function `shared.test_inference_3` function returns `shared` but cannot be inferred `ref`
fail_compilation/shared.d(2216):        return value `getSharedObject()` is not an lvalue
fail_compilation/shared.d(2222): Error: direct access to shared `a` is not allowed, see `core.atomic`
fail_compilation/shared.d(2220): Error: function `shared.test_inference_4` function returns `shared` but cannot be inferred `ref`
fail_compilation/shared.d(2222):        cannot implicitly convert `a` of type `shared(const(Object))` to `object.Object`
fail_compilation/shared.d(2222): Error: cannot implicitly convert expression `a` of type `shared(const(Object))` to `object.Object`
---
 */

#line 2100
// Derived from https://issues.dlang.org/show_bug.cgi?id=20908
ref shared(int) test20908()
{
    shared int* pi;
    // Single indirection, but the pointer is `shared`
    return *pi;
}

ref shared(int) test20908_2()
{
    shared(int*)* pi;
    // Double indirection, external pointer is not `shared`
    return **pi;
}

// DotVarExp tests: See matching tests in `compilable/shared.d`

struct C1
{
    int value;
}

struct C2
{
    C1* c1;
}

struct C3
{
    C2 c1;
    C2* c2;
}

// Reading a shared pointer: not okay
ref shared(int) test_dotvarexp_1(return shared C1* c)
{
    return c.value;
}

// Ditto, but explicitly dereferenced
ref shared(int) test_dotvarexp_2(return shared C1* c)
{
    return (*c).value;
}

// Even taking the address (which offset the pointers) requires a load
shared(int)* test_dotvarexp_3(return shared C1* c)
{
    return &c.value;
}

// First level DotVarExp dereferencing
ref shared(int) test_dotvarexp_4(return shared ref C2 c)
{
    return c.c1.value;
}

// Second level DotVarExp dereferencing
ref shared(int) test_dotvarexp_5(return shared ref C3 c)
{
    return c.c1.c1.value;
}

class K1
{
    int value;
}

class K2
{
    shared K1 k1;
}

class K3
{
    K2 k2;
}

// A class is a pointer under the hood, and `shared` applies to the pointer
ref shared(int) test_dotvarexp_6(return shared K1 k)
{
    return k.value;
}

// Using `k.ke.k1` would be okay, but not `value`
ref shared(int) test_dotvarexp_7(return ref K3 k)
{
    return k.k2.k1.value;
}

// The returned value is `shared` so we shouldn't be able to access it
// The pointer could already be shared, e.g. by the ctor
ref shared(K1) test_newexp_1()
{
    return new shared(K2)().k1;
}

// Inference tests

// Fails because no `ref`
auto test_inference_1(return shared ref C3 c)
{
    return c;
}

// Fails because no `return` => Escapes
auto ref test_inference_2(shared C3 c)
{
    return c;
}

shared(Object) getSharedObject() { assert(0); }

// Fails because rvalue
auto ref test_inference_3()
{
    return getSharedObject();
}

// Fails because `const` conversion
auto ref Object test_inference_4(const return shared ref Object a)
{
    return a;
}

// https://issues.dlang.org/show_bug.cgi?id=23226
// Allow accessing non-shared `this`
struct BitRange
{
    int bits;
    void f()
    {
        this.bits++;
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/shared.d(3004): Error: cast from `void*` to `shared(int*)` not allowed in safe code
fail_compilation/shared.d(3005): Error: cast from `void*` to `shared(const(int*))` not allowed in safe code
fail_compilation/shared.d(3008): Error: cast from `shared(void*)` to `int*` not allowed in safe code
fail_compilation/shared.d(3009): Error: cast from `shared(void*)` to `shared(const(int*))` not allowed in safe code
---
*/

#line 3000

void test_casting_safe() @safe
{
    void *p;
    auto t1 = cast(shared(int*))p;
    auto t2 = cast(const(shared(int*)))p;

    shared void* s;
    auto x1 = cast(int*)s;
    auto x2 = cast(const(shared(int*)))s;
}

#line 3100

// https://issues.dlang.org/show_bug.cgi?id=23783

/*
TEST_OUTPUT:
---
fail_compilation/shared.d(3114): Error: direct access to shared `x` is not allowed, see `core.atomic`
fail_compilation/shared.d(3115): Error: direct access to shared `x` is not allowed, see `core.atomic`
---
*/

void test23783()
{
    shared int x = 3;
    assert(x == 3);
    bool b = x == 3;
}
