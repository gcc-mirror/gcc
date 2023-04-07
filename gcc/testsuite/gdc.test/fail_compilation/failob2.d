// REQUIRED_ARGS: -preview=dip1021

/*
TEST_OUTPUT:
---
fail_compilation/failob2.d(105): Error: variable `failob2.foo1!int.foo1.p` has undefined state and cannot be read
fail_compilation/failob2.d(105): Error: variable `failob2.foo1!int.foo1.p` is returned but is Undefined
fail_compilation/failob2.d(124): Error: template instance `failob2.foo1!int` error instantiating
fail_compilation/failob2.d(111): Error: variable `failob2.foo2!int.foo2.p` has undefined state and cannot be read
fail_compilation/failob2.d(111): Error: variable `failob2.foo2!int.foo2.p` is returned but is Undefined
fail_compilation/failob2.d(125): Error: template instance `failob2.foo2!int` error instantiating
fail_compilation/failob2.d(119): Error: variable `failob2.foo3!int.foo3.p` has undefined state and cannot be read
fail_compilation/failob2.d(119): Error: variable `failob2.foo3!int.foo3.p` is returned but is Undefined
fail_compilation/failob2.d(126): Error: template instance `failob2.foo3!int` error instantiating
---
*/

#line 100

@live
T* foo1(T)()
{
    T* p = void;
    return p;
}

template foo2(T) {
    @live T* foo2() {
        T* p = void;
        return p;
    }
}

@live
template foo3(T) {
    T* foo3() {
        T* p = void;
        return p;
    }
}

void test1() {
    foo1!int();
    foo2!int();
    foo3!int();
}

/*
TEST_OUTPUT:
---
fail_compilation/failob2.d(205): Error: variable `failob2.foo4!int.foo4.p` is not disposed of before return
fail_compilation/failob2.d(209): Error: template instance `failob2.foo4!int` error instantiating
---
*/

#line 200

void* alloc(size_t);

@live void foo4(T)()
{
    auto p = alloc(4);
}

void test2() {
    foo4!int();
}
