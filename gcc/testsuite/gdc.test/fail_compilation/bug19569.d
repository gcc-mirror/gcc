/*
TEST_OUTPUT:
---
fail_compilation/bug19569.d(70): Error: `bug19569.test0` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(56):     `bug19569.test0()`
and:
fail_compilation/bug19569.d(57):     `bug19569.test0()`
fail_compilation/bug19569.d(71): Error: `bug19569.test1` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(59):     `bug19569.test1()`
and:
fail_compilation/bug19569.d(60):     `bug19569.test1()`
fail_compilation/bug19569.d(72): Error: `bug19569.test2` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(62):     `bug19569.test2!().test2()`
and:
fail_compilation/bug19569.d(63):     `bug19569.test2!().test2()`
fail_compilation/bug19569.d(73): Error: `bug19569.test3` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(65):     `bug19569.test3!().test3()`
and:
fail_compilation/bug19569.d(66):     `bug19569.test3!().test3()`
fail_compilation/bug19569.d(78): Error: `bug19569.test0` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(56):     `bug19569.test0()`
and:
fail_compilation/bug19569.d(57):     `bug19569.test0()`
fail_compilation/bug19569.d(79): Error: `bug19569.test1` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(59):     `bug19569.test1()`
and:
fail_compilation/bug19569.d(60):     `bug19569.test1()`
fail_compilation/bug19569.d(80): Error: `bug19569.test2` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(62):     `bug19569.test2!().test2()`
and:
fail_compilation/bug19569.d(63):     `bug19569.test2!().test2()`
fail_compilation/bug19569.d(81): Error: `bug19569.test3` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(65):     `bug19569.test3!().test3()`
and:
fail_compilation/bug19569.d(66):     `bug19569.test3!().test3()`
fail_compilation/bug19569.d(86): Error: `bug19569.test0` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(56):     `bug19569.test0()`
and:
fail_compilation/bug19569.d(57):     `bug19569.test0()`
fail_compilation/bug19569.d(87): Error: `bug19569.test1` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(59):     `bug19569.test1()`
and:
fail_compilation/bug19569.d(60):     `bug19569.test1()`
fail_compilation/bug19569.d(88): Error: `bug19569.test2` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(62):     `bug19569.test2!().test2()`
and:
fail_compilation/bug19569.d(63):     `bug19569.test2!().test2()`
fail_compilation/bug19569.d(89): Error: `bug19569.test3` called with argument types `()` matches multiple overloads exactly:
fail_compilation/bug19569.d(65):     `bug19569.test3!().test3()`
and:
fail_compilation/bug19569.d(66):     `bug19569.test3!().test3()`
---
*/


void test0();
void test0() nothrow;

void test1();
void test1() @nogc;

void test2()();
void test2()() nothrow;

void test3()();
void test3()() @nogc;

void attr0()
{
    test0();
    test1();
    test2();
    test3();
}

void attr1() @nogc
{
    test0();
    test1();
    test2();
    test3();
}

void attr3() nothrow @nogc
{
    test0();
    test1();
    test2();
    test3();
}
