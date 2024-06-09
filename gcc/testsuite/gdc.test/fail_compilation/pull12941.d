/* TEST_OUTPUT:
---
fail_compilation/pull12941.d(110): Error: `pull12941.foo` called with argument types `(int*)` matches both:
fail_compilation/pull12941.d(101):     `pull12941.foo(return ref scope int* p)`
and:
fail_compilation/pull12941.d(102):     `pull12941.foo(return out scope int* p)`
fail_compilation/pull12941.d(111): Error: function `bar` is not callable using argument types `(int)`
fail_compilation/pull12941.d(111):        cannot pass argument `1` of type `int` to parameter `return scope int* p`
fail_compilation/pull12941.d(104):        `pull12941.bar(return scope int* p)` declared here
fail_compilation/pull12941.d(112): Error: function `abc` is not callable using argument types `(int)`
fail_compilation/pull12941.d(112):        cannot pass rvalue argument `1` of type `int` to parameter `return ref int* p`
fail_compilation/pull12941.d(105):        `pull12941.abc(return ref int* p)` declared here
---
 */

/*********************************/
// Tests for https://github.com/dlang/dmd/pull/12941

#line 100

int* foo(ref scope return int* p);
int* foo(out scope return int* p);

int* bar(scope return int* p);
int* abc(ref return int* p);

void test()
{
    int* p;
    foo(p);
    bar(1);
    abc(1);
}
