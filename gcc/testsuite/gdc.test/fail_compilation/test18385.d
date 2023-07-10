/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/test18385.d(13): Error: function `test18385.foo` cannot overload `extern(C)` function at fail_compilation/test18385.d(12)
---
*/


extern (C):

void foo(int) { }
void foo(double) { }

struct S
{
    static void foo(int) {}
    static void foo(double) {}
}

void foo2(int) { }
extern(D) void foo2(double) { } // OK as it has a different mangling

void foo3(int) { }
void foo3(double); // duplicate declarations are allowed

void foo4();
void foo4() { }

extern(D) void foo5();
extern(D) void foo5() { }
