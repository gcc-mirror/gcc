// REQUIRED_ARGS: -m32
/*
TEST_OUTPUT:
---
fail_compilation/diag7420.d(21): Error: static variable `x` cannot be read at compile time
fail_compilation/diag7420.d(21):        while evaluating: `static assert(x < 4)`
fail_compilation/diag7420.d(22): Error: static variable `y` cannot be read at compile time
fail_compilation/diag7420.d(22):        called from here: `__equals(y, "abc")`
fail_compilation/diag7420.d(22):        while evaluating: `static assert(y == "abc")`
fail_compilation/diag7420.d(23): Error: static variable `y` cannot be read at compile time
fail_compilation/diag7420.d(23):        while evaluating: `static assert(cast(ubyte[])y != null)`
fail_compilation/diag7420.d(24): Error: static variable `y` cannot be read at compile time
fail_compilation/diag7420.d(24):        while evaluating: `static assert(cast(int)y[0] == 1)`
fail_compilation/diag7420.d(25): Error: static variable `y` cannot be read at compile time
fail_compilation/diag7420.d(25):        while evaluating: `static assert(y[0..1].length == 1u)`
---
*/

int x = 2;
char[] y = "abc".dup;
static assert(x < 4);
static assert(y == "abc");
static assert(cast(ubyte[])y != null);
static assert(y[0] == 1);
static assert(y[0..1].length == 1);
