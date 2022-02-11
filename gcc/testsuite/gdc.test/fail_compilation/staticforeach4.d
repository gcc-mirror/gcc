/*
REQUIRED_ARGS: -verrors=context
TEST_OUTPUT:
---
fail_compilation/staticforeach4.d(16): Error: index type `byte` cannot cover index range 0..257
static foreach (byte a, int b; data) { }
                               ^
fail_compilation/staticforeach4.d(17): Error: index type `byte` cannot cover index range 0..257
static foreach (byte a, int b; fn()) { }
                                 ^
---
*/
immutable int[257] data = 1;
int[257] fn() { return data; }

static foreach (byte a, int b; data) { }
static foreach (byte a, int b; fn()) { }
