/*
TEST_OUTPUT:
---
fail_compilation/call_function_type.d(18): Error: missing argument for parameter #1: `int`
fail_compilation/call_function_type.d(19): Error: cannot call `int(int)(3)` at compile time
---
*/

// This is a rare case where `dmd.expressionsem.functionParameters` catches a missing argument error,
// which is usually caught earlier by `TypeFunction.callMatch`, and had no test coverage yet.
// This was found while implementing named arguments and reduced from `vibe.internal.meta.traits`.

int f(int);

void m()
{
	alias FT = typeof(f);
	enum X0 = FT();
	enum X1 = FT(3);
}
