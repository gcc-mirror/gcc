/*
TEST_OUTPUT:
---
fail_compilation/attributediagnostic.d(16): Error: `@safe` function `attributediagnostic.layer2` cannot call `@system` function `attributediagnostic.layer1`
fail_compilation/attributediagnostic.d(18):        which calls `attributediagnostic.layer0`
fail_compilation/attributediagnostic.d(20):        which calls `attributediagnostic.system`
fail_compilation/attributediagnostic.d(22):        which was inferred `@system` because of:
fail_compilation/attributediagnostic.d(22):        `asm` statement is assumed to be `@system` - mark it with `@trusted` if it is not
fail_compilation/attributediagnostic.d(17):        `attributediagnostic.layer1` is declared here
---
*/

// Issue 17374 - Improve inferred attribute error message
// https://issues.dlang.org/show_bug.cgi?id=17374

auto layer2() @safe { layer1(); }
auto layer1() { layer0(); }
auto layer0() { system(); }

auto system()
{
	asm {}
}
