/*
TEST_OUTPUT:
---
fail_compilation/parseStc2.d(11): Error: conflicting attribute `const`
fail_compilation/parseStc2.d(12): Error: conflicting attribute `@system`
fail_compilation/parseStc2.d(13): Error: conflicting attribute `@safe`
fail_compilation/parseStc2.d(14): Error: conflicting attribute `@trusted`
fail_compilation/parseStc2.d(15): Error: conflicting attribute `__gshared`
---
*/
immutable const void f4() {}
@safe @system void f4() {}
@trusted @safe void f4() {}
@system @trusted void f4() {}
shared __gshared f4() {}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc2.d(26): Error: redundant attribute `static`
fail_compilation/parseStc2.d(27): Error: redundant attribute `pure`
fail_compilation/parseStc2.d(28): Error: redundant attribute `@property`
fail_compilation/parseStc2.d(29): Error: redundant attribute `@safe`
---
*/
static static void f1() {}
pure nothrow pure void f2() {}
@property extern(C) @property void f3() {}
deprecated("") @safe @safe void f4() {}
@(1) @(1) void f5() {}  // OK

/*
TEST_OUTPUT:
---
fail_compilation/parseStc2.d(39): Error: redundant linkage `extern (C)`
fail_compilation/parseStc2.d(40): Error: conflicting linkage `extern (C)` and `extern (C++)`
---
*/
extern(C) extern(C) void f6() {}
extern(C) extern(C++) void f7() {}
extern(C++, foo) extern(C++, bar) void f8() {}  // OK

/*
TEST_OUTPUT:
---
fail_compilation/parseStc2.d(50): Error: redundant protection attribute `public`
fail_compilation/parseStc2.d(51): Error: conflicting protection attribute `public` and `private`
---
*/
public public void f9() {}
public private void f10() {}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc2.d(63): Error: redundant alignment attribute `align`
fail_compilation/parseStc2.d(64): Error: redundant alignment attribute `align(1)`
fail_compilation/parseStc2.d(65): Error: redundant alignment attribute `align(1)`
fail_compilation/parseStc2.d(66): Error: redundant alignment attribute `align`
fail_compilation/parseStc2.d(67): Error: redundant alignment attribute `align(2)`
---
*/
align    align    void f11() {}
align(1) align(1) void f12() {}
align    align(1) void f13() {}
align(1) align    void f14() {}
align(1) align(2) void f15() {}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc2.d(76): Error: redundant linkage `extern (System)`
fail_compilation/parseStc2.d(77): Error: conflicting linkage `extern (System)` and `extern (C++)`
---
*/
extern(System) extern(System) void f16() {}
extern(System) extern(C++) void f17() {}
