/*
TEST_OUTPUT:
---
fail_compilation/parseStc3.d(10): Deprecation: redundant attribute `pure`
fail_compilation/parseStc3.d(11): Deprecation: redundant attribute `nothrow`
fail_compilation/parseStc3.d(12): Deprecation: redundant attribute `@nogc`
fail_compilation/parseStc3.d(13): Deprecation: redundant attribute `@property`
---
*/
pure      void f1() pure      {}
nothrow   void f2() nothrow   {}
@nogc     void f3() @nogc     {}
@property void f4() @property {}
//ref     int  f5() ref       { static int g; return g; }

/*
TEST_OUTPUT:
---
fail_compilation/parseStc3.d(24): Deprecation: redundant attribute `@safe`
fail_compilation/parseStc3.d(25): Deprecation: redundant attribute `@system`
fail_compilation/parseStc3.d(26): Deprecation: redundant attribute `@trusted`
---
*/
@safe     void f6() @safe    {}
@system   void f7() @system  {}
@trusted  void f8() @trusted {}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc3.d(39): Error: conflicting attribute `@system`
fail_compilation/parseStc3.d(40): Error: conflicting attribute `@trusted`
fail_compilation/parseStc3.d(41): Error: conflicting attribute `@safe`
fail_compilation/parseStc3.d(42): Error: conflicting attribute `@trusted`
fail_compilation/parseStc3.d(43): Error: conflicting attribute `@safe`
fail_compilation/parseStc3.d(44): Error: conflicting attribute `@system`
---
*/
@safe     void f9()  @system  {}
@safe     void f10() @trusted {}
@system   void f11() @safe    {}
@system   void f12() @trusted {}
@trusted  void f13() @safe    {}
@trusted  void f14() @system  {}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc3.d(59): Error: conflicting attribute `@system`
fail_compilation/parseStc3.d(59): Error: conflicting attribute `@trusted`
fail_compilation/parseStc3.d(60): Error: conflicting attribute `@system`
fail_compilation/parseStc3.d(60): Deprecation: redundant attribute `@system`
fail_compilation/parseStc3.d(61): Error: conflicting attribute `@safe`
fail_compilation/parseStc3.d(61): Deprecation: redundant attribute `@system`
fail_compilation/parseStc3.d(62): Error: conflicting attribute `@safe`
fail_compilation/parseStc3.d(62): Deprecation: redundant attribute `@trusted`
---
*/
@safe @system  void f15() @trusted {}
@safe @system  void f16() @system  {}
@system @safe  void f17() @system  {}
@trusted @safe void f18() @trusted {}
