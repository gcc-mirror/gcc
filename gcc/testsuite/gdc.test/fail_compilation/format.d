/*
TEST_OUTPUT:
---
fail_compilation/format.d(101): Error: `pragma(printf)` function `printf1` must have `extern(C)` or `extern(C++)` linkage, not `extern(D)`
fail_compilation/format.d(102): Error: `pragma(printf)` function `printf2` must have signature `int printf2([parameters...], const(char)*, ...)` not `extern (C) int(const(int)*, ...)`
fail_compilation/format.d(103): Error: `pragma(printf)` function `printf3` must have signature `int printf3([parameters...], const(char)*, va_list)`
fail_compilation/format.d(104): Error: `pragma(printf)` function `printf4` must have signature `int printf4([parameters...], const(char)*, ...)` not `extern (C) int(const(char)*, int, ...)`
---
 */

#line 100

pragma(printf)           void printf1(const(char)*, ...);
pragma(printf) extern (C) int printf2(const(int )*, ...);
pragma(printf) extern (C) int printf3(const(char)*);
pragma(printf) extern (C) int printf4(const(char)*, int, ...);

pragma(printf) extern (C) int printf5(const(char)*, ...);
pragma(printf) extern (C) int printf6(immutable(char)*, ...);
pragma(printf) extern (C) int printf7(char*, ...);

/*
TEST_OUTPUT:
---
fail_compilation/format.d(203): Error: `pragma(printf)` function `vprintf1` must have `extern(C)` or `extern(C++)` linkage, not `extern(D)`
fail_compilation/format.d(204): Error: `pragma(printf)` function `vprintf2` must have signature `int vprintf2([parameters...], const(char)*, va_list)`
fail_compilation/format.d(205): Error: `pragma(printf)` function `vprintf3` must have signature `int vprintf3([parameters...], const(char)*, va_list)`
fail_compilation/format.d(206): Error: `pragma(printf)` function `vprintf4` must have signature `int vprintf4([parameters...], const(char)*, va_list)`
fail_compilation/format.d(207): Error: `pragma(printf)` function `vprintf5` must have C-style variadic `...` or `va_list` parameter
fail_compilation/format.d(208): Error: `pragma(scanf)` function `vscanf1` must have `extern(C)` or `extern(C++)` linkage, not `extern(Windows)`
fail_compilation/format.d(208): Error: `pragma(scanf)` function `vscanf1` must have signature `int vscanf1([parameters...], const(char)*, va_list)`
---
 */

#line 200

import core.stdc.stdarg;

pragma(printf)           void vprintf1(const(char)*, va_list);
pragma(printf) extern (C) int vprintf2(const(int )*, va_list);
pragma(printf) extern (C) int vprintf3(const(char)*);
pragma(printf) extern (C) int vprintf4(const(char)*, int, va_list);
pragma(printf) extern (C) int vprintf5(char*, int[] a...);
pragma(scanf)  extern (Windows) int vscanf1();

pragma(printf) extern (C) int vprintf5(const(char)*, va_list);
pragma(printf) extern (C) int vprintf6(immutable(char)*, va_list);
pragma(printf) extern (C) int vprintf7(char*, va_list);
