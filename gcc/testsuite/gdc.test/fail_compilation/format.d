/*
TEST_OUTPUT:
---
fail_compilation/format.d(101): Error: function `format.printf1` `pragma(printf)` functions must be `extern(C) void printf1([parameters...], const(char)*, ...)` not `void(const(char)*, ...)`
fail_compilation/format.d(102): Error: function `format.printf2` `pragma(printf)` functions must be `extern(C) int printf2([parameters...], const(char)*, ...)` not `extern (C) int(const(int)*, ...)`
fail_compilation/format.d(103): Error: function `format.printf3` `pragma(printf)` functions must be `extern(C) int printf3([parameters...], const(char)*, va_list)`
fail_compilation/format.d(104): Error: function `format.printf4` `pragma(printf)` functions must be `extern(C) int printf4([parameters...], const(char)*, ...)` not `extern (C) int(const(char)*, int, ...)`
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
fail_compilation/format.d(203): Error: function `format.vprintf1` `pragma(printf)` functions must be `extern(C) void vprintf1([parameters...], const(char)*, va_list)`
fail_compilation/format.d(204): Error: function `format.vprintf2` `pragma(printf)` functions must be `extern(C) int vprintf2([parameters...], const(char)*, va_list)`
fail_compilation/format.d(205): Error: function `format.vprintf3` `pragma(printf)` functions must be `extern(C) int vprintf3([parameters...], const(char)*, va_list)`
fail_compilation/format.d(206): Error: function `format.vprintf4` `pragma(printf)` functions must be `extern(C) int vprintf4([parameters...], const(char)*, va_list)`
---
 */

#line 200

import core.stdc.stdarg;

pragma(printf)           void vprintf1(const(char)*, va_list);
pragma(printf) extern (C) int vprintf2(const(int )*, va_list);
pragma(printf) extern (C) int vprintf3(const(char)*);
pragma(printf) extern (C) int vprintf4(const(char)*, int, va_list);

pragma(printf) extern (C) int vprintf5(const(char)*, va_list);
pragma(printf) extern (C) int vprintf6(immutable(char)*, va_list);
pragma(printf) extern (C) int vprintf7(char*, va_list);
