/*
DISABLED: win32 win64 freebsd32 openbsd32 linux32 osx32
TEST_OUTPUT:
---
fail_compilation/safeprintf.d(20): Error: `@safe` function `safeprintf.func` cannot call `@system` function `safeprintf.printf`
fail_compilation/safeprintf.d(15):        `safeprintf.printf` is declared here
fail_compilation/safeprintf.d(21): Error: `@safe` function `safeprintf.func` cannot call `@system` function `safeprintf.printf`
fail_compilation/safeprintf.d(15):        `safeprintf.printf` is declared here
fail_compilation/safeprintf.d(22): Error: `@safe` function `safeprintf.func` cannot call `@system` function `safeprintf.printf`
fail_compilation/safeprintf.d(15):        `safeprintf.printf` is declared here
fail_compilation/safeprintf.d(22): Deprecation: format specifier `"%Z"` is invalid
---
*/

extern (C) @system pragma(printf) int printf(const(char)* format, ...);

@safe void func(int i, char* s, dchar* d)
{
    printf("i: %d\n", i);
    printf("s: %s\n", s);
    printf("s: %S\n", d);
    printf("s: %Z\n", s);
}
