// PR c++/18514
// Test whether alternate 'asm' name is applied correctly to
// builtin imported into namespace std.

// { dg-do compile }
// { dg-options "" }
// { dg-final { scan-assembler "fancy_printf" } }

extern "C" int printf(char*, ...) __asm("_fancy_printf");

namespace std { using ::printf; }

namespace std { void foo() { printf("abc"); } }
