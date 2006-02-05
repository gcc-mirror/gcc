// Test whether alternate 'asm' name is applied correctly to
// builtin in global namespace

// { dg-do compile }
// { dg-options "" }
// { dg-final { scan-assembler "fancy_printf" } }

extern "C" int printf(const char*, ...) __asm("_fancy_printf");

void foo() { printf("abc"); }
