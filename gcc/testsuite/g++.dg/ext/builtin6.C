// PR c++/19044
// Verify that alternate asm name for builtin named "foo" also gets
// applied to its sibling "__builtin_foo".

// { dg-do compile }
// { dg-final { scan-assembler "fancy_sin" } }

extern "C" double sin(double) __asm("_fancy_sin");

double foo(double x) { return __builtin_sin(x); }

