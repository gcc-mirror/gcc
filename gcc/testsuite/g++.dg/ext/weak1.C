// Test for #pragma weak where the weak alias symbol isn't declared,
// although the symbol it is an alias for is defined in the
// translation unit.  Bug 7544.
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-do compile }
// { dg-require-weak "" }
// { dg-require-alias "" }
// { dg-options "-fno-common" }

// { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?bar1" } } 

#pragma weak bar1 = foo1
extern "C" void foo1 (void) {}
