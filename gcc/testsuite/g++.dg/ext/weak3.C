// PR c++/20961
// Test for #pragma weak and __attribute__((weak)) being used together. 
// { dg-do compile }
// { dg-require-weak "" }
// { dg-options "" }

// { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?_Z3foov" } }

int foo ();
#pragma weak foo

int
__attribute__((weak))
foo ()
{
  return 0;
}
