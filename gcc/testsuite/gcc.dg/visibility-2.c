/* Test that visibility attribute on declaration extends to definition. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.hidden.*foo" } } */

void 
__attribute__((visibility ("hidden")))
foo();

void foo() { }
