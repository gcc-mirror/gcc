/* Test that -fvisibility works. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-options "-fvisibility=hidden" } */
/* { dg-final { scan-assembler "\\.hidden.*foo" } } */

void foo();

void foo() { }
