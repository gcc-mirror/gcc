/* Test that #pragma GCC visibility works. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.hidden.*foo" } } */

#pragma GCC visibility push(hidden)
void foo();
#pragma GCC visibility pop

void foo() { }
