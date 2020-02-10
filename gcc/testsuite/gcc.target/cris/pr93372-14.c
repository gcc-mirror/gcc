/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "\tcmp|\ttest" 2 } } */

extern void foo(void);

void f(long long int a, long long int b)
{
  /* Trivial check that we don't eliminate a non-eliminable compare. */
  if (a + b <= 0)
    foo();
}
