/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest|\tor" } } */

extern void foo(void);

void f(long long int a, long long int b)
{
  if (a + b == 0)
    foo();
}

void g(long long int a, long long int b)
{
  if (a + b >= 0)
    foo();
}
