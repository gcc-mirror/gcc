/* PR rtl-optimization/56847 */
/* { dg-do compile { target pie } } */
/* { dg-options "-O2 -fpie" } */

struct S { long int a, b; } e;
__thread struct S s;

void
foo (void)
{
  s = e;
}
