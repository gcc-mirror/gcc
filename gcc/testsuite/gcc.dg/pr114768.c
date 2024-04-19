/* PR rtl-optimization/114768 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-final" } */
/* { dg-final { scan-rtl-dump "\\\(mem/v:" "final" } } */

void
foo (int *p)
{
  *p = *(volatile int *) p;
}
