/* PR rtl-optimization/88870 */
/* { dg-do compile } */
/* { dg-require-effective-target nonlocal_goto } */
/* { dg-options "-O1 -fexceptions -fnon-call-exceptions -ftrapv -fno-tree-dominator-opts" } */

int a, b;

void
foo (int *x)
{
  int c = 0;
  {
    int d;
    x = &c;
    for (;;)
      {
        x = &d;
        b = 0;
        d = c + 1;
        b = c = 1;
        ++a;
      }
  }
}
