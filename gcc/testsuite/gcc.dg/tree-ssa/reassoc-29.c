/* PR tree-optimization/57322 */
/* { dg-do compile } */
/* { dg-options "-w -O1" } */
int a;

void f (void)
{
  char b;

  for (;; a++)
    {
      char *p = &b, *q;
      *q = b < 0 & !!*p;
    }
}
