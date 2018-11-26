/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

int f();
int d;
void c()
{
  for (;;)
    {
      f();
      int (*fp)() __attribute__((const)) = (void *)f;
      d = fp();
    }
}

/* We shouldn't ICE and hoist the const call of fp out of the loop.  */
/* { dg-final { scan-tree-dump "Eliminated: 1" "pre" } } */
