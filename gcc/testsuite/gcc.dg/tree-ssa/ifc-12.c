/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-ifcvt-stats" } */
/* { dg-require-visibility "" } */

struct st
{
  int a[1024];
  int b[1024];
};

struct st s = {0};
int foo (int x)
{
  int i;
  struct st *p = &s;

  for (i = 0; i < 1024; i++)
    {
      if (x > i)
	p->a[i] = p->b[i];
    }

  return 0;
}
/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
