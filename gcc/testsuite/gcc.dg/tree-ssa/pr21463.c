/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiprop" } */

struct f
{
  int i;
};

int g(int i, int c, struct f *ff, int g)
{
  int *t;
  if (c)
    t = &i;
  else
    t = &ff->i;
  return *t;
}

/* { dg-final { scan-tree-dump-not "\\*t" "phiprop" } } */
/* { dg-final { cleanup-tree-dump "phiprop" } } */
