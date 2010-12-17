/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiprop-details" } */

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

/* { dg-final { scan-tree-dump-times "Inserting PHI for result of load" 1 "phiprop" } } */
/* { dg-final { cleanup-tree-dump "phiprop" } } */
