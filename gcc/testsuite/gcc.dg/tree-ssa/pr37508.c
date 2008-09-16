/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

struct foo1 {
  int i:1;
};
struct foo2 {
  unsigned i:1;
};

int test1 (struct foo1 *x)
{
  if (x->i == 0)
    return 1;
  else if (x->i == -1)
    return 1;
  return 0;
}

int test2 (struct foo2 *x)
{
  if (x->i == 0)
    return 1;
  else if (x->i == -1)
    return 1;
  return 0;
}

int test3 (struct foo1 *x)
{
  if (x->i == 0)
    return 1;
  else if (x->i == 1)
    return 1;
  return 0;
}

int test4 (struct foo2 *x)
{
  if (x->i == 0)
    return 1;
  else if (x->i == 1)
    return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-times "Folding" 4 "vrp1" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
