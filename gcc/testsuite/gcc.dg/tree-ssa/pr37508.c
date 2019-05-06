/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-fre -fdump-tree-vrp1" } */

struct foo1 {
  int i:1;
};
struct foo2 {
  unsigned i:1;
};

int test1 (struct foo1 *x)
{
  int i = x->i;
  if (i == 0)
    return 1;
  else if (i == -1)
    return 1;
  return 0;
}

int test2 (struct foo2 *x)
{
  if (x->i == 0)
    return 1;
  else if (x->i == -1) /* This test is already folded to false by ccp1.  */
    return 1;
  return 0;
}

int test3 (struct foo1 *x)
{
  if (x->i == 0)
    return 1;
  else if (x->i == 1) /* This test is already folded to false by fold.  */
    return 1;
  return 0;
}

int test4 (struct foo2 *x)
{
  unsigned int i = x->i;
  if (i == 0)
    return 1;
  else if (i == 1)
    return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-times "if" 2 "vrp1" } } */
