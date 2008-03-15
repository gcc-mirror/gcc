/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

static const int x;

int test1 (void)
{
  char *p = "hello";
  int i = x;
  i = i + 5;
  return p[i];
}

int test2 (void)
{
  int i = x;
  i = i + 5;
  return "hello"[i];
}

/* { dg-final { scan-tree-dump-times "return 0;" 2 "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
