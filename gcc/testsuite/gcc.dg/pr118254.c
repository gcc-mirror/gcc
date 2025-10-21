/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void foo(void);
int il=1000;

int m1(void)
{
  short t = il;
  unsigned t1 = t;
  if (t1 == 0) {
    char b = t1;
    if (b != 1)
      return 0;
    foo();
  }
  return 0;
}

int m2(void)
{
  short t = il;
  unsigned t1 = t;
  if (t1 == 0) {
    char b = il;
    if (b != 1)
      return 0;
    foo();
  }
  return 0;
}

/* { dg-final { scan-tree-dump-not "foo"  "evrp" } } */

