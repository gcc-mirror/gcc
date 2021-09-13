/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

extern void foo(void);
int a, c, *f, **d = &f;
char b;
int main()
{
  if (a) {
    b = 0;
    int *g = &c;
    *g = 0;
    f = *d;
    *d = f;
    if ((2 ^ b) == 0)
      foo();
  }
  return 0;
}

/* { dg-final { scan-tree-dump-not "foo" "fre1" } } */
