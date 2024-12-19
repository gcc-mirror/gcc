/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ifcombine-details" } */

/* Check that we don't get confused by multiple conversions.  */

__attribute__((noipa))
int f(int *a,int *d)
{
  signed char b = *a;
  int c = b;
  *d = c; // This store is important even if otherwise unused
  if (c < 0 && (b&1))
    return 1;
  return 0;
}

int main()
{
  unsigned char t = 0x81;
  int x = t, y;
  int tt = f(&x, &y);
  if (!tt)
    __builtin_abort();
}

/* { dg-final { scan-tree-dump-not "optimizing" "ifcombine" } } */
