/* { dg-do compile } */
/* { dg-options "-g -O3 -fdump-tree-optimized -fvar-tracking-assignments -fno-selective-scheduling -fno-selective-scheduling2 -fno-ipa-vrp" } */

int __attribute__((noinline))
f1 (int i)
{
  char a[i + 1];
  char b[i + 2];
  b[1] = 3;
  a[0] = 5;
  return a[0] + b[1];
}

int
main ()
{
  volatile int j;
  int x = 5;
  j = f1 (x);
  return 0;
}

/* One debug source bind is generated for the parameter, and one to describe the
   sizes of a and b.  */
/* { dg-final { scan-tree-dump-times " s=> i" 2 "optimized" } } */

