/* Test -f*sanitize*=all */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-skip-if "" { *-*-* } { "-flto -fno-fat-lto-objects" } } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=all -fdump-tree-optimized" } */

int a[4];

int
f1 (int x, int y, int z)
{
  return a[x] + (1 << y) + (100 / z);
}

char *
f2 (int x)
{
  char *p = (char *) __builtin_calloc (64, 1);
  p[x] = 3;
  return p;
}

int
f3 (int x, int *y, double z, double w)
{
  int a[*y];
  if (x)
    __builtin_unreachable ();
  asm volatile ("" : : "r" (&a[0]));
  return z / w;
}

int
main ()
{
  return 0;
}

/* { dg-final { scan-tree-dump "__ubsan_\[a-z_\]*_abort" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__ubsan_\[a-z_\]*\[^et\] " "optimized" } } */
/* { dg-final { scan-tree-dump "UBSAN_CHECK_" "optimized" } } */
