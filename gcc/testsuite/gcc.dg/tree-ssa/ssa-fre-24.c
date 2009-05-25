/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra -fdump-tree-fre" } */

int foo(void)
{
  int a[16] = {};
  return a[3];
}

int bar(void)
{
  int a[16];
  __builtin_memset (a, 0, sizeof(a));
  return a[3];
}

struct X { int i; };
int baz(void)
{
  struct X a,b;
  a.i = 0;
  b = a;
  return b.i;
}

int bazzoo (void)
{
  struct X b, a = {};
  b = a;
  return b.i;
}

/* { dg-final { scan-tree-dump-times "= 0;" 5 "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
