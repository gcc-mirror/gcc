/* { dg-do run } */
/* { dg-options "-fdump-tree-alias" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-fno-fat-lto-objects" } { "" } } */

struct Foo {
  int **p;
  int **q;
};

int __attribute__((noinline))
bar (void)
{
  struct Foo f;
  int j, i = 1;
  char *p;
  int *x = &i;
  int *y = &j;
  f.p = &y;
  f.q = &x;
  p = (char *)&f;
  for (j = 0; j < sizeof (int *); ++j)
    p++;
  return ***(int ***)p;
}
extern void abort (void);
int main()
{
  if (bar () != 1)
    abort ();
  return 0;
}

/* In theory = { i } is the correct solution.  But it's not easy to scan
   for that reliably, so just use what we create now.  */
/* { dg-final { scan-tree-dump "= { i j }" "alias" } } */
