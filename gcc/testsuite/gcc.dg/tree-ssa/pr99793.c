/* { dg-do compile } */
/* { dg-options "-O -fstrict-aliasing -fdump-tree-optimized" } */

extern void foo(void);
static int a, *b = &a, c, *d = &c;
int main()
{
  int **e = &d;
  if (!((unsigned)((*e = d) == 0) - (*b = 1)))
    foo();
  return 0;
}

/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
