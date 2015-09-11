/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

int i;
int *p = &i;
static __PTRDIFF_TYPE__ d = p - (p + 1);

void
foo (void)
{
  int *q = &i;
  static __PTRDIFF_TYPE__ e = q - (q + 1);
}

/* { dg-final { scan-tree-dump-not " - " "original" } } */
/* { dg-final { scan-tree-dump-not " \\+ " "original" } } */
