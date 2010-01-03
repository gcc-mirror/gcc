/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

int foo (int i, int j, int b)
{
  int res = 0;
  if (b)
    res = i/j;
  /* We should insert the possibly trapping i/j.  */
  res += i/j;
  return res;
}

extern void bar (void);
int foo2 (int i, int j, int b)
{
  int res = 0;
  if (b)
    res = i/j;
  /* But we fail so here because of the possibly not returning
     call in the same basic-block.  */
  res += i/j;
  bar ();
  return res;
}

/* { dg-final { scan-tree-dump-times "# prephitmp" 1 "pre" } } */
/* { dg-final { scan-tree-dump-times "# prephitmp" 2 "pre" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
