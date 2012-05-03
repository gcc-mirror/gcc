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
  /* And here, the possibly not returning call in the same basic-block
     comes after the trapping i/j.  */
  res += i/j;
  bar ();
  return res;
}

/* { dg-final { scan-tree-dump-times "# prephitmp" 2 "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
