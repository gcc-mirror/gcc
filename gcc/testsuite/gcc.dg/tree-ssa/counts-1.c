/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
void unlikely () __attribute__ ((cold));
void unlikely2 () __attribute__ ((cold));

__attribute__ ((noinline)) void
i_am_also_unlikely (int a)
{
  if (a)
    unlikely ();
  else
    unlikely2 ();
}


void
i_am_also_unlikely2 (int a,int b)
{
  if (b)
    i_am_also_unlikely (a);
  else
    unlikely ();
}

void
i_am_not_unlikely (int a,int b,int c)
{
  if (c)
    __builtin_exit (0);
  i_am_also_unlikely2 (a,b);
}
/* Detect i_am_also_unlikely i_am_also_unlikely2 as unlikely.  */
/* { dg-final { scan-tree-dump "i_am_also_unlikely\[^\r\n\]*(unlikely executed)" "optimized"} } */
/* { dg-final { scan-tree-dump "i_am_also_unlikely2\[^\r\n\]*(unlikely executed)" "optimized"} } */
/* { dg-final { scan-tree-dump-not "i_am_not_unlikely\[^\r\n\]*(unlikely executed)" "optimized"} } */
