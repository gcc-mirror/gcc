/* PR middle-end/14470.  Similar to
   gcc.c-torture/execute/20040313-1.c, but with a compile time test to
   make sure the second if() is removed.  We should actually get rid
   of the first if() too, but we're not that smart yet.  */

/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-optimized" } */


extern void abort(void);

int main()
{
  int t[1025] = { 1024 }, d;

  d = 0;
  d = t[d]++;
  if (t[0] != 1025)
    abort();
  if (d != 1024)
    abort();
  return 0;
}

/* { dg-final { scan-tree-dump-times "if " 1 "optimized"} } */
