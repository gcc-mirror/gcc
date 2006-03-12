/* PR middle-end/14470.  Similar to
   gcc.c-torture/execute/20040313-1.c, but with a compile time test to
   make sure the second if() is removed.  */
/* Update: We now remove both ifs.  Whee. */

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

/* { dg-final { scan-tree-dump-times "if " 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
