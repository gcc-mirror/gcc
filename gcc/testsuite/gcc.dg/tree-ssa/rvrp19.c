/* Make sure back edges initialize the range on entry cache properly
 * for "localized data".  This involves no loop analysis.*/
/* { dg-do compile } */
/* { dg-options "-O2  -fdump-tree-rvrp" } */

extern void kill (int k);

int main ()
{ 
  unsigned b = 0;
  int c, d = -8;
  /* The backedge to this loop provides b < 2 info. */
  for (; b < 2; b++)
    {
      for (c = 1; c; c--)
	d++;
    }
  /* Upon loop exit, we ought to know this.  */
  if (b > 2)
    kill (b);

  return 0;
}
/* { dg-final { scan-tree-dump-times "kill \\(" 0 "rvrp"} } */

