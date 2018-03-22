/* { dg-do compile } */
/* { dg-options "-O2 -w -fdump-tree-dom2" } */

extern void frob (void);
extern void frob (void);

void
rhs_to_tree (int x, int z)
{
  if (x >= 4)
    frob ();
  if (x >= 3)
    frob ();
}

/* The second conditional should change into a simple equality test.  */
/* { dg-final { scan-tree-dump-times "if \\(x_\[0-9\]+\\(D\\) == 3\\)" 1 "dom2"} } */

