/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */

blah (int code1, int code2)
{
  int i;
  int n_sets;

  n_sets = (int) (code1 == 32);
  if (code2 == 64) goto L2; else goto L3;

L2:
  aa ();

L3:
  if (n_sets > 1) goto L4; else goto L10;

L4:
  aos ();
  i = 0;
  goto L24;

L10:
  if (n_sets > 0) goto L25; else goto L8;

L25:
  i = 0;

L24:
  aob ();
  i = i + 1;
  if (i < n_sets) goto L24; else goto L8;

L8:
  return;
}



/* The n_sets > 0 test can be simplified into n_sets == 1 since the
   only way to reach the test is when n_sets <= 1, and the only value
   which satisfies both conditions is n_sets == 1.  */
/* { dg-final { scan-tree-dump-times "Simplified relational" 1 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */

