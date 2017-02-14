/* Verify that the 2 constant initializers are uniquized.  */

/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-gimple" } */

int lookup1 (int i)
{
  /* We use vectors long enough that piece-wise initialization is not
     reasonably preferable even for size (when including the constant
     vectors for initialization) for any target.  */
  int a[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
	      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 };
  return a[i];
}

int lookup2 (int i)
{
  int a[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
	      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 };
  return a[i+1];
}

/* { dg-final { scan-tree-dump-times "\[lL\]\\\$?C\\\.*0" 2 "gimple" } } */
