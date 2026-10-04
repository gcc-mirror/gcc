/* { dg-do run } */
/* { dg-options "-O3 -fdump-tree-pcom-details-blocks -fno-tree-dominator-opts" } */
/* DOM folds the exit test to c + 1 == 1 from c's range, which leaves c a
   two-valued PHI rather than an induction variable and predcom nothing to
   do; keep it out of the way.  */

int b, f, d[5][2];
unsigned int c;

int
main ()
{
  for (c = 0; c < 2; c++)
    if (d[b + 3][c] & d[b + 4][c])
      if (f)
	break;
  return 0;
}

/* { dg-final { scan-tree-dump "Executing predictive commoning" "pcom" } } */
/* dom pass introduces one mismatch after simplfying mispredicted conditional
   on c being non-zero on first iteration.  This happens since c is global variable
   and needs alias analysis.  */
/* { dg-final { scan-tree-dump-times "Invalid sum" 1 "pcom" } } */
