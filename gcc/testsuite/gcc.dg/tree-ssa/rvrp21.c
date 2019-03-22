/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-vrp -fdisable-tree-evrp -ftree-rvrp -fdump-tree-rvrp" } */

signed char arr[240];
void foo (void)
{

  unsigned short i, length = 200;

  for (i = 1; (int)i < (length - 1); i++)
    arr[i] = -1;
}

/* NOTE: We currently can't get this with adjust_range_with_scev()
   because the definition of i_12 is not in the loop header.

   I don't know enough about loop/scev to understand how it works, but
   all other users of adjust_range_with_scev() are predicated with:

      if (scev_initialized_p ()
          && interesting
          && (l = loop_containing_stmt (phi))
          && l->header == gimple_bb (phi))

   This suggests that the gimple statement (phi) has to be in the loop
   header.  A comment in adjust_range_with_scev also suggests this:

     We are only entering here for loop header PHI nodes, so using
     the number of latch executions is the correct thing to use.  */

/* i_12 = i_6 + 1... i_6 should be [1, 198] on the back edge,  */
/* i_12 should be [2, 199].				   */
/* { dg-final { scan-tree-dump "\\\[2, 199\\\]" "rvrp" } } */
