/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-thread1-details" } */

/* Test that we thread multiple range-based paths out of bb2.  */

int greather_than_10;
int less_than_12;
int equals_11;

void __GIMPLE (startwith ("thread1")) foo (int v)
{
 bb_2:
  if (v > 10)
    goto bb_3;
  else
    goto bb_4;

 bb_3:
  greather_than_10 = 1;

 bb_4:
  if (v <= 11)
    goto bb_5;
  else
    goto bb_7;

 bb_5:
  less_than_12 = 1;
  if (v == 11)
    goto bb_6;
  else
    goto bb_7;

 bb_6:
  equals_11 = 1;

 bb_7:
  return;
}

/* { dg-final { scan-tree-dump-times "Registering FSM jump thread: \\(2, " 3 "thread1" } } */
