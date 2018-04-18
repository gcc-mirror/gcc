/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-thread1-details" } */

/* Test that we can thread multiple paths that start from the same BB.  */

int never_exec;
int stuff;

void __GIMPLE (startwith ("thread1")) foo (int x, int fork)
{
  /* There should be two paths reaching bb_5 from here that must be threaded:
	2->3->5
	2->3->4->5.  */
 bb_2:
  if (x == 24)
    goto bb_3;
  else
    goto bb_7;

 bb_3:
  if (fork)
    goto bb_5;
  else
    goto bb_4;

 bb_4:
  stuff = 1;

 bb_5:
  if (x == 42)
    goto bb_6;
  else
    goto bb_7;

 bb_6:
    never_exec = 1;

 bb_7:
      return;
}

/* { dg-final { scan-tree-dump-times "Registering FSM jump thread: \\(2, 3\\)" 2 "thread1" } } */
