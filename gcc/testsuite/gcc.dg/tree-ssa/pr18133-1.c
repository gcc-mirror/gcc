/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-blocks" } */

void foo (void)
{
void *p;
p = &&L0;
goto *p;
L0:
return;
}

/* The goto &L0 should have been optimized away during CFG
   cleanups.  */
/* { dg-final { scan-tree-dump-times "goto &L0" 0 "optimized" } } */

/* There should not be any abnormal edges as DOM removed the
   computed goto.  */

/* { dg-final { scan-tree-dump-times "ab" 0 "optimized" } } */

/* And verify that we have fixed the fallthru flag as well. 
   After DOM we will have two fallthru edges (e->0, 0->1),
   but in the dump files we mention the 0->1 two times.  So
   scan for 3 instances of "fallthru".  */

/* { dg-final { scan-tree-dump-times "fallthru" 3 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
