/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-blocks-details" } */

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
/* { dg-final { scan-tree-dump-not "goto &L0" "optimized" } } */

/* There should not be any abnormal edges as DOM removed the
   computed goto.  */

/* { dg-final { scan-tree-dump-not "ABNORMAL" "optimized" } } */

/* And verify that we have fixed the fallthru flag as well. 
   After DOM we will have two fallthru edges (e->0, 0->1),
   but in the dump files we mention the 2->3 two times.  So
   scan for 3 instances of "FALLTHRU".  */

/* { dg-final { scan-tree-dump-times "FALLTHRU" 3 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
