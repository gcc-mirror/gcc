/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-useless" } */

void
foo (void)
{ 
  int i, a; 
  for (i = 0; i < 10; i++) 
    { a = i; } 
}

/* There should be three gotos in the dump.  If one was removed
   in the loop exit condition, it would be re-introduced during
   GIMPLE lowering, at the cost of an extra statement, label,
   and basic block.  */
/* { dg-final { scan-tree-dump-times "goto" 3 "useless"} } */ 
