/* { dg-do compile } */
/* { dg-options "-O1 -fivcanon -funroll-loops -fdump-tree-ivcanon-details" } */

void xxx(void)
{
  int x = 45;

  while (x >>= 1)
    foo ();
}

/* We should be able to find out that the loop iterates four times and unroll it completely.  */

/* { dg-final { scan-tree-dump-times "Added canonical iv to loop 1, 4 iterations" 1 "ivcanon"} } */
/* { dg-final { scan-assembler-times "foo" 5} } */


