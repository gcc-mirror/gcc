/* { dg-do compile } */
/* { dg-options "-O1 -ftree-loop-ivcanon -funroll-loops -fdump-tree-ivcanon-details -fdump-tree-cunroll-details -fdump-tree-vars" } */

/* On 31-bit S/390 the function address will be stored (once) in the literal pool,
   so scan-assembler-times "foo" will return 1 even if the loop is fully unrolled.
   -msmall-exec avoids this by enabling a call instruction with immediate operand.  */
/* { dg-options "-O1 -ftree-loop-ivcanon -funroll-loops -fdump-tree-ivcanon-details -fdump-tree-cunroll-details -fdump-tree-vars -msmall-exec" { target s390-*-* } } */

/* On Darwin, we call extern functions via a stub in PIC mode which is default and
   the stub is named after the function.  To avoid this we use -static to go out
   of PIC mode.  */
/* { dg-options "-O1 -ftree-loop-ivcanon -funroll-loops -fdump-tree-ivcanon-details -fdump-tree-cunroll-details -fdump-tree-vars -static" { target *-*-darwin* } } */

void xxx(void)
{
  int x = 45;

  while (x >>= 1)
    foo ();
}

/* We should be able to find out that the loop iterates four times and unroll it completely.  */

/* { dg-final { scan-tree-dump-times "Added canonical iv to loop 1, 4 iterations" 1 "ivcanon"} } */
/* { dg-final { scan-tree-dump-times "Unrolled loop 1 completely" 1 "cunroll"} } */
/* { dg-final { scan-tree-dump-times "foo" 5 "vars"} } */
/* { dg-final { scan-assembler-times "foo" 5} } */


