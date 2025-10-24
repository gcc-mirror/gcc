/* Verify pattern initialization for structure type automatic variables with
   padding.  */
/* { dg-do compile } */
/* SRA should be turned off as it can fully scalarize var as the padding is not touched. */
/* { dg-options "-O -ftrivial-auto-var-init=pattern -fno-tree-sra" } */

struct test_aligned {
        int internal1;
        long long internal2;
} __attribute__ ((aligned(64)));

int foo ()
{
  struct test_aligned var;
  return var.internal1;
}

/* { dg-final { scan-assembler-times {stp\tq[0-9]+, q[0-9]+,} 2 } } */
