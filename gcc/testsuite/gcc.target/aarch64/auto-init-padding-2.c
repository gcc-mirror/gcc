/* Verify pattern initialization for structure type automatic variables with
   padding.  */
/* { dg-do compile } */
/* { dg-options "-O -ftrivial-auto-var-init=pattern" } */

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
