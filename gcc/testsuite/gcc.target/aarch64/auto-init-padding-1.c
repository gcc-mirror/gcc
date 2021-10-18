/* Verify zero initialization for structure type automatic variables with
   padding.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero" } */

struct test_aligned {
        int internal1;
        long long internal2;
} __attribute__ ((aligned(64)));

int foo ()
{
  struct test_aligned var;
  return var.internal1;
}

/* { dg-final { scan-assembler-times "stp\tq0, q0," 2 } } */
