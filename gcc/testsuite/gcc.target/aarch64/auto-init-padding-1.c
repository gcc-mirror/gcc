/* Verify zero initialization for structure type automatic variables with
   padding.  */
/* { dg-do compile } */
/* { dg-options "-O -ftrivial-auto-var-init=zero" } */

struct test_aligned {
        int internal1;
        long long internal2;
} __attribute__ ((aligned(64)));

void bar (struct test_aligned *);

void foo ()
{
  struct test_aligned var;
  bar(&var);
}

/* { dg-final { scan-assembler-times {stp\tq[0-9]+, q[0-9]+,} 2 } } */
