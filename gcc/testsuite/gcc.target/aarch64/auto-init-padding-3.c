/* Verify zero initialization for nested structure type automatic variables with
   padding.  */
/* { dg-do compile } */
/* { dg-options "-O -ftrivial-auto-var-init=zero" } */

struct test_aligned {
        unsigned internal1;
        unsigned long long internal2;
} __attribute__ ((aligned(64)));

struct test_big_hole {
        char one;
        char two;
        char three;
        /* 61 byte padding hole here. */
        struct test_aligned four;
} __attribute__ ((aligned(64)));

void bar (struct test_big_hole *);

void foo ()
{
  struct test_big_hole var;
  bar (&var);
}

/* { dg-final { scan-assembler-times {stp\tq[0-9]+, q[0-9]+,} 4 } } */
