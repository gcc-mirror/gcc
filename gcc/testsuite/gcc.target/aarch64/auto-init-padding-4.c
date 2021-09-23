/* Verify pattern initialization for nested structure type automatic variables with
   padding.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern" } */

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


int foo ()
{
  struct test_big_hole var;
  return var.four.internal1;
}

/* { dg-final { scan-assembler-times "stp\tq0, q0," 5 } } */

