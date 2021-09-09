/* Verify pattern initialization for structure type automatic variables with
   padding and has explicit initialization.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern" } */

struct test_trailing_hole {
        int one;
        int two;
        int three;
        char four;
        /* "sizeof(unsigned long) - 1" byte padding hole here. */
};

int foo ()
{
  struct test_trailing_hole var = {.one = 1,.two = 2, .four = 'c'};
  return var.four;
}

/* { dg-final { scan-assembler "stp\txzr, xzr," } } */


