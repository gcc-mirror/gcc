/* Verify zero initialization for structure type automatic variables with
   tail padding.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero" } */

struct test_trailing_hole {
        char *one;
        char *two;
        char *three;
        char four;
        /* "sizeof(unsigned long) - 1" byte padding hole here. */
};

int foo ()
{
  struct test_trailing_hole var;
  return var.four;
}

/* { dg-final { scan-assembler-times "stp\txzr, xzr," 2 } } */


