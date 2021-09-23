/* Verify zero initialization for array type with structure element with
   padding.  */ 
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero" } */

struct test_trailing_hole {
        int one;
        int two;
        int three;
        char four;
        /* "sizeof(unsigned long) - 1" byte padding hole here. */
};


int foo ()
{
  struct test_trailing_hole var[10]; 
  return var[2].four;
}

/* { dg-final { scan-assembler-times "stp\tq0, q0," 5 } } */
