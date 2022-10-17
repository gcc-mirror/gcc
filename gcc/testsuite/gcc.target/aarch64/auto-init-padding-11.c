/* Verify zero initialization for union type with structure field with
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

union test_union_padding {
  struct test_trailing_hole u1;
  long long u2;
};


int foo ()
{
  union test_union_padding var;
  return var.u1.four;
}

/* { dg-final { scan-assembler "stp\txzr, xzr," } } */

