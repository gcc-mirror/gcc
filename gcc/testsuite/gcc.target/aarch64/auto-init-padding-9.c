/* Verify zero initialization for array type with structure element with
   padding.  */ 
/* { dg-do compile } */
/* { dg-options "-O -ftrivial-auto-var-init=zero" } */

struct test_trailing_hole {
        int one;
        int two;
        int three;
        char four;
        /* "sizeof(unsigned long) - 1" byte padding hole here. */
};

void bar (void *);

void foo ()
{
  struct test_trailing_hole var[10]; 
  bar (var);
}

/* { dg-final { scan-assembler-times {stp\tq[0-9]+, q[0-9]+,} 5 } } */
