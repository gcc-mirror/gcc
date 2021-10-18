/* Verify zero initialization for array type with structure element with
   padding.  */ 
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -march=x86-64" } */

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

/* { dg-final { scan-assembler "movl\t\\\$0," } } */
/* { dg-final { scan-assembler "movl\t\\\$20," { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "rep stosq" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\t\\\$40," { target ia32} } } */
/* { dg-final { scan-assembler "rep stosl" { target ia32 } } } */
