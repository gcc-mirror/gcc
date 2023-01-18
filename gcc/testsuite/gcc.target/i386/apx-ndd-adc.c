/* { dg-do compile { target { int128 && { ! ia32 } } } } */
/* { dg-options "-mapxf -O2" } */

#include "pr91681-1.c"
// *addti3_doubleword
// *addti3_doubleword_zext
// *adddi3_cc_overflow_1
// *adddi3_carry

int foo3 (int *a, int b) 
{				 	  
  int c = *a + b + (a > b); /* { dg-warning "comparison between pointer and integer" } */
  return c;			 	  
}			
/* { dg-final { scan-assembler-not "xor" } } */
