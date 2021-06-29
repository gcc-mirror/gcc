/* Test BTF generation for pointers to void.

   In this test, we expect that the pointer references type ID 0, the reserved
   void typeid, and that no intermediate type is generated for void. */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x2000000\[\t \]+\[^\n\]*btt_info" 1 } } */

/* { dg-final { scan-assembler-times "\[\t \]0x1000000\[\t \]+\[^\n\]*btt_info" 0 } } */

void *ptr;
