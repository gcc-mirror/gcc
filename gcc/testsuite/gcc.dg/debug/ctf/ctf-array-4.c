/* CTF generation for array type.

   Test CTF generation for single element arrays.  In this testcase, one CTF
   record for array is expected with cta_nelems = 1.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "0x12000000\[\t \]+\[^\n\]*ctt_info" 1 } } */

/* { dg-final { scan-assembler-times "\[\t \]0x1\[\t \]+\[^\n\]*cta_nelems" 1 } } */

int b[1];
